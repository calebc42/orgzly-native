;;; orgzly-rich.el --- Org → rich SDUI content with in-place editing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Adapted from Glasspane's org-rich emitter (glasspane-org-rich.el):
;; org content becomes Jetpacs `rich_text' span runs, native `table'
;; grids, and highlighted `jetpacs-markup' fallbacks — Emacs does the
;; parsing via `org-element'; the device only paints.  Deliberately an
;; app module, not core: how org should look is an opinion.
;;
;; Where Glasspane pairs this with a view/edit mode toggle, Orgzly's
;; note content has no modes (the Notion/Obsidian shape): the rendered
;; blocks are the only surface, and every block edits in place —
;;   - tap a paragraph/list/quote/code block → that block alone becomes
;;     an org editor (`orgzly.note.edit-block' / `save-block' live in
;;     orgzly-ui.el, which owns the view state);
;;   - checkboxes toggle where they are;
;;   - table cells tap-edit (a #+TBLFM-computed field opens its formula
;;     instead — a typed value would be recalculated away), long-press
;;     for row/column ops, "+" affordances append; formulas recalculate
;;     Emacs-side, the phone never computes;
;;   - a src block header carries a play button only when this Emacs
;;     can actually execute the language; the wire names a location,
;;     never code, `org-confirm-babel-evaluate' bridges to a native
;;     dialog, and a timeout stops a runaway block wedging the bridge;
;;   - #+RESULTS content renders read-only in a foldable section —
;;     execution regenerates it, hand edits would be silently lost.

;;; Code:

(require 'org)
(require 'org-element)
(require 'org-table)
(require 'cl-lib)
(require 'jetpacs-widgets)
(require 'jetpacs-shell)
(require 'orgzly-data)

;; Owned by orgzly-ui (the view state); used by the link action's
;; navigation.
(defvar orgzly-ui--note-ref)
(defvar orgzly-ui--current-book)

;; ─── Dynamic context ─────────────────────────────────────────────────────────

(defvar orgzly-rich--file nil
  "File being rendered; enables the interactive affordances.")

(defvar orgzly-rich--body-offset nil
  "Offset mapping temp-buffer positions to real-file positions.
real-pos = offset + temp-pos.  Set by `orgzly-rich-body' when FILE and
OFFSET are supplied.")

(defvar orgzly-rich--read-only nil
  "Non-nil while rendering babel #+RESULTS content.
Suppresses every edit affordance — execution regenerates results, so a
hand edit would be silently overwritten by the next run.")

(defvar orgzly-rich--edit-region nil
  "Real-file (BEG . END) of the block being edited in place, or nil.
The top-level element starting at BEG renders as an org editor seeded
with its raw text instead of its rich form.")

;; ─── Inline spans ────────────────────────────────────────────────────────────

(defun orgzly-rich--flag (style key)
  "Return STYLE (a plist of emphasis flags) with KEY turned on."
  (cons key (cons t style)))

(defun orgzly-rich--leaf (text style)
  "Build a span for TEXT carrying the emphasis flags set in STYLE."
  (apply #'jetpacs-span (or text "")
         (append (when (plist-get style :bold)      '(:bold t))
                 (when (plist-get style :italic)    '(:italic t))
                 (when (plist-get style :underline) '(:underline t))
                 (when (plist-get style :strike)    '(:strike t))
                 (when (plist-get style :code)      '(:code t))
                 (when (plist-get style :baseline)
                   (list :baseline (plist-get style :baseline))))))

(defconst orgzly-rich--image-re
  "\\.\\(png\\|jpe?g\\|gif\\|webp\\|bmp\\|svg\\)\\'"
  "Matches link targets that should render as inline images.")

(defun orgzly-rich--image-url (type target)
  "A renderable URL for a link of TYPE to TARGET when it's an image.
http(s) image URLs pass through; local file paths become file:// URIs
the companion can try to load.  Returns nil for non-images."
  (when (and (stringp target)
             (string-match-p orgzly-rich--image-re (downcase target)))
    (let ((ty (and type (downcase type))))
      (cond
       ((member ty '("http" "https")) (concat ty ":" target))
       ((or (null ty) (equal ty "file"))
        (concat "file://" (expand-file-name target)))))))

(defun orgzly-rich--linkify (spans action)
  "Attach ON-TAP ACTION to every span in SPANS lacking one."
  (mapcar (lambda (sp)
            (if (assq 'on_tap sp) sp (cons (cons 'on_tap action) sp)))
          spans))

(defun orgzly-rich--inline (objects style)
  "Convert org inline OBJECTS (strings and elements) to spans.
STYLE carries inherited emphasis flags as recursion descends.
Whitespace after an object lives in its `:post-blank' — absent from
both the object and the next sibling — so it is re-emitted, or words
jam together after emphasis, links, and timestamps."
  (let (spans)
    (dolist (obj objects)
      (cond
       ((stringp obj)
        (push (orgzly-rich--leaf obj style) spans))
       ((null obj) nil)
       (t
        (pcase (org-element-type obj)
          ('bold
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (orgzly-rich--flag style :bold)))
                               spans)))
          ('italic
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (orgzly-rich--flag style :italic)))
                               spans)))
          ('underline
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (orgzly-rich--flag style :underline)))
                               spans)))
          ('strike-through
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (orgzly-rich--flag style :strike)))
                               spans)))
          ((or 'code 'verbatim)
           (push (orgzly-rich--leaf (org-element-property :value obj)
                                    (orgzly-rich--flag style :code))
                 spans))
          ('link
           (let* ((raw (org-element-property :raw-link obj))
                  (contents (org-element-contents obj))
                  (child (if contents
                             (orgzly-rich--inline contents style)
                           (list (orgzly-rich--leaf (or raw "link") style))))
                  (action (jetpacs-action "orgzly.link.open"
                                       :args (list (cons 'link raw)))))
             (setq spans (append (nreverse (orgzly-rich--linkify child action))
                                 spans))))
          ('timestamp
           (push (orgzly-rich--leaf (org-element-property :raw-value obj)
                                    (orgzly-rich--flag style :code))
                 spans))
          ('entity
           (when-let ((utf8 (or (org-element-property :utf-8 obj)
                                (org-element-property :name obj))))
             (push (orgzly-rich--leaf utf8 style) spans)))
          ('subscript
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (cons :baseline
                                                (cons "sub" style))))
                               spans)))
          ('superscript
           (setq spans (append (nreverse (orgzly-rich--inline
                                          (org-element-contents obj)
                                          (cons :baseline
                                                (cons "super" style))))
                               spans)))
          ('footnote-reference
           (let* ((label (org-element-property :label obj))
                  (marker (format "[%s]"
                                  (if (and (stringp label)
                                           (string-prefix-p "fn:" label))
                                      (substring label 3)
                                    (or label "*"))))
                  (def (string-trim
                        (or (ignore-errors
                              (org-element-interpret-data
                               (org-element-contents obj)))
                            ""))))
             (push (jetpacs-span marker
                              :baseline "super" :tag t
                              :on-tap (jetpacs-action
                                       "orgzly.footnote.show"
                                       :args (list (cons 'label (or label ""))
                                                   (cons 'def def))))
                   spans)))
          ('line-break (push (jetpacs-span "\n") spans))
          (_
           (let ((txt (ignore-errors (org-element-interpret-data obj))))
             (when (stringp txt)
               (push (orgzly-rich--leaf (string-trim-right txt) style)
                     spans)))))
        (let ((pb (or (org-element-property :post-blank obj) 0)))
          (when (> pb 0)
            (push (orgzly-rich--leaf (make-string pb ?\s) style) spans))))))
    (nreverse spans)))

;; ─── Plain lists (interactive checkboxes) ────────────────────────────────────

(defun orgzly-rich--item (item)
  "Render a plain-list ITEM: bullet/checkbox + content + sub-elements.
With file context, checkbox items get a tappable icon toggling them via
Emacs — no edit mode involved."
  (let* ((bullet (or (org-element-property :bullet item) "- "))
         (checkbox (org-element-property :checkbox item))
         (contents (org-element-contents item))
         (para (cl-find-if (lambda (c) (eq (org-element-type c) 'paragraph))
                           contents))
         (inline (when para
                   (orgzly-rich--inline (org-element-contents para) nil)))
         (lead-text (concat (string-trim-right bullet) " "))
         (head
          (if (and checkbox orgzly-rich--file orgzly-rich--body-offset
                   (not orgzly-rich--read-only))
              (let* ((item-pos (+ orgzly-rich--body-offset
                                  (org-element-property :begin item)))
                     (cb-icon (pcase checkbox
                                ('on  "check_box")
                                ('off "check_box_outline_blank")
                                (_    "indeterminate_check_box"))))
                (jetpacs-row
                 (jetpacs-box (list (jetpacs-icon cb-icon :size 20))
                           :on-tap (jetpacs-action
                                    "orgzly.checkbox.toggle"
                                    :args `((file . ,orgzly-rich--file)
                                            (pos . ,item-pos))))
                 (jetpacs-box
                  (list (jetpacs-rich-text
                         (cons (jetpacs-span lead-text)
                               (or inline (list (jetpacs-span ""))))))
                  :weight 1)))
            (let ((mark (pcase checkbox
                          ('on "☑ ") ('off "☐ ") ('trans "◪ ") (_ ""))))
              (jetpacs-rich-text (cons (jetpacs-span (concat lead-text mark))
                                    (or inline (list (jetpacs-span ""))))))))
         (rest (delq para (copy-sequence contents)))
         (sub-nodes (delq nil (mapcar #'orgzly-rich--element rest))))
    (if sub-nodes
        (jetpacs-column head
                     (jetpacs-row (jetpacs-spacer :width 16)
                               (jetpacs-box (list (apply #'jetpacs-column
                                                      sub-nodes))
                                         :weight 1)))
      head)))

(defun orgzly-rich--list (el)
  "Render plain-list EL to a column of item nodes."
  (let ((items (delq nil
                     (mapcar (lambda (item)
                               (when (eq (org-element-type item) 'item)
                                 (orgzly-rich--item item)))
                             (org-element-contents el)))))
    (when items (apply #'jetpacs-column items))))

;; ─── Source blocks ───────────────────────────────────────────────────────────

(defun orgzly-rich--src-block (el)
  "Render src-block EL: highlighted code plus a run header when executable.
The play button appears only with file context *and* an
`org-babel-execute:LANG' in this Emacs — the same test execution makes,
so it never promises more than `org-babel-load-languages' delivers.
The action carries the block's location; the code never crosses the
wire."
  (let* ((lang (org-element-property :language el))
         (code (jetpacs-markup (or (org-element-property :value el) "")
                            :syntax (or lang "text")))
         (pos (and orgzly-rich--file orgzly-rich--body-offset
                   (not orgzly-rich--read-only)
                   lang
                   (fboundp (intern (concat "org-babel-execute:" lang)))
                   (+ orgzly-rich--body-offset
                      (org-element-property :post-affiliated el)))))
    (if (not pos)
        code
      (jetpacs-column
       (jetpacs-row
        (jetpacs-text lang 'label)
        (jetpacs-spacer :weight 1)
        (jetpacs-icon-button "play_arrow"
                          (jetpacs-action "orgzly.babel.execute"
                                       :args `((file . ,orgzly-rich--file)
                                               (pos . ,pos)))
                          :content-description "Run block"))
       code))))

;; ─── Tables ──────────────────────────────────────────────────────────────────

(defconst orgzly-rich--cookie-re "\\`<[lcr]?[0-9]*>\\'"
  "Matches alignment/width cookie cells: <l>, <r>, <c>, <10>, <r20>, …")

(defun orgzly-rich--cell-text (cell)
  "Trimmed plain text of table CELL (nil-safe)."
  (if (null cell) ""
    (string-trim
     (or (ignore-errors
           (org-element-interpret-data (org-element-contents cell)))
         ""))))

(defun orgzly-rich--cookie-row-p (cells)
  "Non-nil when CELLS form a cookie row (alignment config, not data)."
  (let ((texts (mapcar #'orgzly-rich--cell-text cells)))
    (and (cl-some (lambda (s) (not (string-empty-p s))) texts)
         (cl-every (lambda (s)
                     (or (string-empty-p s)
                         (string-match-p orgzly-rich--cookie-re s)))
                   texts))))

(defun orgzly-rich--table-aligns (cookie-rows data-rows ncols)
  "Alignment strings (start/center/end) for NCOLS columns.
A cookie wins; otherwise a mostly-numeric column right-aligns,
mirroring org's own aligner.  nil when every column is \"start\"."
  (let (aligns)
    (dotimes (c ncols)
      (let ((cookie
             (cl-loop for row in cookie-rows
                      for text = (orgzly-rich--cell-text (nth c row))
                      when (string-match "\\`<\\([lcr]\\)" text)
                      return (match-string 1 text))))
        (push
         (pcase cookie
           ("l" "start") ("c" "center") ("r" "end")
           (_ (let ((total 0) (numbers 0))
                (dolist (row data-rows)
                  (let ((text (orgzly-rich--cell-text (nth c row))))
                    (unless (string-empty-p text)
                      (cl-incf total)
                      (when (string-match-p org-table-number-regexp text)
                        (cl-incf numbers)))))
                (if (and (> total 0)
                         (>= (/ (float numbers) total)
                             org-table-number-fraction))
                    "end" "start"))))
         aligns)))
    (setq aligns (nreverse aligns))
    (and (cl-some (lambda (a) (not (equal a "start"))) aligns)
         aligns)))

(defun orgzly-rich--table-cell (cell)
  "Build a cell node for table CELL: tap edits, long-press row/column ops."
  (let* ((spans (or (orgzly-rich--inline (org-element-contents cell) nil)
                    (list (jetpacs-span ""))))
         (pos (and orgzly-rich--file orgzly-rich--body-offset
                   (not orgzly-rich--read-only)
                   (+ orgzly-rich--body-offset
                      (or (org-element-property :contents-begin cell)
                          (org-element-property :begin cell)))))
         (args (when pos
                 `((file . ,orgzly-rich--file) (pos . ,pos)))))
    (jetpacs-table-cell
     spans
     :on-tap (when pos (jetpacs-action "orgzly.table.edit" :args args))
     :on-long-tap (when pos
                    (jetpacs-action "orgzly.table.cell-menu" :args args)))))

(defun orgzly-rich--table (el)
  "Render org table EL to a native `jetpacs-table' node, or nil when empty.
Cookie rows configure alignment and drop from display; header rows are
the first row group when a rule separates it from more groups."
  (let* ((file orgzly-rich--file)
         (offset orgzly-rich--body-offset)
         (cookie-rows nil)
         (shapes
          (delq nil
                (mapcar
                 (lambda (row)
                   (when (eq (org-element-type row) 'table-row)
                     (if (eq (org-element-property :type row) 'rule)
                         'rule
                       (let ((cells (org-element-contents row)))
                         (if (orgzly-rich--cookie-row-p cells)
                             (progn (push cells cookie-rows) nil)
                           cells)))))
                 (org-element-contents el))))
         (data-rows (cl-remove 'rule shapes))
         (ncols (cl-loop for s in data-rows maximize (length s))))
    (when (and ncols (> ncols 0))
      (let ((groups 0) (prev-rule t) header-rows)
        (dolist (shape shapes)
          (if (eq shape 'rule)
              (setq prev-rule t)
            (when prev-rule (cl-incf groups))
            (setq prev-rule nil)
            (when (= groups 1) (push shape header-rows))))
        (unless (> groups 1) (setq header-rows nil))
        (let ((table-pos (and file offset (not orgzly-rich--read-only)
                              (+ offset
                                 (org-element-property :post-affiliated el)))))
          (jetpacs-table
           (mapcar (lambda (shape)
                     (if (eq shape 'rule)
                         (jetpacs-table-rule)
                       (jetpacs-table-row
                        (mapcar #'orgzly-rich--table-cell shape)
                        :header (and (memq shape header-rows) t))))
                   shapes)
           :aligns (orgzly-rich--table-aligns
                    (nreverse cookie-rows) data-rows ncols)
           :on-add-row (when table-pos
                         (jetpacs-action "orgzly.table.add-row"
                                      :args `((file . ,file)
                                              (pos . ,table-pos))))
           :on-add-col (when table-pos
                         (jetpacs-action "orgzly.table.add-col"
                                      :args `((file . ,file)
                                              (pos . ,table-pos))))))))))

;; ─── Block elements ──────────────────────────────────────────────────────────

(defun orgzly-rich--drawer (el)
  "Render drawer EL as a folded section, like desktop org."
  (let ((name (or (org-element-property :drawer-name el) "DRAWER"))
        (inner (delq nil (mapcar #'orgzly-rich--element
                                 (org-element-contents el)))))
    (when inner
      (jetpacs-collapsible
       (format "orgzly-drawer:%s:%s"
               (or orgzly-rich--file "")
               (+ (or orgzly-rich--body-offset 0)
                  (org-element-property :begin el)))
       (jetpacs-text name 'label)
       inner
       :collapsed t))))

(defun orgzly-rich--paragraph-image (el)
  "If paragraph EL is a single image link, return a `jetpacs-image' node."
  (let* ((contents (org-element-contents el))
         (non-blank (cl-remove-if (lambda (c)
                                    (and (stringp c) (string-blank-p c)))
                                  contents)))
    (when (and (= (length non-blank) 1)
               (consp (car non-blank))
               (eq (org-element-type (car non-blank)) 'link))
      (let* ((lnk (car non-blank))
             (url (orgzly-rich--image-url
                   (org-element-property :type lnk)
                   (org-element-property :path lnk))))
        (when url (jetpacs-image url))))))

(defun orgzly-rich--element (el)
  "Render one org element EL to a node, or nil to skip it.
Babel output — any element under a #+RESULTS: keyword — renders
read-only inside a foldable RESULTS section, like desktop org."
  (if (org-element-property :results el)
      (let* ((orgzly-rich--read-only t)
             (node (orgzly-rich--element-1 el)))
        (when node
          ;; `:results drawer' output is already a foldable drawer —
          ;; don't nest a second collapsible around it.
          (if (equal (alist-get 't node) "collapsible")
              node
            (jetpacs-collapsible
             (format "orgzly-results:%s:%s"
                     (or orgzly-rich--file "")
                     (+ (or orgzly-rich--body-offset 0)
                        (org-element-property :begin el)))
             (jetpacs-text "RESULTS" 'label)
             (list node)))))
    (orgzly-rich--element-1 el)))

(defun orgzly-rich--element-1 (el)
  "Render element EL to a node ignoring any #+RESULTS: wrapping."
  (pcase (org-element-type el)
    ('paragraph
     (or (orgzly-rich--paragraph-image el)
         (let ((spans (orgzly-rich--inline (org-element-contents el) nil)))
           (when spans (jetpacs-rich-text spans)))))
    ('plain-list (orgzly-rich--list el))
    ('src-block (orgzly-rich--src-block el))
    ((or 'example-block 'fixed-width)
     (jetpacs-markup (or (org-element-property :value el) "")))
    ('quote-block
     (let ((inner (delq nil (mapcar #'orgzly-rich--element
                                    (org-element-contents el)))))
       (when inner (apply #'jetpacs-column inner))))
    ('table
     ;; table.el tables keep the monospace fallback; org tables go native.
     (if (eq (org-element-property :type el) 'table.el)
         (jetpacs-markup (string-trim (org-element-interpret-data el))
                      :syntax "org")
       (or (orgzly-rich--table el)
           (jetpacs-markup (string-trim (org-element-interpret-data el))
                        :syntax "org"))))
    ('horizontal-rule (jetpacs-divider))
    ('drawer (orgzly-rich--drawer el))
    ((or 'keyword 'comment 'comment-block 'planning
         'property-drawer 'node-property)
     nil)
    (_
     (let ((txt (ignore-errors (string-trim (org-element-interpret-data el)))))
       (when (and (stringp txt) (not (string-empty-p txt)))
         (jetpacs-markup txt :syntax "org"))))))

;; ─── In-place block editing (the Notion shape) ───────────────────────────────

(defconst orgzly-rich--editable-types
  '(paragraph plain-list quote-block example-block fixed-width
    src-block verse-block)
  "Top-level element types a tap turns into an in-place editor.
Tables and checkboxes have finer-grained interactions of their own;
drawers fold; keywords render nothing.")

(defun orgzly-rich--block-editor (beg end)
  "The in-place editor for the block spanning real [BEG, END).
Runs inside `orgzly-rich-body''s temp buffer, so the raw text comes
from it (temp-pos = real-pos - offset)."
  (let* ((offset orgzly-rich--body-offset)
         (raw (string-trim-right
               (buffer-substring-no-properties
                (max (point-min) (- beg offset))
                (min (point-max) (- end offset))))))
    (jetpacs-column
     (jetpacs-editor (format "orgzly-block:%s:%s" orgzly-rich--file beg)
                  raw
                  :on-save (jetpacs-action "orgzly.note.save-block"
                                        :args `((file . ,orgzly-rich--file)
                                                (beg . ,beg) (end . ,end)))
                  :syntax "org" :toolbar "org" :chromeless t)
     (jetpacs-row
      (jetpacs-spacer :weight 1.0)
      (jetpacs-button "Cancel" (jetpacs-action "orgzly.note.cancel-block")
                   :variant "text")))))

(defun orgzly-rich--top-node (el)
  "Render top-level element EL, edit-in-place aware.
The element under `orgzly-rich--edit-region' becomes an editor; other
editable blocks get a tap opening theirs."
  (let ((beg (and orgzly-rich--body-offset
                  (+ orgzly-rich--body-offset
                     (org-element-property :begin el)))))
    (if (and beg orgzly-rich--edit-region
             (= beg (car orgzly-rich--edit-region)))
        (orgzly-rich--block-editor (car orgzly-rich--edit-region)
                                   (cdr orgzly-rich--edit-region))
      (let ((node (orgzly-rich--element el)))
        (if (and node beg orgzly-rich--file
                 (not (org-element-property :results el))
                 (memq (org-element-type el) orgzly-rich--editable-types))
            (jetpacs-box
             (list node)
             :on-tap (jetpacs-action
                      "orgzly.note.edit-block"
                      :args `((file . ,orgzly-rich--file)
                              (beg . ,beg)
                              (end . ,(+ orgzly-rich--body-offset
                                         (org-element-property :end el))))
                      :when-offline "drop"))
          node)))))

;; ─── Entry point ─────────────────────────────────────────────────────────────

(defun orgzly-rich--top-elements (tree)
  "Top-level elements of parsed TREE, descending through a section."
  (let (out)
    (dolist (el (org-element-contents tree))
      (if (eq (org-element-type el) 'section)
          (setq out (append out (org-element-contents el)))
        (setq out (append out (list el)))))
    out))

(defun orgzly-rich-body (body &optional base-dir file offset)
  "Parse org BODY string into a list of Jetpacs nodes.
BASE-DIR resolves relative image paths.  FILE and OFFSET enable the
interactive affordances: OFFSET maps temp-buffer positions to real
file positions (real = offset + temp).  Returns nil for empty input."
  (if (or (null body) (string-empty-p (string-trim body)))
      nil
    (let ((orgzly-rich--file file)
          (orgzly-rich--body-offset offset))
      (with-temp-buffer
        (insert body)
        (when (and base-dir (file-directory-p base-dir))
          (setq default-directory base-dir))
        (let ((org-inhibit-startup t)
              (org-element-use-cache nil))
          (delay-mode-hooks (org-mode))
          (let ((tree (org-element-parse-buffer)))
            (delq nil (mapcar #'orgzly-rich--top-node
                              (orgzly-rich--top-elements tree)))))))))

;; ─── Actions: links, footnotes, checkboxes ───────────────────────────────────

(jetpacs-defaction "orgzly.link.open"
  ;; A tapped link in rich content.  Emacs resolves it via the org link
  ;; machinery; when it lands on a heading in a notebook, navigate to
  ;; that note — otherwise report the outcome as a snackbar.
  (lambda (args _)
    (let ((link (alist-get 'link args)))
      (when (and (stringp link) (not (string-empty-p link)))
        (let (target)
          (condition-case err
              (progn
                (save-window-excursion
                  (org-link-open-from-string link)
                  (when (and (derived-mode-p 'org-mode) (buffer-file-name))
                    (setq target
                          (list (buffer-file-name)
                                (save-excursion
                                  (and (ignore-errors (org-back-to-heading t))
                                       (point)))
                                (nth 4 (ignore-errors
                                         (org-heading-components)))))))
                (if (and target (nth 1 target)
                         (string-prefix-p
                          (expand-file-name orgzly-directory)
                          (expand-file-name (car target))))
                    (progn
                      (setq orgzly-ui--note-ref
                            `((file . ,(car target)) (pos . ,(nth 1 target))
                              (title . ,(or (nth 2 target) "")))
                            orgzly-ui--current-book
                            (orgzly-data-book-name (car target)))
                      (jetpacs-shell-push nil :switch-to "orgzly.note"))
                  (jetpacs-shell-notify (format "Opened %s" link))
                  (jetpacs-shell-push)))
            (error
             (jetpacs-shell-notify (format "Couldn't open %s: %s" link
                                        (error-message-string err)))
             (jetpacs-shell-push))))))))

(jetpacs-defaction "orgzly.footnote.show"
  (lambda (args _)
    (let ((def (alist-get 'def args))
          (label (alist-get 'label args)))
      (jetpacs-shell-notify
       (if (and (stringp def) (not (string-empty-p def)))
           (format "Footnote: %s" def)
         (format "Footnote %s" (or label ""))))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.checkbox.toggle"
  ;; Tap a checkbox in rendered content: toggle in place, statistics
  ;; cookies update through org's own machinery.
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (progn
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (goto-char pos)
                 (org-toggle-checkbox))
                (let ((save-silently t)) (save-buffer)))
              (orgzly-data-invalidate)
              (jetpacs-shell-push))
          (error
           (jetpacs-shell-notify (format "Toggle failed: %s"
                                      (error-message-string err)))))))))

;; ─── Actions: tables ─────────────────────────────────────────────────────────

(defun orgzly-rich--table-mutate (file pos fn)
  "Run FN with point at POS inside FILE's table, then align, save, repush.
Afterwards the table is realigned, recalculated when a #+TBLFM line
follows (formulas live Emacs-side — the phone never computes), saved,
and every view repushed.  A mutation that moves point off the table
realigns from the table's start; one that consumes the table entirely
skips the realign instead of erroring."
  (with-current-buffer (find-file-noselect file)
    (org-with-wide-buffer
     (goto-char pos)
     (unless (org-at-table-p) (error "No table at position %s" pos))
     (let ((table-beg (org-table-begin)))
       (funcall fn)
       (unless (org-at-table-p)
         (goto-char (min table-beg (point-max))))
       (when (org-at-table-p)
         (org-table-align)
         (when (save-excursion
                 (goto-char (org-table-end))
                 (let ((case-fold-search t))
                   (looking-at-p "[ \t]*#\\+TBLFM:")))
           (org-table-recalculate t)))))
    (let ((save-silently t)) (save-buffer)))
  (orgzly-data-invalidate)
  (jetpacs-shell-push))

(defun orgzly-rich--table-field-formula ()
  "The #+TBLFM entry (LHS . RHS) computing the field at point, or nil.
Field formulas (@R$C) win over column formulas ($C), mirroring org's
own recalculation.  Point must be inside a table."
  (org-table-analyze)
  (let* ((line (count-lines org-table-current-begin-pos
                            (line-beginning-position)))
         (dline (org-table-line-to-dline line))
         (col (org-table-current-column))
         (stored (org-table-get-stored-formulas t))
         (norm (lambda (kv)
                 (or (ignore-errors
                       (org-table-formula-handle-first/last-rc (car kv)))
                     (car kv)))))
    (when (and dline col (> col 0))
      (or (cl-find (format "@%d$%d" dline col) stored :key norm :test #'equal)
          (cl-find (format "$%d" col) stored :key norm :test #'equal)))))

(jetpacs-defaction "orgzly.table.edit"
  ;; Tap a cell: a bridged prompt prefilled with the field, written back
  ;; through the org table machinery so formulas recalculate.  A field
  ;; #+TBLFM computes opens its formula instead — recalculation would
  ;; immediately overwrite a typed value.
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (let (current formula)
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (goto-char pos)
                 (unless (org-at-table-p) (error "No table cell here"))
                 (setq formula (orgzly-rich--table-field-formula))
                 (unless formula
                   (setq current (string-trim (org-table-get-field))))))
              (if formula
                  (let ((input (string-trim
                                (read-string
                                 (format "Formula %s= " (car formula))
                                 (cdr formula)))))
                    (when (string-empty-p input)
                      (error "Empty formula — edit the #+TBLFM line instead"))
                    (orgzly-rich--table-mutate file pos
                      (lambda ()
                        (let* ((stored (org-table-get-stored-formulas t))
                               (entry (or (assoc (car formula) stored)
                                          (error "Formula %s not found"
                                                 (car formula)))))
                          (setcdr entry input)
                          (save-excursion
                            (org-table-store-formulas stored))))))
                (let* ((input (read-string "Cell: " current))
                       ;; A field is one line between pipes — keep it so.
                       (new (string-replace
                             "|" "\\vert{}"
                             (string-replace "\n" " " input))))
                  (orgzly-rich--table-mutate file pos
                    (lambda () (org-table-get-field nil new))))))
          (error
           (jetpacs-shell-notify (format "Edit failed: %s"
                                      (error-message-string err)))
           (jetpacs-shell-push)))))))

(jetpacs-defaction "orgzly.table.cell-menu"
  ;; Long-press a cell: row/column structure edits via a bridged
  ;; completing-read.  Org's own commands fix up #+TBLFM references.
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (let ((choice (completing-read
                           "Row/column: "
                           '("Insert row above" "Insert column left"
                             "Delete row" "Delete column")
                           nil t)))
              (orgzly-rich--table-mutate file pos
                (lambda ()
                  (pcase choice
                    ("Insert row above"   (org-table-insert-row))
                    ("Insert column left" (org-table-insert-column))
                    ("Delete row"         (org-table-kill-row))
                    ("Delete column"      (org-table-delete-column))))))
          (error
           (jetpacs-shell-notify (format "Table edit failed: %s"
                                      (error-message-string err)))
           (jetpacs-shell-push)))))))

(jetpacs-defaction "orgzly.table.add-row"
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (orgzly-rich--table-mutate file pos
              (lambda ()
                (goto-char (org-table-end))
                (forward-line -1)
                (org-table-insert-row t)))
          (error
           (jetpacs-shell-notify (format "Add row failed: %s"
                                      (error-message-string err)))
           (jetpacs-shell-push)))))))

(jetpacs-defaction "orgzly.table.add-col"
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (orgzly-rich--table-mutate file pos
              (lambda ()
                ;; Force-create a field one past the last column of the
                ;; first data line; the realign squares the rest off.
                ;; `org-table-insert-column' inserts LEFT of point, so
                ;; it can't append at the right edge directly.
                (goto-char (org-table-begin))
                (while (and (org-at-table-hline-p)
                            (< (point) (org-table-end)))
                  (forward-line 1))
                (let ((ncols (1- (cl-count
                                  ?| (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))))
                  (org-table-goto-column (1+ (max 1 ncols)) nil 'force))))
          (error
           (jetpacs-shell-notify (format "Add column failed: %s"
                                      (error-message-string err)))
           (jetpacs-shell-push)))))))

;; ─── Actions: babel ──────────────────────────────────────────────────────────

(defcustom orgzly-babel-timeout 30
  "Seconds before a phone-triggered babel execution is abandoned.
Best-effort: the timer can't interrupt a synchronous subprocess
mid-call, but it fires between process reads and stops a runaway block
from wedging the bridge forever."
  :type 'integer :group 'orgzly)

(jetpacs-defaction "orgzly.babel.execute"
  ;; The play button on a src-block header.  The wire names only a
  ;; location — the code that runs lives in the user's own file.
  ;; `org-confirm-babel-evaluate' is honored: the yes/no prompt bridges
  ;; to a native dialog and runs BEFORE the timeout starts, so a slow
  ;; answer never counts against the execution budget.
  (lambda (args _)
    (let ((file (alist-get 'file args))
          (pos (alist-get 'pos args)))
      (when (and file pos (file-readable-p file))
        (condition-case err
            (progn
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (goto-char pos)
                 (let ((info (org-babel-get-src-block-info)))
                   (unless info (error "No source block here"))
                   ;; `org-babel-confirm-evaluate' RETURNS nil on
                   ;; decline (it does not signal) — gate on that.
                   (unless (org-babel-confirm-evaluate info)
                     (user-error "Evaluation declined"))
                   (let ((org-confirm-babel-evaluate nil))
                     (with-timeout ((max 1 orgzly-babel-timeout)
                                    (error "Timed out after %ss"
                                           orgzly-babel-timeout))
                       (org-babel-execute-src-block nil info)))))
                (let ((save-silently t)) (save-buffer)))
              (orgzly-data-invalidate)
              (jetpacs-shell-notify "Block executed")
              (jetpacs-shell-push))
          (error
           (jetpacs-shell-notify (format "Run failed: %s"
                                      (error-message-string err)))
           (jetpacs-shell-push)))))))

(provide 'orgzly-rich)
;;; orgzly-rich.el ends here
