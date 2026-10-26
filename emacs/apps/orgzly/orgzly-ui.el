;;; orgzly-ui.el --- Orgzly screens built with Compose Foundation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Builders return the actual Jetpacs IR and read only the published snapshot.
;;; Code:
(require 'orgzly-state)
(require 'orgzly-design)
(require 'jetpacs-chrome)
(require 'jetpacs-org-links)       ; the system-link rule every applet shares

(defvar orgzly-field-scope nil
  "Dynamic identity namespace of the current form, shared by its capture handler.")

(declare-function orgzly--note-org "orgzly" (entry))

(defun orgzly-field-scope (route)
  "Return the stable form namespace for ROUTE and its revision-bound identity."
  (jetpacs-wire-id (symbol-name (plist-get route :kind))
                   (concat (or (plist-get route :id) (plist-get route :parent-id)
                               (plist-get route :sibling-id)
                               (plist-get route :book-id) "form")
                           (if (plist-get route :before) "-above" ""))))

(defun orgzly-field-id (id)
  "Qualify logical field ID with the current form's identity namespace."
  (if orgzly-field-scope (jetpacs-wire-id orgzly-field-scope id) id))

(defconst orgzly-ui-fields '("oz-title" "oz-tags" "oz-content" "oz-prop-name" "oz-prop-value")
  "Controlled note fields the single atomic save action may capture.
Title and content are captured only while edited in place; state,
priority and planning are picked into `orgzly-state-draft'.")

(defun orgzly-ui-action (name &optional args confirm fields)
  "Build NAME with opaque ARGS, optional CONFIRM and captured FIELDS."
  (jetpacs-action name :args args :when-offline "drop" :confirm confirm :capture-fields (mapcar #'orgzly-field-id fields)))

(defun orgzly-ui-icon (icon label action)
  "Return a labeled Foundation ICON button for ACTION with accessible LABEL."
  (jetpacs-icon-button icon action :content-description label))

(defun orgzly-ui-dark ()
  "Non-nil when the dark Orgzly palette is selected."
  orgzly-state-dark)

(defun orgzly-ui-styled (styles &rest children)
  "Return CHILDREN under the Orgzly design STYLES, a style name or list."
  (jetpacs-design-styled (if (listp styles) styles (list styles)) children))

(defun orgzly-ui-styled-text (styles text &rest options)
  "Return TEXT with OPTIONS in the Orgzly text STYLES."
  (orgzly-ui-styled styles (apply #'jetpacs-text text options)))

(defun orgzly-ui-navigation ()
  "Return the app's compact, horizontally scrollable destinations."
  (apply #'jetpacs-row
         (append
          (mapcar (lambda (pair)
                    (jetpacs-chip (car pair)
                                  :on-tap (orgzly-ui-action "orgzly.open" (list :view (cdr pair)))))
                  '(("Notebooks" . "books") ("Agenda" . "agenda")
                    ("Search" . "search") ("Settings" . "settings")))
          '(:scroll t :spacing 4))))

(defconst orgzly-ui--emphasis
  '((?* . (:font-weight "bold")) (?/ . (:italic t)) (?_ . (:underline t))
    (?= . (:mono t)) (?~ . (:mono t)) (?+ . nil))
  "Org emphasis markers and the span keywords each renders as.")

(defun orgzly-ui--emphasis-spans (text &rest base)
  "Split Org TEXT into spans, rendering emphasis markers; BASE styles every run.
A marker pair must open after a space or punctuation and enclose text that
neither starts nor ends with a space, as Org's own emphasis rule does."
  (let ((re "\\(?:^\\|[[:space:]('\"{]\\)\\(\\([*/_=~+]\\)\\([^[:space:]]\\(?:.*?[^[:space:]]\\)?\\)\\2\\)\\(?:$\\|[[:space:]-.,:!?;'\")}]\\)")
        (start 0) spans)
    (while (string-match re text start)
      (let ((open (match-beginning 1)) (close (match-end 1))
            (marker (string-to-char (match-string 2 text)))
            (inner (match-string 3 text)))
        (when (> open start)
          (push (apply #'jetpacs-span (substring text start open) base) spans))
        (push (apply #'jetpacs-span inner
                     (append (alist-get marker orgzly-ui--emphasis) base))
              spans)
        (setq start close)))
    (when (< start (length text))
      (push (apply #'jetpacs-span (substring text start) base) spans))
    (nreverse spans)))

(defconst orgzly-ui--system-schemes
  (mapconcat #'regexp-quote jetpacs-org-link-system-types "\\|")
  "Revived's SYSTEM_LINK_SCHEMES: links the device's own apps open.")

(defconst orgzly-ui--link-re
  (concat "\\[\\[\\(.+?\\)\\]\\(?:\\[\\(.+?\\)\\]\\)?\\]"
          "\\|\\(\\(?:" orgzly-ui--system-schemes "\\|id\\|file\\):[^[:space:]]+\\)")
  "An Org bracket link, or a bare link as Revived's OrgFormatter finds it:
a known scheme and everything up to the next whitespace.")

(defun orgzly-ui-system-link-p (target)
  "Non-nil when TARGET is a link the device's own apps open, as in Revived."
  (jetpacs-org-link-system-p target))

(defun orgzly-ui-link-segments (text)
  "Split Org TEXT into (STRING . TARGET) segments; TARGET is nil for prose.
A bracket link shows its description, or its target when it has none."
  (let ((start 0) (from 0) segments)
    (while (string-match orgzly-ui--link-re text from)
      (let ((target (or (match-string 1 text) (match-string 3 text)))
            (shown (or (match-string 2 text) (match-string 1 text) (match-string 3 text)))
            (begin (match-beginning 0)) (end (match-end 0)))
        ;; Revived's (?<![a-zA-Z0-9_@%:]): a bare link starts a word.
        (if (and (match-beginning 3) (> begin 0)
                 (string-match-p "[a-zA-Z0-9_@%:]" (substring text (1- begin) begin)))
            (setq from (1+ begin))
          (when (> begin start)
            (push (cons (substring text start begin) nil) segments))
          (push (cons shown target) segments)
          (setq start end from end))))
    (when (< start (length text))
      (push (cons (substring text start) nil) segments))
    (nreverse segments)))

(defun orgzly-ui-org-spans (text id field)
  "Render Org TEXT of note ID's FIELD as spans: emphasis and tappable links.
A system link (web, mail, phone, message, map) opens on the device with
the `uri.open' builtin; any other link's tap names the note, the field
and the link's index, never its target, so Emacs opens what its own
snapshot holds."
  (let ((index -1) (link (orgzly-design-color 'link (orgzly-ui-dark))))
    (cl-mapcan
     (lambda (segment)
       (if (null (cdr segment))
           (orgzly-ui--emphasis-spans (car segment))
         (let ((target (cdr segment)) (n (cl-incf index)))
           (list (jetpacs-span (car segment) :underline t :color link
                               :on-tap
                               ;; Revived hands system links to the device's
                               ;; own apps (EBP #208); the rest are Emacs's.
                               (jetpacs-org-link-action
                                target
                                (orgzly-ui-action
                                 "orgzly.link.open"
                                 (list :id id :field field :link n))))))))
     (orgzly-ui-link-segments text))))

(defun orgzly-ui-title-spans (entry)
  "Return ENTRY's title as Revived's TitleGenerator lays it out.
State and priority are bold, the state in the TODO or DONE color; parts are
separated by two spaces and the tags follow the title."
  (let* ((state (alist-get 'state entry))
         (done (member state (alist-get 'done (plist-get orgzly-state :ctx))))
         (dark (orgzly-ui-dark))
         (head (delq nil
                     (list (and state
                                (jetpacs-span state :font-weight "bold"
                                              :color (orgzly-design-color (if done 'done 'todo) dark)))
                           (when-let* ((priority (alist-get 'priority entry)))
                             (jetpacs-span (concat (if state "  " "") "#" priority)
                                           :font-weight "bold"))))))
    (append head
            (when head (list (jetpacs-span "  ")))
            (orgzly-ui-org-spans (alist-get 'title entry) (alist-get 'id entry) "title")
            (when-let* ((tags (alist-get 'tags entry)))
              ;; postTitleTextSize: 14sp after the 16sp title.
              (list (jetpacs-span (concat "  " (string-join tags " ")) :scale 0.875))))))

(defun orgzly-ui--faded-p (entry)
  "Non-nil when ENTRY is done or archived and Revived draws it at 45% alpha."
  (or (member (alist-get 'state entry) (alist-get 'done (plist-get orgzly-state :ctx)))
      (member "ARCHIVE" (alist-get 'tags entry))
      (member "ARCHIVE" (alist-get 'itags entry))))

(defun orgzly-ui--time (timestamp)
  "Format Org TIMESTAMP alist the way Revived's UserTimeFormatter does."
  (let* ((raw (alist-get 'raw timestamp))
         (time (ignore-errors (org-time-string-to-time raw)))
         (now (decode-time (plist-get orgzly-state :now))))
    (if (null time) raw
      (concat
       (format-time-string
        (if (= (decoded-time-year (decode-time time)) (decoded-time-year now))
            "%a, %b %-d" "%a, %b %-d, %Y")
        time)
       (when (string-match "[0-9]+:[0-9]+" raw)
         (format-time-string " %-I:%M %p" time))
       (when (string-match " \\([.+]?\\+[0-9]+[hdwmy]\\)" raw)
         (concat " " (match-string 1 raw)))))))

(defun orgzly-ui--folded-p (entry)
  "Non-nil when ENTRY's descendants and content are folded away."
  (member (alist-get 'id entry) orgzly-state-folded))

(defun orgzly-ui--foldable-p (entry)
  "Non-nil when ENTRY has children or content, so Revived shows its fold button."
  (or (alist-get 'has-children entry)
      (not (string-empty-p (alist-get 'content entry)))))

(defconst orgzly-ui-max-guides 8
  "Indent guides an outline row can draw before deeper notes share the last.")

(defconst orgzly-ui--planning
  '(("scheduled" scheduled "today") ("deadline" deadline "alarm")
    ("closed" closed "check_circle_outline"))
  "Planning members shown under a title: key, entry field and Revived's icon.")

(defun orgzly-ui--guide ()
  "One indent guide: item_head.xml's 20dp indent with its 1dp line centred.
The row is intrinsically sized, so the stretched guide spans the note."
  (jetpacs-with-attrs
   (jetpacs-row (jetpacs-spacer :width 9.33)
                (jetpacs-with-attrs
                 (orgzly-ui-styled "orgzly.guide.line" (jetpacs-spacer :width 1))
                 :align_self "stretch")
                :align "top")
   :width 20 :align_self "stretch"))

(defun orgzly-ui--bullet (ring)
  "The 20dp bullet slot: an 8dp dot, on a 16dp RING when a parent is folded."
  (let ((dot (orgzly-ui-styled "orgzly.bullet.dot" (jetpacs-spacer :width 8 :height 8))))
    (jetpacs-with-attrs
     (jetpacs-box (if ring
                      (orgzly-ui-styled "orgzly.bullet.ring"
                                        (jetpacs-with-attrs
                                         (jetpacs-box dot :alignment "center")
                                         :width 16 :height 16))
                    dot)
                  :alignment "center")
     :width 20 :height 21.7)))

(defun orgzly-ui--planning-line (text icon &optional flush)
  "A line under the title: 14dp ICON, 4dp, then 14sp TEXT.
Planning lines sit 2dp below what precedes them (the comfortable
below-title padding); FLUSH drops it, as the notebook line does."
  (let ((line (jetpacs-row
               (jetpacs-icon icon :size 14 :color (orgzly-design-color 'text (orgzly-ui-dark)))
               (orgzly-ui-styled-text "orgzly.t.content" text)
               :spacing 4 :align "center")))
    (if flush line (orgzly-ui-styled "orgzly.below-title" line))))

(defun orgzly-ui--row-column (item &optional show-book)
  "The title column of outline ITEM: title, planning lines and content.
SHOW-BOOK adds the notebook line search results carry.  A done or
archived note fades these at 45%, never its bullet (setupAlpha)."
  (let ((column (orgzly-ui--row-column-1 item show-book)))
    (if (gethash "faded" item) (orgzly-ui-styled "orgzly.faded" column) column)))

(defun orgzly-ui--row-column-1 (item show-book)
  "The unfaded title column of outline ITEM, with the notebook if SHOW-BOOK."
  (apply #'jetpacs-column
         (delq nil
               (append
                (list (orgzly-ui-styled "orgzly.t.title"
                                        (jetpacs-rich-text (append (gethash "spans" item) nil))))
                (list (and show-book (orgzly-ui--planning-line (gethash "book" item) "folder_open" t)))
                (mapcar (lambda (p)
                          (let ((text (gethash (car p) item)))
                            (unless (string-empty-p text)
                              (orgzly-ui--planning-line text (nth 2 p)))))
                        orgzly-ui--planning)
                (list (unless (string-empty-p (gethash "content" item))
                        (orgzly-ui-styled "orgzly.below-title"
                                          (orgzly-ui-styled
                                           "orgzly.t.content"
                                           (jetpacs-rich-text
                                            (append (gethash "content-spans" item) nil))))))))))

(defun orgzly-ui--swipe-side (actions)
  "A popup swipe side of ACTIONS, (LABEL ICON NAME ARGS [CONFIRM]) lists.
Revived's NotePopup: a fling on a note opens its buttons beside the
finger (SPEC 17.3 `presentation: popup', amendment #207)."
  (jetpacs-swipe
   (mapcar (lambda (a)
             (jetpacs-swipe-action (nth 0 a) :icon (nth 1 a)
                                   :on-trigger (orgzly-ui-action (nth 2 a) (nth 3 a) (nth 4 a))))
           actions)
   :presentation "popup"))

(defun orgzly-ui-note-swipes (id &optional query)
  "The popup swipe sides of note ID's row, as a plist for the pressable.
A fling to the right (the start side) offers Revived's default
schedule, deadline, state and done buttons; to the left, in a notebook,
delete and the three new-note placements.  QUERY rows (search, agenda)
keep only the right side: Revived's one left button opens the note's
notebook, which the row's tap already does."
  (append
   (list :swipe-start
         (orgzly-ui--swipe-side
          `(("Schedule" "today" "orgzly.note.plan" (:id ,id :field "scheduled"))
            ("Deadline" "alarm" "orgzly.note.plan" (:id ,id :field "deadline"))
            ("State" "flag_filled" "orgzly.note.state" (:id ,id))
            ("Done" "check_circle_outline" "orgzly.note.operate" (:id ,id :operation "toggle")))))
   (unless query
     (list :swipe-end
           (orgzly-ui--swipe-side
            `(("Delete" "delete" "orgzly.note.operate" (:id ,id :operation "delete")
               "Delete this note?")
              ("New above" "add_above" "orgzly.note.place" (:id ,id :place "above"))
              ("New under" "add_under" "orgzly.note.place" (:id ,id :place "under"))
              ("New below" "add_below" "orgzly.note.place" (:id ,id :place "below"))))))))

(defun orgzly-ui-outline-node (item)
  "Build outline ITEM as Revived's item_head.xml: guides, bullet, title, fold.
ITEM is the hash `orgzly-ui-outline-row' or `orgzly-ui-preface-row'
returns.  Rows are literal nodes rather than a template because a title's
state, priority and emphasis are rich spans, which SPEC 13.8.3 does not
let a template bind."
  (jetpacs-with-attrs
   (if (not (string-empty-p (gethash "preface" item)))
       (orgzly-ui-styled "orgzly.preface"
                         (orgzly-ui-styled-text "orgzly.t.preface" (gethash "preface" item)))
     (orgzly-ui-styled
      (if (gethash "selected" item) '("orgzly.row.box" "orgzly.row.selected") "orgzly.row.box")
      (apply
       #'jetpacs-row
       (append
        (list (jetpacs-spacer :width 16))
        (cl-loop repeat (gethash "depth" item) collect (orgzly-ui--guide))
        (list
         (jetpacs-with-attrs
          (apply #'jetpacs-design-pressable
                 (append (gethash "styles" item) nil)
                 ;; Revived: with a selection, a tap toggles instead of opening.
                 (orgzly-ui-action (if orgzly-state-selected "orgzly.note.select" "orgzly.note.open")
                                   (list :id (gethash "id" item)))
                 (list
                  (jetpacs-row
                   (orgzly-ui--bullet (gethash "ring" item))
                   (jetpacs-spacer :width 8)
                   (jetpacs-with-attrs (orgzly-ui--row-column item) :weight 1)
                   :align "top"))
                 :on-long-tap (orgzly-ui-action "orgzly.note.select" (list :id (gethash "id" item)))
                 (orgzly-ui-note-swipes (gethash "id" item)))
          :weight 1)
         (let ((width (orgzly-design-fold-width (jetpacs-window))))
           (if (string-empty-p (gethash "fold" item))
               (jetpacs-spacer :width width)
             (jetpacs-with-attrs
              (jetpacs-design-pressable
               '("orgzly.fold") (orgzly-ui-action "orgzly.note.fold" (list :id (gethash "id" item)))
               (list (jetpacs-with-attrs
                      (orgzly-ui-styled-text "orgzly.t.fold" (gethash "fold" item))
                      :width width))
               ;; "Long-click the icon to unfold the entire subtree."
               :on-long-tap (orgzly-ui-action "orgzly.note.fold-subtree"
                                              (list :id (gethash "id" item))))
              :width width)))
         :align "top")))))
   :key (gethash "id" item)))

(defun orgzly-ui-preface-row (book)
  "Return BOOK's preface `/rows' item, or nil when it has no preface."
  (let ((preface (string-trim (or (alist-get 'preface book) ""))))
    (unless (string-empty-p preface)
      (let ((row (make-hash-table :test #'equal :size 12)))
        (puthash "id" (concat "preface-" (alist-get 'id book)) row)
        (puthash "preface" preface row)
        (puthash "depth" 0 row)
        (puthash "styles" ["orgzly.head"] row)
        (puthash "spans" [] row)
        (dolist (p orgzly-ui--planning) (puthash (car p) "" row))
        (puthash "content" "" row)
        (puthash "fold" "" row)
        row))))

(defun orgzly-ui-outline-row (entry)
  "Return ENTRY's outline item: the values `orgzly-ui-outline-node' draws."
  (let* ((id (alist-get 'id entry))
         (level (max 0 (1- (alist-get 'level entry))))
         (folded (orgzly-ui--folded-p entry))
         (content (string-trim (or (alist-get 'content entry) "")))
         (row (make-hash-table :test #'equal :size 12)))
    (puthash "id" id row)
    (puthash "preface" "" row)
    (puthash "book" (alist-get 'book entry) row)
    (puthash "depth" (min orgzly-ui-max-guides level) row)
    (puthash "styles" ["orgzly.head"] row)
    (puthash "faded" (and (orgzly-ui--faded-p entry) t) row)
    (puthash "selected" (and (member id orgzly-state-selected) t) row)
    (puthash "ring" (and folded (alist-get 'has-children entry) t) row)
    (puthash "spans" (vconcat (orgzly-ui-title-spans entry)) row)
    (dolist (p orgzly-ui--planning)
      (puthash (car p) (if-let* ((ts (alist-get (nth 1 p) entry))) (orgzly-ui--time ts) "") row))
    (puthash "content" (if folded "" (truncate-string-to-width content 2000 nil nil "…")) row)
    (puthash "content-spans"
             (vconcat (unless (string-empty-p (gethash "content" row))
                        (orgzly-ui-org-spans (gethash "content" row) id "content")))
             row)
    (puthash "fold" (cond ((not (orgzly-ui--foldable-p entry)) "")
                          (folded "+") (t "–"))
             row)
    row))

(defun orgzly-ui-note-row (entry &optional show-book)
  "Build ENTRY's search-result row; SHOW-BOOK adds its notebook name.
Revived's search results drop the bullet, guides and fold button and pad
both edges by 16dp; the title, planning lines and content are the
outline row's."
  (let ((item (orgzly-ui-outline-row entry)))
    (jetpacs-with-attrs
     (orgzly-ui-styled
      "orgzly.result.box"
      (apply #'jetpacs-design-pressable
             (append (gethash "styles" item) nil)
             (orgzly-ui-action "orgzly.note.open" (list :id (alist-get 'id entry)))
             (list (orgzly-ui--row-column item show-book))
             (orgzly-ui-note-swipes (alist-get 'id entry) t)))
     :key (alist-get 'id entry))))

(defun orgzly-ui-page-window (count)
  "Return (PAGE START END) of the active page over COUNT entries."
  (let* ((page (min orgzly-state-page (max 0 (/ (1- count) orgzly-state-page-size))))
         (start (* page orgzly-state-page-size)))
    (list page start (min count (+ start orgzly-state-page-size)))))

(defun orgzly-ui-pager (count)
  "Return the Previous/Next row when COUNT exceeds one page, else nil."
  (pcase-let ((`(,page ,start ,end) (orgzly-ui-page-window count)))
    (when (> count orgzly-state-page-size)
      (jetpacs-row
       (jetpacs-button "Previous" (orgzly-ui-action "orgzly.page" (list :page (max 0 (1- page)))))
       (jetpacs-text (format "%d–%d of %d" (1+ start) end count) :style "caption")
       (jetpacs-button "Next" (orgzly-ui-action "orgzly.page" (list :page (1+ page))))
       :spacing 8 :align "center"))))

(defun orgzly-ui-page (entries &optional show-book)
  "Return bounded page nodes for ENTRIES, optionally showing SHOW-BOOK."
  (pcase-let ((`(,_ ,start ,end) (orgzly-ui-page-window (length entries))))
    (append
     (mapcar (lambda (entry) (orgzly-ui-note-row entry show-book)) (cl-subseq entries start end))
     (when-let* ((pager (orgzly-ui-pager (length entries)))) (list pager)))))

(defun orgzly-ui-outline-rows (name)
  "Return the `/rows' array for NAME's visible outline at the active page."
  (let ((entries (orgzly-state-visible (orgzly-state-entries name))))
    (pcase-let ((`(,_ ,start ,end) (orgzly-ui-page-window (length entries))))
      (vconcat (delq nil (cons (and (zerop start)
                                    (orgzly-ui-preface-row (orgzly-state-book name)))
                               (mapcar #'orgzly-ui-outline-row
                                       (cl-subseq entries start end))))))))

(defun orgzly-ui--book-time ()
  "The snapshot time as Revived formats a notebook's last action."
  (format-time-string "%a, %b %-d, %-I:%M %p" (plist-get orgzly-state :now)))

(defun orgzly-ui--book-detail (icon text)
  "One BookItemInfoIcon line: 14dp ICON and 12sp TEXT."
  (jetpacs-row
   (orgzly-ui-styled "orgzly.detail.icon"
                     (jetpacs-icon icon :size 14 :color (orgzly-design-color 'text (orgzly-ui-dark))))
   (jetpacs-with-attrs (orgzly-ui-styled-text "orgzly.t.detail" text) :weight 1)
   :align "center"))

(defun orgzly-ui-book-card (book)
  "Return BOOK's item_book.xml card: title, then modification and last action."
  (jetpacs-with-attrs
   (orgzly-ui-styled
    "orgzly.card.margin"
    (jetpacs-design-pressable
     '("orgzly.card") (orgzly-ui-action "orgzly.book.open" (list :id (alist-get 'id book)))
     (list
      (jetpacs-column
       (orgzly-ui-styled-text "orgzly.t.book" (alist-get 'title book))
       (orgzly-ui-styled
        "orgzly.details"
        (jetpacs-column
         (orgzly-ui--book-detail "access_time" "Not modified")
         (orgzly-ui--book-detail
          "info"
          (format "%s: Loaded from %s.org" (orgzly-ui--book-time) (alist-get 'name book)))))))))
   :key (alist-get 'id book)))

(defun orgzly-ui-books ()
  "Return the notebook cards from the published snapshot."
  (append
   (mapcar #'orgzly-ui-book-card (plist-get orgzly-state :books))
   (unless (plist-get orgzly-state :books)
     (list (orgzly-ui-styled "orgzly.preface"
                             (orgzly-ui-styled-text "orgzly.t.preface"
                                                    "No notebooks yet. Create one to start writing."))))
   ;; fragment_books: the list clears the FAB (padding_bottom_for_fab).
   (list (jetpacs-spacer :height 104))))

(defun orgzly-ui-book (name)
  "Return NAME's preface and outline rows from the snapshot."
  (let ((book (orgzly-state-book name)))
    (if (null book)
        (list (orgzly-ui-styled "orgzly.preface"
                                (orgzly-ui-styled-text "orgzly.t.preface"
                                                       "This notebook is no longer available. Refresh to continue.")))
      (append
       (list (jetpacs-with-attrs
              (apply #'jetpacs-lazy-column
                     ;; fragment_book.xml pads the list 8dp at the top.
                     (append (list (jetpacs-spacer :height 8))
                             (mapcar #'orgzly-ui-outline-node
                                     (orgzly-ui-outline-rows name))
                             ;; padding_bottom_for_fab
                             (list (jetpacs-spacer :height 104))))
              :weight 1))
       (when-let* ((pager (orgzly-ui-pager
                           (length (orgzly-state-visible (orgzly-state-entries name))))))
         (list pager))))))

(defun orgzly-ui--drawer-item (label action &optional icon active)
  "One NavigationView row: LABEL running ACTION.
An optional ICON leads the label; ACTIVE draws the selection pill."
  (let ((color (orgzly-design-color (if active 'on-pill 'text) (orgzly-ui-dark))))
    (jetpacs-with-attrs
     (orgzly-ui-styled
      "orgzly.drawer.margin"
      (jetpacs-design-pressable
       (list (if active "orgzly.drawer.active" "orgzly.drawer.item")) action
       (list
        (jetpacs-with-attrs
         (jetpacs-row
          (if icon (jetpacs-icon icon :size 24 :color color) (jetpacs-spacer :width 24))
          (jetpacs-spacer :width 12)
          (orgzly-ui-styled-text "orgzly.t.drawer" label
                                 :font-weight (if active "bold" nil)
                                 :color (and active color))
          :align "center")
         :height 48))))
     :key (jetpacs-wire-id "drawer" label))))

(defun orgzly-ui--drawer-footer ()
  "The drawer's Sync footer, which reloads the notebooks from their files."
  (orgzly-ui-styled
   "orgzly.drawer.footer"
   (jetpacs-design-pressable
    '("orgzly.drawer.plain") (orgzly-ui-action "orgzly.refresh")
    (list
     (jetpacs-with-attrs
      (jetpacs-row
       (jetpacs-icon "sync" :size 24 :color (orgzly-design-color 'text (orgzly-ui-dark)))
       (jetpacs-spacer :width 16)
       (orgzly-ui-styled-text "orgzly.t.drawer" "Sync")
       :align "center")
      :height 24)))))

(defun orgzly-ui-drawer (route)
  "Revived's navigation drawer for ROUTE: searches, notebooks and settings."
  (let ((kind (plist-get route :kind)))
    (orgzly-ui-styled
     "orgzly.drawer"
     (apply
      #'jetpacs-column
      (append
       (list (orgzly-ui--drawer-item "Searches" (orgzly-ui-action "orgzly.open" '(:view "searches"))
                                     "search" (eq kind 'searches)))
       (mapcar (lambda (search)
                 (orgzly-ui--drawer-item (car search)
                                         (orgzly-ui-action "orgzly.search.saved"
                                                           (list :name (car search)))
                                         nil (and (eq kind 'query)
                                                  (equal (plist-get route :name) (car search)))))
               (plist-get orgzly-state :searches))
       (list (orgzly-ui--drawer-item "Notebooks" (orgzly-ui-action "orgzly.open" '(:view "books"))
                                     "library_books_filled" (eq kind 'books)))
       (mapcar (lambda (book)
                 (orgzly-ui--drawer-item (alist-get 'title book)
                                         (orgzly-ui-action "orgzly.book.open"
                                                           (list :id (alist-get 'id book)))
                                         nil (and (eq kind 'book)
                                                  (equal (plist-get route :book)
                                                         (alist-get 'name book)))))
               (plist-get orgzly-state :books))
       (list (orgzly-ui--drawer-item "Settings" (orgzly-ui-action "orgzly.open" '(:view "settings"))
                                     "settings_filled" (eq kind 'settings))
             (jetpacs-spacer :weight 1)
             (orgzly-ui--drawer-footer)
             :spacing 0))))))

(defun orgzly-ui-fab (route)
  "Revived's 56dp add FAB for ROUTE, or nil where it has none."
  (when-let* ((action
               (pcase (plist-get route :kind)
                 ('books (orgzly-ui-action "orgzly.book.new"))
                 ('searches (orgzly-ui-action "orgzly.open" '(:view "save-search")))
                 ('book (when-let* ((book (orgzly-state-book (plist-get route :book))))
                          (orgzly-ui-action "orgzly.note.new" (list :id (alist-get 'id book))))))))
    (jetpacs-design-pressable
     '("orgzly.fab") action
     (list (jetpacs-with-attrs
            (jetpacs-box (jetpacs-icon "add" :size 24
                                       :color (orgzly-design-color 'on-fab (orgzly-ui-dark)))
                         :alignment "center")
            :width 56 :height 56)))))

(defun orgzly-ui--list-divider ()
  "The 1dp line between list rows (DividerItemDecoration)."
  (orgzly-ui-styled "orgzly.divider" (jetpacs-spacer :height 1)))

(defun orgzly-ui--with-dividers (rows)
  "ROWS with a list divider after each, keyed after its row."
  (cl-loop for row in rows for n from 0
           append (list row (jetpacs-with-attrs (orgzly-ui--list-divider)
                                                :key (format "div-%d" n)))))

(defun orgzly-ui-result-row (entry &optional kind key)
  "ENTRY as a search or agenda result: title, notebook and planning lines.
KIND, an agenda occurrence's `scheduled', `deadline' or `event', keeps
only that time, as Revived's agenda does; content stays hidden, as its
search results default.  KEY overrides the row's identity."
  (let ((item (orgzly-ui-outline-row entry)))
    (puthash "content" "" item)
    (when kind
      (dolist (p orgzly-ui--planning)
        (unless (eq (nth 1 p) kind) (puthash (car p) "" item)))
      (when (eq kind 'event)
        (puthash "event" (orgzly-ui--time (car (alist-get 'events entry))) item)))
    (jetpacs-with-attrs
     (orgzly-ui-styled
      "orgzly.result.box"
      (apply #'jetpacs-design-pressable
             '("orgzly.head")
             (orgzly-ui-action "orgzly.note.open" (list :id (alist-get 'id entry)))
             (list (orgzly-ui--row-column item t))
             (orgzly-ui-note-swipes (alist-get 'id entry) t)))
     :key (or key (alist-get 'id entry)))))

(defun orgzly-ui-searches ()
  "Revived's Searches list: each saved query's name over its query."
  (append
   (orgzly-ui--with-dividers
    (mapcar
     (lambda (search)
       (jetpacs-with-attrs
        (jetpacs-box
         (orgzly-ui-styled
          "orgzly.search.row"
          (jetpacs-column
           (orgzly-ui-styled-text "orgzly.t.search-name" (car search))
           (orgzly-ui-styled-text "orgzly.t.content" (cdr search))))
         :on-tap (orgzly-ui-action "orgzly.search.saved" (list :name (car search)))
         :on-long-tap (orgzly-ui-action "orgzly.search.delete" (list :name (car search))
                                        "Delete this saved search?"))
        :key (jetpacs-wire-id "search" (car search))))
     (plist-get orgzly-state :searches)))
   (list (jetpacs-spacer :height 104))))

(defun orgzly-ui-query (route)
  "The results of ROUTE's query: notes, or agenda days for an `ad.N' query."
  (let ((query (orgzly-query-parse (or (plist-get route :query) ""))))
    (append
     (if (plist-get query :agenda-days)
         (orgzly-ui-agenda query)
       (or (orgzly-ui--with-dividers
            (pcase-let ((`(,_ ,start ,end) (orgzly-ui-page-window
                                            (length (orgzly-query-select
                                                     (orgzly-state-entries nil) query
                                                     (plist-get orgzly-state :ctx))))))
              (mapcar #'orgzly-ui-result-row
                      (cl-subseq (orgzly-query-select (orgzly-state-entries nil) query
                                                      (plist-get orgzly-state :ctx))
                                 start end))))
           (list (orgzly-ui-styled "orgzly.preface"
                                   (orgzly-ui-styled-text "orgzly.t.preface" "No notes found.")))))
     (list (jetpacs-spacer :height 104)))))

(defun orgzly-ui--band (label key)
  "An agenda day divider keyed KEY: LABEL bold on the container band."
  (jetpacs-with-attrs
   (orgzly-ui-styled "orgzly.band"
                     (jetpacs-with-attrs
                      (jetpacs-box (orgzly-ui-styled-text "orgzly.t.band" label)
                                   :alignment "center_start")
                      :height 48))
   :key key))

(defun orgzly-ui-agenda (&optional query)
  "Return bounded day sections for QUERY using the snapshot clock.
Every day of the range gets its divider, an empty one too, and each
occurrence shows only the time that placed it there."
  (let ((sections (orgzly-agenda-sections
                   (orgzly-state-entries nil) (or query (orgzly-query-parse ".it.done ad.7"))
                   (plist-get orgzly-state :ctx) (plist-get orgzly-state :now)))
        (remaining orgzly-state-page-size) nodes (n 0))
    (dolist (section sections)
      (when (or (cdr section) (not (eq (car section) 'overdue)))
        (push (orgzly-ui--band (if (eq (car section) 'overdue) "Overdue"
                                 (format-time-string "%a, %b %-d" (seconds-to-time (car section))))
                               (format "band-%d" (cl-incf n)))
              nodes)
        (push (jetpacs-with-attrs (orgzly-ui--list-divider) :key (format "band-div-%d" n)) nodes)
        (dolist (item (cdr section))
          (when (> remaining 0)
            ;; One heading may occur on several days; keys are occurrence-owned.
            (push (orgzly-ui-result-row (alist-get 'entry item) (alist-get 'kind item)
                                        (format "occ-%d" remaining))
                  nodes)
            (push (jetpacs-with-attrs (orgzly-ui--list-divider) :key (format "occ-div-%d" remaining))
                  nodes)
            (setq remaining (1- remaining))))))
    (when (= remaining 0)
      (push (orgzly-ui-styled "orgzly.preface"
                              (orgzly-ui-styled-text "orgzly.t.content"
                                                     "Showing the first 40 occurrences. Narrow the query for more."))
            nodes))
    (nreverse nodes)))

(defun orgzly-ui-field (id label value &optional multiline)
  "Build field ID with LABEL and VALUE; MULTILINE enables the Org content area."
  (jetpacs-text-input (orgzly-field-id id) :label label :value (or value "")
                      :single-line (if multiline :json-false t)
                      :min-lines (if multiline 6 1) :max-lines (if multiline 16 1)
                      :max-length (if multiline 16000 512)
                      :monospace (and multiline t) :syntax (and multiline "org")))

(defun orgzly-ui--ancestors (entry)
  "ENTRY's ancestor notes in the snapshot, outermost first."
  (let ((level (alist-get 'level entry)) ancestors)
    (dolist (e (reverse (cl-subseq (orgzly-state-entries (alist-get 'book entry)) 0
                                   (cl-position (alist-get 'id entry)
                                                (orgzly-state-entries (alist-get 'book entry))
                                                :key (lambda (x) (alist-get 'id x))
                                                :test #'equal))))
      (when (< (alist-get 'level e) level)
        (push e ancestors)
        (setq level (alist-get 'level e))))
    ancestors))

(defun orgzly-ui--breadcrumbs (book entry parent)
  "Breadcrumbs for BOOK, then ENTRY's or PARENT's ancestors, as links."
  (let* ((link (orgzly-design-color 'link (orgzly-ui-dark)))
         (book-alist (orgzly-state-book book))
         (crumbs (append
                  (list (cons (or (alist-get 'title book-alist) book)
                              (orgzly-ui-action "orgzly.book.open"
                                                (list :id (alist-get 'id book-alist)))))
                  (mapcar (lambda (e) (cons (alist-get 'title e)
                                            (orgzly-ui-action "orgzly.note.open"
                                                              (list :id (alist-get 'id e)))))
                          (append (and entry (orgzly-ui--ancestors entry))
                                  (and parent (append (orgzly-ui--ancestors parent)
                                                      (list parent))))))))
    (orgzly-ui-styled
     "orgzly.note.bread"
     (orgzly-ui-styled
      "orgzly.t.bread"
      (jetpacs-rich-text
       (cdr (cl-mapcan (lambda (crumb)
                         (list (jetpacs-span "  •  ")
                               ;; A new note's crumbs are plain, as Revived's are.
                               (if entry
                                   (jetpacs-span (car crumb) :underline t :color link
                                                 :on-tap (cdr crumb))
                                 (jetpacs-span (car crumb)))))
                       crumbs)))))))

(defun orgzly-ui--note-divider ()
  "The 1dp HorizontalDivider between the note's sections."
  (orgzly-ui-styled "orgzly.divider" (jetpacs-spacer :height 1)))

(defun orgzly-ui--section-header (icon section folded)
  "A SECTION header: ICON and the chevron, down when FOLDED."
  (jetpacs-with-attrs
   (jetpacs-design-pressable
    '("orgzly.note.header") (orgzly-ui-action "orgzly.note.section" (list :section section))
    (list (jetpacs-with-attrs
           (jetpacs-row
            (jetpacs-spacer :width 16)
            (jetpacs-icon icon :size 24 :color (orgzly-design-color 'text (orgzly-ui-dark)))
            (jetpacs-spacer :weight 1)
            (jetpacs-icon (if folded "keyboard_arrow_down" "keyboard_arrow_up") :size 24
                          :color (orgzly-design-color 'text (orgzly-ui-dark)))
            (jetpacs-spacer :width 16)
            :align "center")
           :height 48 :weight 1)))
   :key (concat "header-" section)))

(defun orgzly-ui--field-scope (kind child)
  "CHILD under a nested scope binding the Foundation field slots to KIND.
KIND is a symbol: `title', `value' or `content', each a chromeless
TextView-shaped field."
  (let ((prefix (format "orgzly.field.%s" kind)))
    (jetpacs-design-scope
     nil nil (list child)
     :component-styles
     (list (jetpacs-design-component-style "text-field.outlined" (list (concat prefix ".box")))
           (jetpacs-design-component-style "text-field.filled" (list (concat prefix ".box")))
           (jetpacs-design-component-style "text-field.text" (list (concat prefix ".text")))
           (jetpacs-design-component-style "text-field.placeholder" (list (concat prefix ".hint")))))))

(defun orgzly-ui--input (kind id value hint &optional multiline focus suggestions)
  "A chromeless KIND field ID holding VALUE with placeholder HINT.
MULTILINE lets it grow over as many lines as its text takes; FOCUS
focuses it, opening the keyboard.  SUGGESTIONS, words, complete the word
being typed, as Revived's tags field does."
  (orgzly-ui--field-scope
   kind
   (jetpacs-text-input (orgzly-field-id id) :value (or value "") :hint hint
                       :autofocus (and focus t)
                       :single-line (if multiline :json-false t)
                       :min-lines 1 :max-lines (if multiline 1000 1)
                       :max-length (if multiline 64000 512)
                       :suggestions (and suggestions (seq-take suggestions 1000)))))

(defun orgzly-ui-content-document (scope)
  "The document of form SCOPE's content, a synchronized editor (SPEC 19).
Emacs assigns the identifier; the Companion only ever echoes it."
  (concat "orgzly:content:" scope))

(defun orgzly-ui-content-synced-p ()
  "Whether the note's content is a synchronized editor (SPEC 19).
Revived's content field is a real editor: a timestamp lands at its
caret and Org editing can follow.  A Companion without `editor.sync'
gets the captured text field instead."
  (jetpacs-granted-p "editor.sync"))

(defun orgzly-ui--content-editor (text focus)
  "The note's content as a synchronized editor seeded with TEXT.
FOCUS focuses it, opening the keyboard.  The editor wears the content
field's TextView-shaped styles; its text lives in the editor session's
mirror, so Done reads the draft the mirror keeps, never a capture."
  (let ((prefix "orgzly.field.content"))
    (jetpacs-design-scope
     nil nil
     (list (jetpacs-editor (orgzly-field-id "oz-content")
                           :document (orgzly-ui-content-document orgzly-field-scope)
                           :value (or text "") :hint "Content" :chromeless t
                           :min-lines 1 :max-lines 1000
                           :autofocus (and focus t)))
     :component-styles
     ;; The editor's own typography is code; the field's is the body font.
     (list (jetpacs-design-component-style "editor.chromeless" (list (concat prefix ".box")))
           (jetpacs-design-component-style "editor.text" (list "typography.body" (concat prefix ".text")))
           (jetpacs-design-component-style "editor.placeholder"
                                           (list "typography.body" (concat prefix ".hint")))))))

(defun orgzly-ui--meta-row (icon body &optional trailing key)
  "One metadata row: ICON, BODY and an optional TRAILING button, keyed KEY."
  (jetpacs-with-attrs
   (jetpacs-row
    (jetpacs-spacer :width 16)
    (jetpacs-icon icon :size 24 :color (orgzly-design-color 'text (orgzly-ui-dark)))
    (jetpacs-spacer :width 24)
    (jetpacs-with-attrs body :weight 1)
    (or trailing (jetpacs-spacer :width 48))
    (jetpacs-spacer :width 4)
    :align "center")
   :height 48 :key key))

(defun orgzly-ui--meta-button (icon label action)
  "A 48dp metadata button showing ICON, named LABEL, running ACTION."
  (jetpacs-icon-button icon action :content-description label
                       :color (orgzly-design-color 'text (orgzly-ui-dark))))

(defun orgzly-ui--meta-value (field value hint id &optional raw)
  "A tappable metadata value: VALUE, or HINT when empty, picking FIELD of ID.
RAW, the field's Org timestamp, seeds the timestamp dialog."
  (jetpacs-design-pressable
   '("orgzly.note.value") (orgzly-ui-action "orgzly.note.pick"
                                            (append (list :field field :scope id)
                                                    (and raw (list :raw raw))))
   (list (jetpacs-with-attrs
          (orgzly-ui-styled-text (if (string-empty-p value) "orgzly.t.hint" "orgzly.t.value")
                                 (if (string-empty-p value) hint value))
          :weight 1))))

(defun orgzly-ui-note-editing-p (field)
  "Non-nil when the open form edits FIELD (\"title\" or \"content\") in place."
  (and (equal (plist-get orgzly-state-editing :scope) orgzly-field-scope)
       (member field (plist-get orgzly-state-editing :fields))))

(defun orgzly-ui--note-text (kind field text hint id)
  "FIELD of note ID in view mode: TEXT rendered as Org, or HINT; tap edits.
KIND is `title' or `content', which share their metrics with the fields."
  (jetpacs-with-attrs
   (jetpacs-design-pressable
    (list (format "orgzly.note.%s-view" kind))
    (orgzly-ui-action "orgzly.note.edit" (list :field field :scope orgzly-field-scope))
    (list (jetpacs-with-attrs
           (if (string-empty-p text)
               (orgzly-ui-styled-text (format "orgzly.field.%s.hint" kind) hint)
             (orgzly-ui-styled (format "orgzly.field.%s.text" kind)
                               (jetpacs-rich-text (orgzly-ui-org-spans text id field))))
           :weight 1)))
   :key (concat field "-view")))

(defun orgzly-ui-note-field-shown-p (kind entry)
  "Whether the note form shows metadata KIND's field for ENTRY.
KIND is `tags' or `properties'; folding the section or Revived's
\"Show selected\" can take either off the screen."
  (and (not (memq 'metadata orgzly-state-note-folds))
       (orgzly-ui--metadata-shown-p
        kind (pcase kind
               ('tags (not (string-blank-p (orgzly-ui-note-tags entry))))
               ('properties (and (alist-get 'properties entry) t))))))

(defun orgzly-ui-note-captures (route)
  "The fields Done captures on note ROUTE: those shown as editable."
  (let* ((orgzly-field-scope (orgzly-field-scope route))
         (id (plist-get route :id))
         (entry (and id (ignore-errors (orgzly-state-resolve id t)))))
    (append (when (or (eq (plist-get route :kind) 'new-note) (orgzly-ui-note-editing-p "title"))
              '("oz-title"))
            (when (orgzly-ui-note-field-shown-p 'tags entry) '("oz-tags"))
            (when (orgzly-ui-note-field-shown-p 'properties entry) '("oz-prop-name" "oz-prop-value"))
            ;; A synchronized content editor is read from its mirror.
            (when (and (orgzly-ui-note-editing-p "content") (not (orgzly-ui-content-synced-p)))
              '("oz-content")))))

(defun orgzly-ui-note-value (field entry)
  "FIELD's current editor value for ENTRY: the draft's, else the note's.
FIELD is one of \"state\", \"priority\", \"scheduled\" or \"deadline\";
the value is a string, empty when unset."
  (let ((draft (and (equal (plist-get orgzly-state-draft :scope) orgzly-field-scope)
                    orgzly-state-draft))
        (key (intern (concat ":" field))))
    (if (plist-member draft key) (plist-get draft key)
      (pcase field
        ("state" (or (alist-get 'state entry) ""))
        ("priority" (or (alist-get 'priority entry) ""))
        ((or "scheduled" "deadline")
         (or (alist-get 'raw (alist-get (intern field) entry)) ""))))))

(defun orgzly-ui-note-text (field entry)
  "The editor value of ENTRY's \"title\" or \"content\" FIELD.
A timestamp inserted into the field puts the new text in the draft."
  (let ((draft (and (equal (plist-get orgzly-state-draft :scope) orgzly-field-scope)
                    orgzly-state-draft)))
    (or (plist-get draft (intern (concat ":" field)))
        (alist-get (intern field) entry))))

(defun orgzly-ui-note-tags (entry)
  "The Tags field's value for ENTRY: the draft's, else the note's own tags."
  (let ((draft (and (equal (plist-get orgzly-state-draft :scope) orgzly-field-scope)
                    orgzly-state-draft)))
    (or (plist-get draft :tags) (string-join (alist-get 'tags entry) " "))))

(defun orgzly-ui-all-tags ()
  "Every tag set on a note of any notebook, sorted as Revived lists them."
  (sort (delete-dups (cl-mapcan (lambda (e) (copy-sequence (alist-get 'tags e)))
                                (orgzly-state-entries nil)))
        #'string<))

(defun orgzly-ui--tags-buttons (scope)
  "The Tags row's buttons for form SCOPE: the dropdown, and Clear with text.
Revived shows Clear while the field holds text, so the Companion picks
between the two rows from the field's live value, with no round trip."
  (jetpacs-design-state-switch
   (orgzly-field-id "oz-tags")
   `(("" . ,(orgzly-ui--tags-row scope nil)))
   :otherwise (orgzly-ui--tags-row scope t)))

(defun orgzly-ui--tags-row (scope clear)
  "The Tags row's buttons for form SCOPE: the dropdown, and Clear if CLEAR.
Revived's \"...\" opens the autocomplete dropdown of every known tag under
the field; a picked tag goes into the field as a typed one would."
  (let* ((tags (orgzly-ui-all-tags))
         (text (orgzly-design-color 'text (orgzly-ui-dark)))
         (menu (if (null tags)
                   (jetpacs-icon-button "more_horiz" (orgzly-ui-action "orgzly.note.tag")
                                      :content-description "Tags" :color text :enabled :json-false)
                 (orgzly-design-tags-scope
                  (orgzly-ui-dark) (jetpacs-window) clear
                  (jetpacs-menu
                   (mapcar (lambda (tag)
                             (jetpacs-menu-item
                              tag (orgzly-ui-action "orgzly.note.tag" (list :scope scope :tag tag)
                                                    nil '("oz-tags"))))
                           tags)
                   :icon "more_horiz")))))
    (if (not clear) menu
      (jetpacs-row menu
                   (jetpacs-icon-button "close" (orgzly-ui-action "orgzly.note.clear"
                                                                  (list :field "tags" :scope scope))
                                        :content-description "Clear tags" :color text)))))

(defun orgzly-ui--shown-time (raw)
  "RAW Org timestamp as Revived shows it, or the empty string."
  (if (string-empty-p raw) "" (orgzly-ui--time (list (cons 'raw raw)))))

(defun orgzly-ui-picker-dialog (field title choices current)
  "The picker for note FIELD: an AlertDialog titled TITLE.
CHOICES, when given, form Revived's single-choice list with CURRENT
checked; otherwise a timestamp field holds CURRENT.  Clear and Cancel sit
in the button bar, Set too for a timestamp."
  (let ((ts-id (jetpacs-wire-id "pick-ts" field))
        (text (orgzly-design-color 'text (orgzly-ui-dark))))
    (orgzly-ui-styled
     "orgzly.dialog"
     (apply
      #'jetpacs-column
      (append
       (list (jetpacs-spacer :height 18.13)
             (orgzly-ui-styled "orgzly.dialog.title"
                               (orgzly-ui-styled-text "orgzly.t.dialog-title" title))
             (jetpacs-spacer :height 16))
       (if choices
           (mapcar
            (lambda (c)
              (jetpacs-with-attrs
               (jetpacs-design-pressable
                '("orgzly.dialog.item") (jetpacs-dialog-submit :value c)
                (list (jetpacs-with-attrs
                       (jetpacs-row
                        (jetpacs-spacer :width 24)
                        (jetpacs-icon (if (equal c current) "radio_button_checked"
                                        "radio_button_unchecked")
                                      :size 24
                                      :color (if (equal c current)
                                                 (orgzly-design-color 'link (orgzly-ui-dark))
                                               text))
                        (jetpacs-spacer :width 23.29)
                        (orgzly-ui-styled-text "orgzly.t.value" c)
                        :align "center")
                       :height 48 :weight 1)))
               :key (jetpacs-wire-id "choice" c)))
            choices)
         (list (orgzly-ui-styled
                "orgzly.dialog.title"
                (orgzly-ui--field-scope
                 'value (jetpacs-text-input ts-id :value (or current "") :single-line t
                                            :hint "<2026-10-25 Sun 10:00 +1w>")))))
       (list (jetpacs-spacer :height 13.87)
             (jetpacs-with-attrs
              (jetpacs-row
               (jetpacs-spacer :width 24)
               (orgzly-ui--dialog-button "Clear" (jetpacs-dialog-submit :value "__clear__"))
               (jetpacs-spacer :weight 1)
               (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
               (unless choices
                 (orgzly-ui--dialog-button "Set" (jetpacs-dialog-submit :capture-fields (list ts-id))))
               (jetpacs-spacer :width 24)
               :align "center")
              :height 48)
             (jetpacs-spacer :height 13.87)))))))

(defun orgzly-ui-text-dialog (title value)
  "An AlertDialog titled TITLE editing VALUE in the `oz-setting' field."
  (orgzly-ui-styled
   "orgzly.dialog"
   (jetpacs-column
    (jetpacs-spacer :height 18.13)
    (orgzly-ui-styled "orgzly.dialog.title" (orgzly-ui-styled-text "orgzly.t.dialog-title" title))
    (jetpacs-spacer :height 16)
    (orgzly-ui-styled "orgzly.dialog.title"
                      (orgzly-ui--field-scope
                       'value (jetpacs-text-input "oz-setting" :value value :single-line t
                                                  :max-length 512 :autofocus t)))
    (jetpacs-spacer :height 13.87)
    (jetpacs-with-attrs
     (jetpacs-row
      (jetpacs-spacer :weight 1)
      (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
      (orgzly-ui--dialog-button "OK" (jetpacs-dialog-submit :capture-fields '("oz-setting")))
      (jetpacs-spacer :width 24)
      :align "center")
     :height 48)
    (jetpacs-spacer :height 13.87))))

(defun orgzly-ui-name-dialog (title hint ok &optional value)
  "Revived's single-field AlertDialog: TITLE, an underlined HINT field, OK.
The field is `oz-dialog-name', holding VALUE; Cancel dismisses and OK
submits it."
  (orgzly-ui-styled
   "orgzly.dialog"
   (jetpacs-column
    (jetpacs-spacer :height 18.13)
    (orgzly-ui-styled "orgzly.dialog.title" (orgzly-ui-styled-text "orgzly.t.dialog-title" title))
    (jetpacs-spacer :height 16)
    (orgzly-ui-styled
     "orgzly.dialog.input"
     (jetpacs-column
      (orgzly-ui--field-scope
       'dialog (jetpacs-text-input "oz-dialog-name" :value (or value "") :hint hint :single-line t
                                   :max-length 512 :autofocus t))
      (orgzly-ui-styled "orgzly.dialog.underline" (jetpacs-spacer :height 2))))
    (jetpacs-spacer :height 36.6)
    (jetpacs-with-attrs
     (jetpacs-row
      (jetpacs-spacer :weight 1)
      (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
      (jetpacs-spacer :width 8)
      (orgzly-ui--dialog-button ok (jetpacs-dialog-submit :capture-fields '("oz-dialog-name")))
      (jetpacs-spacer :width 24)
      :align "center")
     :height 48)
    (jetpacs-spacer :height 13.5))))

(defun orgzly-ui-choice-dialog (title choices)
  "An AlertDialog titled TITLE listing CHOICES, (LABEL . ACTION) pairs."
  (orgzly-ui-styled
   "orgzly.dialog"
   (apply
    #'jetpacs-column
    (append
     (list (jetpacs-spacer :height 18.13)
           (orgzly-ui-styled "orgzly.dialog.title"
                             (orgzly-ui-styled-text "orgzly.t.dialog-title" title))
           (jetpacs-spacer :height 8))
     (mapcar (lambda (choice)
               (jetpacs-with-attrs
                (jetpacs-design-pressable
                 '("orgzly.dialog.item") (cdr choice)
                 (list (jetpacs-with-attrs
                        (jetpacs-row (jetpacs-spacer :width 24)
                                     (orgzly-ui-styled-text "orgzly.t.value" (car choice))
                                     :align "center")
                        :height 48 :weight 1)))
                :key (jetpacs-wire-id "choice" (car choice))))
             choices)
     (list (jetpacs-spacer :height 8)
           (jetpacs-with-attrs
            (jetpacs-row (jetpacs-spacer :weight 1)
                         (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
                         (jetpacs-spacer :width 24)
                         :align "center")
            :height 48)
           (jetpacs-spacer :height 13.87))))))

(defconst orgzly-ui-period-units
  '(("h" . "Hour") ("d" . "Day") ("w" . "Week") ("m" . "Month") ("y" . "Year"))
  "Org interval units and the names Revived's period wheels show.")

(defun orgzly-ui-period-kind (part field)
  "The period dialog for timestamp PART of FIELD, as Revived picks it.
A plist: :title, :description, :types (cookie prefixes) and :hints (the
line under the wheels for each type).  A deadline's delay is a warning
period, which has no types."
  (cond
   ((equal part "repeater")
    '(:title "Repeat" :description "After task is marked complete, repeater will shift the time"
      :types ("+" "++" ".+")
      :hints ("Add period to time" "Keep adding period to time until time reaches future"
              "Add period to current time")))
   ((equal field "deadline")
    '(:title "Warning period" :description "Warn about the approaching or missed deadline"
      :types ("-") :hints nil))
   (t
    '(:title "Delay" :description "Delay the display of the task in the agenda"
      :types ("-" "--")
      :hints ("Affects all occurrences" "Affects only the first scheduled occurrence")))))

(defun orgzly-ui-period-dialog (kind type value unit on-type)
  "Revived's period dialog for KIND (see `orgzly-ui-period-kind').
Three wheels: TYPE among the kind's types, VALUE from 1 to 100 and UNIT,
an Org unit letter.  ON-TYPE runs when the type wheel settles.  OK
submits, capturing `oz-period-type', `oz-period-value' and
`oz-period-unit'."
  (let* ((dark (orgzly-ui-dark))
         (types (plist-get kind :types))
         (strong (orgzly-design-color 'strong dark))
         (divider (orgzly-design-color 'divider dark))
         (wheel (lambda (id values value &rest more)
                  (apply #'jetpacs-design-wheel id values value :color strong :divider-color divider more)))
         (hint (lambda (text)
                 (orgzly-ui-styled "orgzly.period.hint"
                                   (jetpacs-with-attrs (orgzly-ui-styled-text "orgzly.t.period-hint" text)
                                                       :fill_fraction 1)))))
    (orgzly-ui-styled
     "orgzly.dialog"
     (jetpacs-column
      (jetpacs-spacer :height 18)
      (orgzly-ui-styled "orgzly.dialog.title"
                        (orgzly-ui-styled-text "orgzly.t.period-title" (plist-get kind :title)))
      (jetpacs-spacer :height 16)
      (funcall hint (plist-get kind :description))
      (jetpacs-spacer :height 8)
      (jetpacs-with-attrs
       (apply #'jetpacs-row
              (append
               (list (jetpacs-spacer :weight 1))
               (when (cdr types)
                 (list (funcall wheel "oz-period-type" types type :on-change on-type)
                       (jetpacs-spacer :width 8)))
               (list (funcall wheel "oz-period-value"
                              (mapcar #'number-to-string (number-sequence 1 (max 100 (string-to-number value))))
                              value)
                     (jetpacs-spacer :width 8)
                     (funcall wheel "oz-period-unit" (mapcar #'cdr orgzly-ui-period-units)
                              (alist-get unit orgzly-ui-period-units "Day" nil #'equal))
                     (jetpacs-spacer :weight 1))))
       :height 180)
      (jetpacs-spacer :height 16)
      (cond
       ((null (plist-get kind :hints)) (jetpacs-spacer :height 0))
       ;; The hint follows the type wheel as it turns, as Revived's does.
       ((cdr types)
        (jetpacs-design-state-switch
         "oz-period-type"
         (cl-mapcar (lambda (ty text) (cons ty (funcall hint text)))
                    types (plist-get kind :hints))))
       (t (funcall hint (car (plist-get kind :hints)))))
      (jetpacs-spacer :height 12)
      (jetpacs-with-attrs
       (jetpacs-row
        (jetpacs-spacer :weight 1)
        (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
        (jetpacs-spacer :width 8)
        (orgzly-ui--dialog-button "OK" (jetpacs-dialog-submit
                                        :capture-fields (append (and (cdr types) '("oz-period-type"))
                                                                '("oz-period-value" "oz-period-unit"))))
        (jetpacs-spacer :width 12)
        :align "center")
       :height 48)
      (jetpacs-spacer :height 4)))))

(defun orgzly-ui-modified-dialog ()
  "Revived's \"Note modified\" prompt: Cancel apart, then Discard and Save.
Discard and Save submit their names; Cancel dismisses."
  (orgzly-ui-styled
   "orgzly.dialog"
   (jetpacs-column
    (jetpacs-spacer :height 18.13)
    (orgzly-ui-styled "orgzly.dialog.title"
                      (orgzly-ui-styled-text "orgzly.t.dialog-title" "Note modified"))
    (jetpacs-spacer :height 16)
    (orgzly-ui-styled "orgzly.dialog.title"
                      (orgzly-ui-styled-text "orgzly.t.dialog-message" "Save changes?"))
    (jetpacs-spacer :height 43.37)
    (jetpacs-with-attrs
     (jetpacs-row
      (jetpacs-spacer :width 24.5)
      (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
      (jetpacs-spacer :weight 1)
      (orgzly-ui--dialog-button "Discard" (jetpacs-dialog-submit :value "discard"))
      (jetpacs-spacer :width 8)
      (orgzly-ui--dialog-button "Save" (jetpacs-dialog-submit :value "save"))
      (jetpacs-spacer :width 24)
      :align "center")
     :height 48)
    (jetpacs-spacer :height 13.87))))

(defconst orgzly-ui-ts-checks '(time-on end-on repeater-on delay-on)
  "The timestamp dialog's checkboxes, each a part of `orgzly-state-ts'.")

(defun orgzly-ui-ts-check-id (check)
  "The timestamp dialog's checkbox node id for CHECK, a symbol."
  (format "oz-ts-%s" check))

(defun orgzly-ui--ts-day (date)
  "DATE (YYYY-MM-DD) as Revived names a day: \"Sun, Oct 25\"."
  (let ((time (encode-time (parse-time-string (concat date " 00:00:00")))))
    (concat (format-time-string "%a, %b %-d" time)
            (unless (equal (substring date 0 4) (format-time-string "%Y"))
              (format-time-string ", %Y" time)))))

(defun orgzly-ui--ts-clock (hm)
  "HM (HH:MM) on the device's clock: \"10:11 AM\", or \"10:11\" on h23."
  (format-time-string (if (equal (plist-get (jetpacs-locale) :hour_cycle) "h23") "%H:%M" "%-I:%M %p")
                      (encode-time (parse-time-string (concat "2000-01-01 " hm ":00")))))

(defun orgzly-ui--ts-title (ts)
  "The heading of timestamp dialog TS: its day and every checked part."
  (string-join
   (delq nil (list (orgzly-ui--ts-day (plist-get ts :date))
                   (and (plist-get ts :time-on)
                        (concat (orgzly-ui--ts-clock (plist-get ts :time))
                                (and (plist-get ts :end-on)
                                     (concat "–" (orgzly-ui--ts-clock (plist-get ts :end))))))
                   (and (plist-get ts :repeater-on) (plist-get ts :repeater))
                   (and (plist-get ts :delay-on) (plist-get ts :delay))))
   " "))

(defun orgzly-ui--ts-part (part &optional more)
  "The timestamp dialog's action setting PART, with MORE args."
  (jetpacs-action "orgzly.ts.part" :args (append (list :part part) more) :when-offline "drop"
                  :capture-fields (mapcar #'orgzly-ui-ts-check-id orgzly-ui-ts-checks)))

(defun orgzly-ui--ts-cell (node)
  "NODE, a 40dp button, in its 48dp cell: Material's 4dp insets."
  (jetpacs-with-attrs (jetpacs-box (jetpacs-with-attrs node :fill_fraction 1))
                      :pad '(:vertical 4) :weight 1))

(defun orgzly-ui--ts-pill (label action)
  "A tonal pill showing LABEL on one line, running ACTION."
  (orgzly-ui--ts-cell
   (jetpacs-design-pressable
    '("orgzly.ts.pill") action
    ;; The label fills the pill, so its centred alignment centres it.
    (list (jetpacs-with-attrs (orgzly-ui-styled "orgzly.t.ts-pill"
                                                (jetpacs-with-attrs (jetpacs-text label :max-lines 1)
                                                                    :fill_fraction 1))
                              :weight 1)))))

(defun orgzly-ui--ts-check (check ts &optional on-change disabled)
  "The 48dp checkbox for CHECK of TS; ON-CHANGE runs when it flips.
DISABLED greys it out, as Revived's end time box is without a time."
  (jetpacs-with-attrs
   (jetpacs-box (jetpacs-checkbox (orgzly-ui-ts-check-id check)
                                  :checked (if (plist-get ts (intern (format ":%s" check))) t :json-false)
                                  :on-change on-change
                                  :enabled (if disabled :json-false t))
                :alignment "center")
   :width 48))

(defun orgzly-ui--ts-label (text)
  "TEXT as a centred 12sp caps label over a column of the dialog."
  (jetpacs-with-attrs (orgzly-ui-styled-text "orgzly.t.ts-label" (upcase text)) :weight 1))

(defun orgzly-ui-timestamp-dialog (ts)
  "Revived's dialog_timestamp.xml for TS, the `orgzly-state-ts' plist.
The day, times, repeater and delay as tonal buttons, each optional part
behind a checkbox; Clear, Cancel and Set below.  Picks and shortcuts
dispatch `orgzly.ts.part', which shows the dialog again."
  (let* ((orgzly-field-scope nil)
         (gap (jetpacs-spacer :width 48))
         (time-on (plist-get ts :time-on)))
    (orgzly-ui-styled
     "orgzly.dialog"
     (jetpacs-column
      (orgzly-ui-styled
       "orgzly.ts.head"
       (jetpacs-with-attrs
        (jetpacs-box (orgzly-ui-styled-text "orgzly.t.ts-title" (orgzly-ui--ts-title ts))
                     :alignment "center")
        :height 36 :fill_fraction 1))
      (orgzly-ui-styled
       "orgzly.ts.body"
       (jetpacs-column
        (jetpacs-row (orgzly-ui--ts-label "Day"))
        (jetpacs-row (orgzly-ui--ts-cell
                      (jetpacs-date-button (orgzly-ui--ts-day (plist-get ts :date))
                                           (orgzly-ui--ts-part "date")
                                           :value (plist-get ts :date))))
        (jetpacs-row (orgzly-ui--ts-pill "Today" (orgzly-ui--ts-part "today"))
                     (jetpacs-spacer :width 8)
                     (orgzly-ui--ts-pill "Tomorrow" (orgzly-ui--ts-part "tomorrow"))
                     (jetpacs-spacer :width 8)
                     (orgzly-ui--ts-pill "Next Week" (orgzly-ui--ts-part "week")))
        (jetpacs-spacer :height 8)
        (jetpacs-row (orgzly-ui--ts-label "Time") gap (orgzly-ui--ts-label "End time") gap)
        (jetpacs-row
         (orgzly-ui--ts-cell (jetpacs-time-button (orgzly-ui--ts-clock (plist-get ts :time))
                                                  (orgzly-ui--ts-part "time")
                                                  :value (plist-get ts :time)))
         ;; Unticking the time disables the end time, so it shows again.
         (orgzly-ui--ts-check 'time-on ts (orgzly-ui--ts-part "checks"))
         (orgzly-ui--ts-cell (jetpacs-time-button (orgzly-ui--ts-clock (plist-get ts :end))
                                                  (orgzly-ui--ts-part "end")
                                                  :value (plist-get ts :end)
                                                  :enabled (if time-on t :json-false)))
         (orgzly-ui--ts-check 'end-on ts nil (not time-on))
         :align "center")
        (jetpacs-spacer :height 8)
        (jetpacs-row (orgzly-ui--ts-label "Repeater") gap (jetpacs-spacer :weight 1) gap)
        (jetpacs-row (orgzly-ui--ts-pill (plist-get ts :repeater) (orgzly-ui--ts-part "repeater"))
                     (orgzly-ui--ts-check 'repeater-on ts)
                     (jetpacs-spacer :weight 1) gap
                     :align "center")
        (jetpacs-spacer :height 8)
        (jetpacs-row (orgzly-ui--ts-label "Delay") gap (jetpacs-spacer :weight 1) gap)
        (jetpacs-row (orgzly-ui--ts-pill (plist-get ts :delay) (orgzly-ui--ts-part "delay"))
                     (orgzly-ui--ts-check 'delay-on ts)
                     (jetpacs-spacer :weight 1) gap
                     :align "center")))
      ;; From a 600dp smallest width Revived's buttons sit 16dp lower.
      (jetpacs-spacer :height (if (orgzly-design-sw600-p (jetpacs-window)) 16 0))
      (jetpacs-with-attrs
       (jetpacs-row
        (jetpacs-spacer :width 24)
        (orgzly-ui--dialog-button "Clear" (jetpacs-dialog-submit :value "clear"))
        (jetpacs-spacer :weight 1)
        (orgzly-ui--dialog-button "Cancel" (jetpacs-dialog-dismiss))
        (jetpacs-spacer :width 8)
        (orgzly-ui--dialog-button "Set" (jetpacs-dialog-submit
                                         :value "set"
                                         :capture-fields (mapcar #'orgzly-ui-ts-check-id
                                                                 orgzly-ui-ts-checks)))
        (jetpacs-spacer :width 24)
        :align "center")
       :height 48)
      (jetpacs-spacer :height 13.87)))))

(defun orgzly-ui--dialog-button (label action)
  "An AlertDialog text button: LABEL in the accent color, running ACTION."
  (jetpacs-design-pressable
   '("orgzly.dialog.button") action
   ;; Material's text button: the 12dp-padded label centred in at least 64dp.
   (list (jetpacs-with-attrs
          (jetpacs-box (jetpacs-with-attrs
                        (jetpacs-box (orgzly-ui-styled-text "orgzly.t.dialog-button" label))
                        :pad '(:horizontal 12))
                       :alignment "center")
          :min_width 64 :height 48))))

(defun orgzly-ui-note-form (route)
  "Build the note screen of ROUTE as Revived's fragment_note.xml.
Breadcrumbs, a chromeless title, the collapsible metadata rows and the
content.  Title, tags, content and a new property are captured fields;
state, priority and planning come from pickers into the draft, and the
toolbar's Done saves everything at once."
  (let* ((id (plist-get route :id))
         (entry (and id (ignore-errors (orgzly-state-resolve id))))
         (book (or (alist-get 'book entry) (plist-get route :book)))
         (parent (and (plist-get route :parent-id)
                      (ignore-errors (orgzly-state-resolve (plist-get route :parent-id)))))
         (scope orgzly-field-scope)
         (folds orgzly-state-note-folds))
    (if (and id (null entry))
        (list (orgzly-ui-styled "orgzly.preface"
                                (orgzly-ui-styled-text "orgzly.t.preface"
                                                       "This note does not exist anymore.")))
      (let ((value (lambda (field) (orgzly-ui-note-value field entry)))
            (remove (lambda (field label)
                      (orgzly-ui--meta-button "close" label
                                              (orgzly-ui-action "orgzly.note.clear"
                                                                (list :field field :scope scope))))))
        (append
         (list (orgzly-ui--breadcrumbs book entry parent)
               (if (or (null entry) (orgzly-ui-note-editing-p "title"))
                   (orgzly-ui--input 'title "oz-title" (orgzly-ui-note-text "title" entry) "Title" nil t)
                 (orgzly-ui--note-text 'title "title" (alist-get 'title entry) "Title" id))
               (orgzly-ui--note-divider)
               (orgzly-ui--section-header "info" "metadata" (memq 'metadata folds)))
         (unless (memq 'metadata folds)
           (append
            (delq
             nil
             (list
             (let ((tags (orgzly-ui-note-tags entry)))
               (when (orgzly-ui--metadata-shown-p 'tags (not (string-blank-p tags)))
               (orgzly-ui--meta-row
                "label" (orgzly-ui--input 'value "oz-tags" tags "Tags" nil nil (orgzly-ui-all-tags))
                (orgzly-ui--tags-buttons scope)
                "tags")))
             (when (orgzly-ui--metadata-shown-p 'state (not (string-empty-p (funcall value "state"))))
             (orgzly-ui--meta-row
              "flag_filled" (orgzly-ui--meta-value "state" (funcall value "state") "State" scope)
              (unless (string-empty-p (funcall value "state")) (funcall remove "state" "Clear state"))
              "state"))
             (when (orgzly-ui--metadata-shown-p 'priority (not (string-empty-p (funcall value "priority"))))
             (orgzly-ui--meta-row
              "star_border" (orgzly-ui--meta-value "priority" (funcall value "priority") "Priority" scope)
              (unless (string-empty-p (funcall value "priority")) (funcall remove "priority" "Clear priority"))
              "priority"))
             (when (orgzly-ui--metadata-shown-p 'scheduled (not (string-empty-p (funcall value "scheduled"))))
             (orgzly-ui--meta-row
              "today_filled" (orgzly-ui--meta-value "scheduled" (orgzly-ui--shown-time (funcall value "scheduled"))
                                                    "Scheduled time" scope (funcall value "scheduled"))
              (unless (string-empty-p (funcall value "scheduled")) (funcall remove "scheduled" "Clear scheduled time"))
              "scheduled"))
             (when (orgzly-ui--metadata-shown-p 'deadline (not (string-empty-p (funcall value "deadline"))))
             (orgzly-ui--meta-row
              "alarm" (orgzly-ui--meta-value "deadline" (orgzly-ui--shown-time (funcall value "deadline"))
                                             "Deadline time" scope (funcall value "deadline"))
              (unless (string-empty-p (funcall value "deadline")) (funcall remove "deadline" "Clear deadline time"))
              "deadline"))))
            (when-let* ((closed (alist-get 'closed entry)))
              (list (orgzly-ui--meta-row "check_circle_outline"
                                         (orgzly-ui-styled-text "orgzly.t.value" (orgzly-ui--time closed))
                                         nil "closed")))
            (when (orgzly-ui--metadata-shown-p 'properties (alist-get 'properties entry))
            (cl-loop for (name . val) in (alist-get 'properties entry) for n from 0
                     collect (orgzly-ui--meta-row
                              "abc" (jetpacs-row (jetpacs-with-attrs (orgzly-ui-styled-text "orgzly.t.value" name)
                                                                     :weight 1)
                                                 (jetpacs-spacer :width 8)
                                                 (jetpacs-with-attrs (orgzly-ui-styled-text "orgzly.t.value" val)
                                                                     :weight 1)
                                                 :align "center")
                              nil (format "prop-%d" n))))
            (when (orgzly-ui--metadata-shown-p 'properties (alist-get 'properties entry))
            (list (orgzly-ui--meta-row
                   "abc" (jetpacs-row (jetpacs-with-attrs
                                       (orgzly-ui--input 'value "oz-prop-name" "" "Property name") :weight 1)
                                      (jetpacs-spacer :width 8)
                                      (jetpacs-with-attrs
                                       (orgzly-ui--input 'value "oz-prop-value" "" "Property value") :weight 1)
                                      :align "center")
                   nil "prop-new")))))
         (list (orgzly-ui--note-divider)
               (orgzly-ui--section-header "notes" "content" (memq 'content folds)))
         (unless (memq 'content folds)
           (list (cond ((not (or (null entry) (orgzly-ui-note-editing-p "content")))
                        (orgzly-ui--note-text 'content "content" (or (orgzly-ui-note-text "content" entry) "")
                                              "Content" id))
                       ((orgzly-ui-content-synced-p)
                        (orgzly-ui--content-editor (orgzly-ui-note-text "content" entry) (and entry t)))
                       (t (orgzly-ui--input 'content "oz-content" (orgzly-ui-note-text "content" entry)
                                            "Content" t)))))
         (list (orgzly-ui--note-divider)))))))

(defun orgzly-ui--metadata-shown-p (kind set)
  "Whether the note shows metadata row KIND, which has a value when SET.
Revived's Metadata submenu: everything, or the selected rows plus, while
\"Always show set\" is on, every row with a value."
  (or (eq orgzly-state-metadata-visibility 'all)
      (memq kind orgzly-selected-metadata)
      (and orgzly-state-metadata-always-set set t)))

(defun orgzly-ui-book-form (route)
  "Build notebook metadata form for ROUTE, with explicit destructive confirmation."
  (let* ((id (plist-get route :id)) (book (and id (ignore-errors (orgzly-state-resolve id)))))
    (if (and id (null book)) (list (jetpacs-text "This notebook changed. Reopen its details."))
      (append
       (list (orgzly-ui-field "oz-name" "Notebook filename" (alist-get 'name book))
             (orgzly-ui-field "oz-preface" "Preface, #+TITLE and #+FILETAGS" (alist-get 'preface book) t)
             (jetpacs-button "Save notebook" (orgzly-ui-action "orgzly.book.save" (and id (list :id id)) nil
                                                               '("oz-name" "oz-preface"))))
       (when id (list (jetpacs-button "Delete notebook"
                                      (orgzly-ui-action "orgzly.book.delete" (list :id id)
                                                        "Delete this notebook and every note in it?") :variant "text")))))))

(defun orgzly-ui--note-args (route)
  "The save and operation arguments of note ROUTE.
A new note names its notebook and the note it is placed against: a
parent, or a sibling it goes below or, with `:before', above."
  (if (plist-get route :id) (list :id (plist-get route :id))
    (append (list :book-id (plist-get route :book-id) :parent-id (plist-get route :parent-id))
            (when (plist-get route :sibling-id)
              (list :sibling-id (plist-get route :sibling-id)
                    :before (and (plist-get route :before) t))))))

(defun orgzly-ui--overflow-items (route)
  "Revived's overflow items for ROUTE's screen, then Orgzly Native's own.
Orgzly Native's items follow only while `orgzly-menu-extras' is non-nil."
  (let* ((kind (plist-get route :kind)) (id (plist-get route :id))
         (scope (orgzly-field-scope route))
         (content (let ((orgzly-field-scope scope))
                    (and (orgzly-ui-note-editing-p "content") (not (orgzly-ui-content-synced-p))
                         '("oz-content"))))
         (item (lambda (label name &optional args confirm fields)
                 (jetpacs-menu-item label (orgzly-ui-action name args confirm fields))))
         (common (list (funcall item "Sync" "orgzly.refresh")
                       (funcall item "Settings" "orgzly.open" '(:view "settings")))))
    (append
     (pcase kind
       ('books (cons (funcall item "Import Org file" "orgzly.book.import") common))
       ('book
        (append
         (when-let* ((book (orgzly-state-book (plist-get route :book))))
           (list (funcall item "Edit preface" "orgzly.book.edit" (list :id (alist-get 'id book)))
                 (funcall item "Tags" "orgzly.book.tags" (list :id (alist-get 'id book)))))
         common))
       ((or 'note 'new-note)
        (let ((submenus (jetpacs-feature-advertised-p "menu.submenu" :app))
              (clock-op (lambda (label op icon)
                          (jetpacs-menu-item label (orgzly-ui-action
                                                    "orgzly.note.content-op"
                                                    (append (list :scope scope :op op) (and id (list :id id)))
                                                    nil content)
                                             :icon icon))))
          (append
           (list (if submenus
                     (jetpacs-menu-item "Clock" nil :items
                                        (list (funcall clock-op "Clock in" "clock-in" "hourglass_top")
                                              (funcall clock-op "Clock out" "clock-out" "hourglass_bottom")
                                              (funcall clock-op "Clock cancel" "clock-cancel"
                                                       "hourglass_disabled")))
                   (funcall item "Clock" "orgzly.note.clock" (append (list :scope scope) (and id (list :id id)))
                            nil content))
                 (if submenus
                     (jetpacs-menu-item "Metadata" nil :items (orgzly-ui--metadata-items))
                   (funcall item "Metadata" "orgzly.note.section" '(:section "metadata"))))
           (when (eq kind 'note)
             (list (funcall item "Delete" "orgzly.note.operate" (list :id id :operation "delete")
                            "Delete this note and its children?")
                   (if submenus
                       (jetpacs-menu-item "Share" nil :items (orgzly-ui--share-items id))
                     (funcall item "Share" "orgzly.note.share" (list :id id)))))
           common
           (list (funcall item "Sort note" "orgzly.note.content-op"
                          (append (list :scope scope :op "sort") (and id (list :id id)))
                          nil content)))))
       (_ common))
     (when orgzly-menu-extras
       (append
        (when (eq kind 'note)
          (mapcar (lambda (op) (funcall item (car op) "orgzly.note.operate"
                                        (list :id id :operation (cdr op))))
                  '(("Promote" . "promote") ("Demote" . "demote") ("Move up" . "up")
                    ("Move down" . "down") ("Copy" . "copy") ("Paste below" . "paste"))))
        (when (eq kind 'note)
          (list (funcall item "New child note" "orgzly.note.child" (list :id id))))
        (list (funcall item (if orgzly-state-dark "Light theme" "Dark theme") "orgzly.theme")
              ;; Orgzly owns its whole chrome, so the way back to Jetpacs lives here.
              (jetpacs-menu-item "Jetpacs apps"
                                 (jetpacs-shell-open-surface-action "app:jetpacs.app-store"))))))))

(defun orgzly-ui--metadata-items ()
  "Revived's Metadata submenu: Show all or Show selected, then Always show set."
  (let ((radio (lambda (label show)
                 (jetpacs-menu-item label (orgzly-ui-action "orgzly.note.metadata" (list :show show))
                                    :checked (if (equal (symbol-name orgzly-state-metadata-visibility) show)
                                                 t :json-false)
                                    :checked-icon "radio_button_checked"
                                    :unchecked-icon "radio_button_unchecked"))))
    (list (funcall radio "Show all" "all")
          (funcall radio "Show selected" "selected")
          (jetpacs-menu-item "Always show set" (orgzly-ui-action "orgzly.note.metadata" '(:always-set t))
                             :checked (if orgzly-state-metadata-always-set t :json-false)
                             :checked-icon "check_box" :unchecked-icon "check_box_outline_blank"))))

(defun orgzly-ui--share-items (id)
  "Revived's Share submenu for note ID: the note as Org, its title, its content.
An empty part shares nothing, as Revived's does."
  (let ((entry (ignore-errors (orgzly-state-resolve id t))))
    (mapcar (lambda (part)
              (let ((text (and entry (funcall (nth 2 part) entry))))
                (jetpacs-menu-item (car part)
                                   (if (and text (not (string-empty-p text)))
                                       (jetpacs-share text)
                                     (orgzly-ui-action "orgzly.refresh"))
                                   :icon (cadr part))))
            (list (list "Share note" "subject" #'orgzly--note-org)
                  (list "Share title" "title" (lambda (e) (or (alist-get 'title e) "")))
                  (list "Share content" "article_filled" (lambda (e) (or (alist-get 'content e) "")))))))

(defun orgzly-ui--overflow (route)
  "The toolbar's overflow menu on ROUTE's screen."
  (orgzly-design-overflow-scope
   (orgzly-ui-dark) (jetpacs-window)
   (jetpacs-menu (orgzly-ui--overflow-items route) :icon "more_vert")))

(defun orgzly-ui-selecting-p (route)
  "Non-nil when ROUTE's outline is in Revived's selection mode."
  (and (eq (plist-get route :kind) 'book) orgzly-state-selected t))

(defun orgzly-ui--select-action (op &optional args confirm)
  "The selection's OP as an action, with ARGS and an optional CONFIRM."
  (orgzly-ui-action "orgzly.select.op" (append (list :op op) args) confirm))

(defun orgzly-ui--place-menu (icon label op)
  "A menu under ICON offering above, under and below for OP; LABEL names it."
  (ignore label)
  (jetpacs-menu
   (list (jetpacs-menu-item "Above" (orgzly-ui--select-action op '(:place "above")) :icon "add_above")
         (jetpacs-menu-item "Under" (orgzly-ui--select-action op '(:place "under")) :icon "add_under")
         (jetpacs-menu-item "Below" (orgzly-ui--select-action op '(:place "below")) :icon "add_below"))
   :icon icon))

(defun orgzly-ui--selection-actions ()
  "Revived's book_cab_top: cut, copy, paste and move, or the move arrows.
Paste needs exactly one selected note; Delete waits in the overflow."
  (let ((text (orgzly-design-color 'text (orgzly-ui-dark)))
        (one (= (length orgzly-state-selected) 1)))
    (if orgzly-state-moving
        (mapcar (lambda (b)
                  (jetpacs-icon-button (nth 0 b) (orgzly-ui--select-action (nth 1 b))
                                       :content-description (nth 2 b) :color text))
                '(("arrow_back" "promote" "Move left") ("arrow_downward" "down" "Move down")
                  ("arrow_upward" "up" "Move up") ("arrow_forward" "demote" "Move right")))
      (append
       (list (jetpacs-icon-button "content_cut" (orgzly-ui--select-action "cut")
                                  :content-description "Cut" :color text)
             (jetpacs-icon-button "content_copy" (orgzly-ui--select-action "copy")
                                  :content-description "Copy" :color text))
       (when one (list (orgzly-ui--place-menu "content_paste" "Paste" "paste")))
       (list (jetpacs-icon-button "swap_vert" (orgzly-ui-action "orgzly.select.move")
                                  :content-description "Move" :color text)
             (jetpacs-spacer :width 4)
             (orgzly-ui-styled
              "orgzly.overflow"
              (orgzly-design-overflow-scope
               (orgzly-ui-dark) (jetpacs-window)
               (jetpacs-menu
                (list (jetpacs-menu-item
                       "Delete" (orgzly-ui--select-action
                                 "delete" nil
                                 (format "Delete %d note%s?" (length orgzly-state-selected)
                                         (if one "" "s")))))
                :icon "more_vert"))))))))

(defun orgzly-ui-selection-bottom-bar ()
  "Revived's book_cab_bottom: refile, schedule, deadline, state, done, new note.
The BottomAppBar ends its actions 8dp from the edge; New note needs
exactly one selected note."
  (let ((text (orgzly-design-color 'text (orgzly-ui-dark)))
        (one (= (length orgzly-state-selected) 1)))
    (orgzly-ui-styled
     "orgzly.bottom-bar"
     (apply #'jetpacs-row
            (append
             (delq nil
                   (list
                    (jetpacs-with-attrs (jetpacs-spacer) :weight 1)
                    (jetpacs-icon-button "move_to_inbox" (orgzly-ui--select-action "refile")
                                         :content-description "Refile" :color text)
                    (jetpacs-icon-button "today_filled" (orgzly-ui--select-action "scheduled")
                                         :content-description "Schedule" :color text)
                    (jetpacs-icon-button "alarm" (orgzly-ui--select-action "deadline")
                                         :content-description "Deadline" :color text)
                    (jetpacs-icon-button "flag_filled" (orgzly-ui--select-action "state")
                                         :content-description "State" :color text)
                    (jetpacs-icon-button "check_circle_outline" (orgzly-ui--select-action "toggle")
                                         :content-description "Done" :color text)
                    (when one (orgzly-ui--place-menu "add" "New note" "new"))
                    (jetpacs-spacer :width 8)))
             '(:align "center"))))))

(defun orgzly-ui--toolbar-actions (route)
  "The top app bar's trailing actions on ROUTE's screen, as Revived places them.
The 48dp action buttons abut; the overflow sits 4dp after them."
  (if (orgzly-ui-selecting-p route) (orgzly-ui--selection-actions)
  (let ((text (orgzly-design-color 'text (orgzly-ui-dark)))
        (kind (plist-get route :kind)))
    (append
     (when (eq kind 'book)
       (list (jetpacs-icon-button "unfold_more" (orgzly-ui-action "orgzly.fold.all")
                                  :content-description "Fold/Unfold All" :color text)))
     (when (eq kind 'searches)
       (list (jetpacs-icon-button "help_outline" (orgzly-ui-action "orgzly.search.help")
                                  :content-description "Help" :color text)))
     (when (and (memq kind '(books book searches)) (not orgzly-state-searching))
       (list (jetpacs-icon-button "search" (orgzly-ui-action "orgzly.search.open")
                                  :content-description "Search" :color text)))
     (when (memq kind '(note new-note))
       (list (jetpacs-icon-button "check" (orgzly-ui-action "orgzly.note.save"
                                                            (orgzly-ui--note-args route)
                                                            nil (orgzly-ui-note-captures route))
                                  :content-description "Done" :color text)))
     (when (memq kind '(note new-note))
       (let ((orgzly-field-scope (orgzly-field-scope route)))
         ;; Revived shows "insert timestamp" while title or content is edited.
         (when (or (eq kind 'new-note) (orgzly-ui-note-editing-p "title")
                   (orgzly-ui-note-editing-p "content"))
           (list (jetpacs-icon-button "today"
                                      (orgzly-ui-action "orgzly.note.timestamp"
                                                        (list :scope orgzly-field-scope) nil
                                                        (seq-filter (lambda (f) (member f '("oz-title" "oz-content")))
                                                                    (orgzly-ui-note-captures route)))
                                      :content-description "Insert timestamp" :color text)))))
     (unless (eq kind 'settings)
       (list (jetpacs-spacer :width 4)
             (orgzly-ui-styled "orgzly.overflow" (orgzly-ui--overflow route))))))))

(defconst orgzly-ui-settings
  '(("look" "Look & Feel" "palette_filled"
     ("Color scheme"
      (theme "Theme" orgzly-state-dark "Dark" "Light")))
    ("notes" "Notes & Notebooks" "library_books_filled"
     ("Notebooks"
      (string "Default notebook" orgzly-default-book)
      (string "Directory" orgzly-directory))
     ("New notes"
      (boolean "Add to the top" orgzly-new-note-prepend
               "New notes are added above their siblings")
      (boolean "Created time" orgzly-new-note-created-property
               "Stamp new notes with their creation time")
      (boolean "Add ID" orgzly-new-note-add-id "Give new notes an :ID: property")
      (boolean "Scheduled today" orgzly-new-note-scheduled "Schedule new notes for today"))
     ("Agenda"
      (boolean "Group scheduled with today" orgzly-agenda-group-scheduled-with-today
               "Overdue scheduled notes are listed under today")))
    ("notifications" "Notifications" "notifications"
     (nil
      (boolean "Reminders" orgzly-reminders-enabled "Display reminders for notes")))
    ("reminders" "Reminders" "alarm"
     (nil
      (boolean "Scheduled" orgzly-reminders-scheduled
               "Display notification for notes with scheduled time")
      (boolean "Deadlines" orgzly-reminders-deadline
               "Display notification for notes with deadline time")
      (boolean "Events" orgzly-reminders-event
               "Display notification for notes with event time")
      (string "Daily reminder time" orgzly-reminders-daily-time)))
    ("app" "App" "info"
     (nil
      (about "Orgzly Native" "Plain Org files in Emacs, presented by Jetpacs"))))
  "The settings screens: (PAGE TITLE ICON (HEADER ITEM...)...).
An item is (boolean TITLE VARIABLE SUMMARY), (string TITLE VARIABLE),
\(theme TITLE VARIABLE ON OFF) or (about TITLE SUMMARY).  Pages follow
Revived's categories, with the options Orgzly Native has.")

(defun orgzly-ui--switch (on)
  "An AppCompat switch, ON or off: a 25dp track under a 19.5dp thumb."
  (progn
    (jetpacs-with-attrs
     (jetpacs-box
      (jetpacs-with-attrs
       (orgzly-ui-styled (if on "orgzly.switch.track-on" "orgzly.switch.track-off")
                         (jetpacs-spacer :width 25 :height 13.5))
       :pad '(:start 11))
      (jetpacs-with-attrs
       (orgzly-ui-styled (if on "orgzly.switch.thumb-on" "orgzly.switch.thumb-off")
                         (jetpacs-spacer :width 19.5 :height 19.5))
       :pad (list :start (if on 23.8 3.9)))
      :alignment "center_start")
     :width 47 :height 27 :key (if on "switch-on" "switch-off"))))

(defun orgzly-ui--pref (title summary action &optional trailing key toggled)
  "One preference row running ACTION: TITLE over SUMMARY at 72dp.
An optional TRAILING widget sits at the end; KEY overrides the identity.
TOGGLED, t or `:json-false', makes the row a toggle for accessibility."
  (jetpacs-with-attrs
   (jetpacs-design-pressable
    '("orgzly.pref") action
    (list (jetpacs-with-attrs
           (jetpacs-row
            (jetpacs-with-attrs
             (apply #'jetpacs-column
                    (delq nil (list (orgzly-ui-styled-text "orgzly.t.pref-title" title)
                                    (and summary (orgzly-ui-styled-text "orgzly.t.summary" summary)))))
             :weight 1)
            (or trailing (jetpacs-spacer :width 0))
            :align "center")
           :weight 1))
    :toggled toggled)
   :key (or key (jetpacs-wire-id "pref" title))))

(defun orgzly-ui-settings (route)
  "Revived's Settings: the category list, or ROUTE's `:page' of options."
  (let ((page (assoc (plist-get route :page) orgzly-ui-settings)))
    (if (null page)
        (mapcar (lambda (p)
                  (jetpacs-with-attrs
                   (jetpacs-design-pressable
                    '("orgzly.pref.category") (orgzly-ui-action "orgzly.open" (list :view "settings" :page (car p)))
                    (list (jetpacs-with-attrs
                           (jetpacs-row
                            (jetpacs-spacer :width 16)
                            (jetpacs-icon (nth 2 p) :size 24
                                          :color (orgzly-design-color 'text (orgzly-ui-dark)))
                            (jetpacs-spacer :width 32)
                            (orgzly-ui-styled-text "orgzly.t.pref-title" (nth 1 p))
                            :align "center")
                           :weight 1)))
                   :key (concat "cat-" (car p))))
                orgzly-ui-settings)
      (cl-loop
       for (header . items) in (nthcdr 3 page) for n from 0
       append
       (append
        (when (and header (> n 0))
          (list (jetpacs-with-attrs (orgzly-ui--list-divider) :key (format "sec-div-%d" n))))
        (when header
          (list (jetpacs-with-attrs
                 (orgzly-ui-styled "orgzly.pref.header"
                                   (orgzly-ui-styled-text "orgzly.t.pref-header" header))
                 :key (format "header-%d" n))))
        (mapcar
         (lambda (item)
           (pcase item
             (`(boolean ,title ,var ,summary)
              (orgzly-ui--pref title summary
                               (orgzly-ui-action "orgzly.setting" (list :name (symbol-name var)))
                               (orgzly-ui--switch (symbol-value var)) nil
                               (if (symbol-value var) t :json-false)))
             (`(string ,title ,var)
              (orgzly-ui--pref title (format "%s" (symbol-value var))
                               (orgzly-ui-action "orgzly.setting" (list :name (symbol-name var)))))
             (`(theme ,title ,var ,on ,off)
              (orgzly-ui--pref title (if (symbol-value var) on off)
                               (orgzly-ui-action "orgzly.theme")))
             (`(about ,title ,summary)
              (orgzly-ui--pref title summary (orgzly-ui-action "orgzly.open" '(:view "settings"))))))
         items))))))

(defun orgzly-ui--search-field ()
  "The toolbar's SearchView: a search icon and the query field."
  (jetpacs-row
   (jetpacs-icon "search" :size 24 :color (orgzly-design-color 'text (orgzly-ui-dark)))
   (jetpacs-spacer :width 8)
   (jetpacs-with-attrs
    (orgzly-ui--field-scope
     'search (jetpacs-text-input (orgzly-field-id "oz-query") :value "" :hint "Search notes"
                                 :single-line t :autofocus t :max-length 512
                                 :on-submit (orgzly-ui-action "orgzly.search.run"
                                                              (list :scope orgzly-field-scope)
                                                              nil '("oz-query"))))
    :weight 1)
   :align "center"))

(defun orgzly-ui-screen (route &optional back)
  "Render ROUTE and BACK from explicit app state; perform no reads or mutations."
  (let* ((kind (plist-get route :kind))
         (orgzly-field-scope (orgzly-field-scope route))
         (title (pcase kind ('books "Notebooks")
                       ('book (or (alist-get 'title (orgzly-state-book (plist-get route :book)))
                                  (plist-get route :book)))
                       ('note "Note") ('new-note "New note") ('searches "Searches")
                       ('query (or (plist-get route :name) (plist-get route :query)))
                       ('settings (or (nth 1 (assoc (plist-get route :page) orgzly-ui-settings))
                                      "Settings"))
                       (_ "Orgzly")))
         ;; Drawer destinations wear the hamburger, as in Revived; forms
         ;; opened from them keep the back arrow.
         (selecting (orgzly-ui-selecting-p route))
         (top (and (not selecting) (memq kind '(books book searches query note new-note))))
         (searching (and orgzly-state-searching (memq kind '(books book searches query))))
         (nodes
          (pcase kind
            ('books (orgzly-ui-books)) ('book (orgzly-ui-book (plist-get route :book)))
            ((or 'note 'new-note) (orgzly-ui-note-form route))
            ((or 'new-book 'book-edit) (orgzly-ui-book-form route))
            ('searches (orgzly-ui-searches))
            ('query (orgzly-ui-query route))
            ('save-search (list (orgzly-ui-field "oz-name" "Search name" "")
                                (orgzly-ui-field "oz-query" "Dotted query" orgzly-state-query)
                                (jetpacs-button "Save search" (orgzly-ui-action "orgzly.search.save" nil nil '("oz-name" "oz-query")))))
            ('settings (orgzly-ui-settings route))
            (_ (list (jetpacs-text "Unknown Orgzly screen.")))))
         (body (cond
                ;; The outline's rows are their own lazy column.
                ((eq kind 'book) (apply #'jetpacs-column nodes))
                (t (let ((list-node (apply #'jetpacs-lazy-column nodes)))
                     (if (memq kind '(new-book book-edit save-search))
                         (jetpacs-with-attrs list-node :padding 16)
                       list-node)))))
         (title (if selecting (number-to-string (length orgzly-state-selected)) title))
         (screen (jetpacs-chrome-screen
                  title body
                  ;; Searching, the arrow closes the field as Revived's does;
                  ;; selecting, it leaves move mode, then the selection.
                  :back (cond (selecting (orgzly-ui-action "orgzly.select.back"))
                              (searching (orgzly-ui-action "orgzly.search.close"))
                              (t (unless top back)))
                  ;; The hamburger stays, and back still returns below.
                  ;; A note asks before an edit is left, as Revived's does.
                  ;; A remote back gesture names the surface it keeps.
                  :on-back (cond (selecting (orgzly-ui-action "orgzly.select.back"))
                                 ((memq kind '(note new-note))
                                  (jetpacs-shell-action-opening-surface
                                   "orgzly.note.back" (jetpacs-shell-surface-for "orgzly")
                                   :args (orgzly-ui--note-args route) :when-offline "drop"
                                   :capture-fields (mapcar #'orgzly-field-id
                                                           (orgzly-ui-note-captures route))))
                                 ((and top (not searching)) back))
                  :drawer (and top (not searching) (orgzly-ui-drawer route))
                  :fab (unless selecting (orgzly-ui-fab route))
                  :bottom-bar (and selecting (not orgzly-state-moving)
                                   (orgzly-ui-selection-bottom-bar))
                  :on-refresh (and (not selecting) (memq kind '(books book))
                                   (orgzly-ui-action "orgzly.refresh"))
                  :actions (orgzly-ui--toolbar-actions route)))
         (bar (plist-get screen :top_bar)))
    (plist-put bar :spacing 0)
    ;; Selecting, the arrow is the Toolbar's navigation button: centred
    ;; 36dp in, the count 7.5dp after it, as Revived's selection bar draws.
    (when selecting
      (let ((children (plist-get bar :children)))
        (when (and (> (length children) 0)
                   (equal (plist-get (aref children 0) :icon) "arrow_back"))
          (aset children 0 (jetpacs-with-attrs (aref children 0) :width 40))
          (plist-put bar :children
                     (vconcat (list (aref children 0) (jetpacs-spacer :width 7.5))
                              (seq-drop children 1))))))
    ;; From 600dp up AppCompat widens the toolbar's insets by 8dp a side.
    (unless (or selecting (equal (jetpacs-window-class :width) "compact"))
      (setq screen (plist-put screen :top_bar (jetpacs-with-attrs bar :pad '(:horizontal 8)))
            bar (plist-get screen :top_bar)))
    ;; The M3 small top app bar draws the hamburger; its title slot is this
    ;; row, whose title wears Revived's 22sp toolbar text.
    (cl-loop for child across (plist-get bar :children)
             for i from 0
             when (equal (plist-get child :t) "text")
             do (aset (plist-get bar :children) i
                      (jetpacs-with-attrs
                       (cond
                        (searching (orgzly-ui--search-field))
                        ((memq kind '(note new-note))
                           ;; Revived's note toolbar shows no title; the
                           ;; pane title still names the screen.
                         (jetpacs-spacer))
                        (t (orgzly-ui-styled "orgzly.t.toolbar"
                                             (jetpacs-text (plist-get child :text) :max-lines 1))))
                       :weight 1)))
    (orgzly-design-wrap screen orgzly-state-dark)))

(provide 'orgzly-ui)
;;; orgzly-ui.el ends here
