;;; orgzly-ui.el --- Books, note list & note editor views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; The Orgzly screens, as jetpacs shell views:
;;   "books"   tab      — the notebooks list (create/rename/delete/preface)
;;   "book"    overlay  — one book as a flat foldable outline in Orgzly's
;;                        item_head idiom: colored state keywords, icon-led
;;                        planning lines, inline content, swipe/long-press
;;                        quick-action popup, batch selection
;;   "note"    overlay  — the note editor: breadcrumbs, inline title,
;;                        icon-led metadata rows with clear buttons, the
;;                        timestamp dialog, tags, properties, content
;;   "preface" overlay  — the book preface editor
;;
;; All mutations funnel through orgzly-data.el (which invalidates the scan
;; memo); every handler ends in a `jetpacs-shell-push'.  Prompts inside
;; handlers (`read-string', `completing-read', `y-or-n-p') surface as
;; native dialogs via the jetpacs minibuffer bridge.
;;
;; Rendering constraint worth knowing: the companion honours `color' only
;; on rich-text spans (hex) and icons (hex or theme token) — `jetpacs-text'
;; color is ignored.  Every piece of colored text below is therefore a
;; span, and icons use the "outline" theme token so they adapt to the
;; device light/dark theme.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)
(require 'orgzly-data)
(require 'orgzly-query)

;; ─── Display preferences (Orgzly's Display settings) ─────────────────────────

(defcustom orgzly-display-content t
  "When non-nil, note content is shown in note lists (Orgzly: display content)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-content-line-count nil
  "When non-nil, titles carry the content line count when content is hidden."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-planning t
  "When non-nil, planning times show under note titles."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-book-name-in-search t
  "When non-nil, search and agenda rows show the note's notebook."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-content-preview-lines 8
  "Maximum content lines shown under a title in note lists."
  :type 'integer :group 'orgzly)

;; ─── Drill-in state ──────────────────────────────────────────────────────────

(defvar orgzly-ui--current-book nil
  "Name of the open book, or nil when on the notebooks list.")

(defvar orgzly-ui--note-ref nil
  "Ref alist of the note open in the editor view, or nil.")

(defvar orgzly-ui--preface-book nil
  "Book whose preface is being edited, or nil.")

(defvar orgzly-ui--select-mode nil
  "Non-nil while the book view is in multi-select mode.")

(defvar orgzly-ui--selection nil
  "Refs of the notes selected in multi-select mode.")

(defun orgzly-ui--reset-drill-in (&optional _view)
  "Clear all drill-in state (used when the user lands on a tab)."
  (setq orgzly-ui--current-book nil
        orgzly-ui--note-ref nil
        orgzly-ui--preface-book nil
        orgzly-ui--select-mode nil
        orgzly-ui--selection nil))

(add-hook 'jetpacs-shell-view-switched-hook #'orgzly-ui--reset-drill-in)
(add-hook 'jetpacs-shell-refresh-hook #'orgzly-data-invalidate)

;; ─── Refs on the wire ────────────────────────────────────────────────────────

(defun orgzly-ui--args-ref (args)
  "The note ref carried in action ARGS."
  `((file . ,(alist-get 'file args))
    (pos . ,(alist-get 'pos args))
    (title . ,(alist-get 'title args))))

(defun orgzly-ui--entry-for-ref (ref)
  "The current entry alist for REF, matched by position then title."
  (let* ((file (alist-get 'file ref))
         (book (orgzly-data-book-name file))
         (entries (orgzly-data-entries book)))
    (or (cl-find-if (lambda (e) (and (equal (alist-get 'file e) file)
                                     (eql (alist-get 'pos e) (alist-get 'pos ref))))
                    entries)
        (cl-find-if (lambda (e) (equal (alist-get 'title e)
                                       (alist-get 'title ref)))
                    entries))))

(defun orgzly-ui--note-action (name entry-or-ref &rest extra)
  "An `jetpacs-action' NAME carrying ENTRY-OR-REF's ref plus EXTRA args."
  (let ((ref (if (alist-get 'book entry-or-ref)
                 (orgzly-data-entry-ref entry-or-ref)
               entry-or-ref)))
    (jetpacs-action name :args (append ref extra))))

;; ─── Shared rendering ────────────────────────────────────────────────────────

;; Orgzly's item_head palette: open keywords red, done keywords green
;; (light theme red_900/green_900, dark red_200/green_200).  Span colors
;; are raw hex on the wire, so use the Material 600s — readable on both
;; companion themes.
(defconst orgzly-ui--todo-color "#e53935" "Open keyword color (Orgzly red).")
(defconst orgzly-ui--done-color "#43a047" "Done keyword color (Orgzly green).")
(defconst orgzly-ui--muted-color "#8a8a8a" "Secondary text (post-title, times).")

(defun orgzly-ui--done-p (entry)
  (let ((kws (orgzly-data-todo-keywords)))
    (and (alist-get 'state entry)
         (member (alist-get 'state entry) (cdr kws)) t)))

(defun orgzly-ui--faded-p (entry)
  "Non-nil when ENTRY renders faded — done or archived (Orgzly's 45% alpha)."
  (or (orgzly-ui--done-p entry)
      (member "ARCHIVE" (alist-get 'tags entry))
      (member "ARCHIVE" (alist-get 'itags entry))
      nil))

(cl-defun orgzly-ui--title-node (entry &key hide-content)
  "The headline laid out as Orgzly's TitleGenerator does:
STATE  #A  Title  tags  count — state colored (red open / green done),
state and priority bold, post-title text muted and space-separated.
Done and archived rows fade entirely (alpha emulated with the muted
color).  The content line count appears only when HIDE-CONTENT and the
line-count preference are on."
  (let* ((state (alist-get 'state entry))
         (done (orgzly-ui--done-p entry))
         (fade (and (orgzly-ui--faded-p entry) orgzly-ui--muted-color))
         (tags (alist-get 'tags entry))
         (lines (or (alist-get 'content-lines entry) 0)))
    (jetpacs-rich-text
     (append
      (when state
        (list (jetpacs-span (concat state "  ") :bold t
                         :color (if done orgzly-ui--done-color
                                  orgzly-ui--todo-color))))
      (when (alist-get 'priority entry)
        (list (jetpacs-span (format "#%s  " (alist-get 'priority entry))
                         :bold t :color fade)))
      (list (jetpacs-span (alist-get 'title entry) :color fade))
      (when tags
        (list (jetpacs-span
               (concat "  " (mapconcat #'substring-no-properties tags " "))
               :color orgzly-ui--muted-color)))
      (when (and hide-content orgzly-display-content-line-count (> lines 0))
        (list (jetpacs-span (format "  %d" lines)
                         :color orgzly-ui--muted-color))))
     :style 'body)))

(defun orgzly-ui--icon-line (icon label &optional color)
  "One icon-led metadata line under a title (Orgzly's item_head rows)."
  (jetpacs-row
   (jetpacs-icon icon :size 14 :color "outline")
   (jetpacs-rich-text
    (list (jetpacs-span label :color (or color orgzly-ui--muted-color)))
    :style 'caption)
   :spacing 6 :align "center"))

(defun orgzly-ui--ts-label (ts)
  "TS as Orgzly's user-facing time: \"Wed, Jul 9 10:00 +1w\"."
  (let* ((time (seconds-to-time (alist-get 'time ts)))
         (this-year (equal (format-time-string "%Y")
                           (format-time-string "%Y" time)))
         (base (format-time-string
                (concat "%a, %b %-d" (unless this-year ", %Y")
                        (when (alist-get 'has-time ts) " %H:%M"))
                time)))
    (concat base
            (when (alist-get 'repeater ts)
              (concat " " (alist-get 'repeater ts))))))

(cl-defun orgzly-ui--meta-lines (entry &key show-book only-kind)
  "The icon-led lines under ENTRY's title, per the display preferences.
ONLY-KIND, when non-nil, keeps just that planning line — the agenda
shows only the time responsible for an item's presence."
  (let ((keep (lambda (kind) (or (null only-kind) (eq kind only-kind)))))
    (delq nil
          (list
           (when (and show-book orgzly-display-book-name-in-search)
             (orgzly-ui--icon-line "folder_open" (alist-get 'book entry)))
           (when (and orgzly-display-planning (funcall keep 'scheduled))
             (when-let ((ts (alist-get 'scheduled entry)))
               (orgzly-ui--icon-line "today" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (funcall keep 'deadline))
             (when-let ((ts (alist-get 'deadline entry)))
               (orgzly-ui--icon-line "alarm" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (funcall keep 'event))
             (when-let ((ts (car (alist-get 'events entry))))
               (orgzly-ui--icon-line "access_time" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (null only-kind))
             (when-let ((ts (alist-get 'closed entry)))
               (orgzly-ui--icon-line "task_alt" (orgzly-ui--ts-label ts))))))))

(defun orgzly-ui--content-node (entry)
  "ENTRY's content shown under the title (truncated, org-highlighted), or nil."
  (when orgzly-display-content
    (let ((content (alist-get 'content entry)))
      (when (and (stringp content) (not (string-empty-p content)))
        (let ((lines (split-string content "\n")))
          (jetpacs-markup
           (if (> (length lines) orgzly-content-preview-lines)
               (concat (string-join
                        (seq-take lines orgzly-content-preview-lines) "\n")
                       "\n…")
             content)
           :syntax "org" :style 'caption))))))

(defun orgzly-ui--selected-p (entry)
  (let ((file (alist-get 'file entry)) (pos (alist-get 'pos entry)))
    (cl-find-if (lambda (r) (and (equal (alist-get 'file r) file)
                                 (eql (alist-get 'pos r) pos)))
                orgzly-ui--selection)))

(cl-defun orgzly-ui--note-row (entry &key show-book only-kind hide-content)
  "One note as a tappable flat row; the building block of every list.
Title plus icon-led metadata lines — tap opens the note (or toggles
selection in multi-select mode)."
  (let ((ref (orgzly-data-entry-ref entry)))
    (apply #'jetpacs-row
           (append
            (when orgzly-ui--select-mode
              (list (jetpacs-checkbox
                     (format "orgzly-sel:%s:%s" (alist-get 'file entry)
                             (alist-get 'pos entry))
                     :checked (and (orgzly-ui--selected-p entry) t)
                     :on-change (orgzly-ui--note-action "orgzly.note.select" ref))))
            (list
             (jetpacs-box
              (list (apply #'jetpacs-column
                           (cons (orgzly-ui--title-node
                                  entry :hide-content hide-content)
                                 (orgzly-ui--meta-lines
                                  entry :show-book show-book
                                  :only-kind only-kind))))
              :weight 1.0 :padding 4
              :on-tap (if orgzly-ui--select-mode
                          (orgzly-ui--note-action "orgzly.note.select-toggle" ref)
                        (orgzly-ui--note-action "orgzly.note.open" ref))))
            (list :align "center")))))

;; ─── Quick-action popup (Orgzly's note popup) ────────────────────────────────

(defconst orgzly-ui--quick-ops
  '(("state"       "State"       "flag"                "orgzly.note.pick-state")
    ("toggle-done" "Done"        "check_circle"        "orgzly.note.toggle-done")
    ("new-under"   "New under"   "playlist_add"        "orgzly.note.new-under")
    ("refile"      "Refile"      "move_to_inbox"       "orgzly.note.refile")
    ("archive"     "Archive"     "archive"             "orgzly.note.archive")
    ("cut"         "Cut"         "content_cut"         "orgzly.note.cut")
    ("copy"        "Copy"        "content_copy"        "orgzly.note.copy")
    ("paste"       "Paste below" "content_paste"       "orgzly.note.paste")
    ("delete"      "Delete"      "delete"              "orgzly.note.delete"))
  "(OP LABEL ICON ACTION) rows of the quick popup, in Orgzly's popup order.")

(defun orgzly-ui--quick-dialog (ref)
  "The quick-action popup spec for REF — Orgzly's swipe note popup."
  (let ((btn (lambda (op)
               (pcase-let ((`(,name ,label ,icon ,_) (assoc op orgzly-ui--quick-ops)))
                 (jetpacs-button label
                              (jetpacs-action "orgzly.note.quick-op"
                                           :args (append ref `((op . ,name)))
                                           :when-offline "drop")
                              :icon icon :variant "text")))))
    (jetpacs-lazy-column
     (jetpacs-row
      (jetpacs-box (list (orgzly-ui--quick-title ref)) :weight 1.0)
      (jetpacs-icon-button "close" (jetpacs-action "dialog.dismiss")
                        :content-description "Close")
      :align "center")
     (jetpacs-flow-row
      (jetpacs-date-button "Schedule"
                        (jetpacs-action "orgzly.note.quick-plan"
                                     :args (append ref '((kind . "scheduled")))
                                     :when-offline "drop"))
      (jetpacs-date-button "Deadline"
                        (jetpacs-action "orgzly.note.quick-plan"
                                     :args (append ref '((kind . "deadline")))
                                     :when-offline "drop"))
      (funcall btn "state")
      (funcall btn "toggle-done"))
     (jetpacs-divider)
     (apply #'jetpacs-flow-row
            (mapcar btn '("new-under" "refile" "archive")))
     (jetpacs-divider)
     (apply #'jetpacs-flow-row
            (mapcar btn '("cut" "copy" "paste" "delete"))))))

(defun orgzly-ui--quick-title (ref)
  (jetpacs-rich-text
   (list (jetpacs-span (or (alist-get 'title ref) "") :bold t))
   :style 'body))

(jetpacs-defaction "orgzly.note.quick"
  ;; Swipe or long-press on a note row: show the quick-action popup.
  (lambda (args _)
    (jetpacs-send-dialog (orgzly-ui--quick-dialog (orgzly-ui--args-ref args)))))

(jetpacs-defaction "orgzly.note.quick-op"
  ;; A popup button: dismiss the popup, then run the underlying action.
  (lambda (args payload)
    (jetpacs-dismiss-dialog)
    (when-let* ((op (assoc (alist-get 'op args) orgzly-ui--quick-ops))
                (handler (gethash (nth 3 op) jetpacs-action-handlers)))
      (funcall handler args payload))))

(jetpacs-defaction "orgzly.note.quick-plan"
  ;; The popup's Schedule/Deadline date pick: dismiss, apply the date.
  (lambda (args payload)
    (jetpacs-dismiss-dialog)
    (funcall (gethash "orgzly.note.plan-date" jetpacs-action-handlers)
             args payload)))

;; ─── Books view ──────────────────────────────────────────────────────────────

(defun orgzly-ui--book-menu (name)
  (jetpacs-menu
   (list
    (jetpacs-menu-item "New note" (jetpacs-action "orgzly.note.new"
                                            :args `((book . ,name)))
                    :icon "add")
    (jetpacs-menu-item "Paste note" (jetpacs-action "orgzly.note.paste"
                                              :args `((book . ,name)))
                    :icon "content_paste")
    (jetpacs-menu-item "Edit preface" (jetpacs-action "orgzly.book.preface"
                                                :args `((book . ,name))
                                                :when-offline "drop")
                    :icon "notes")
    (jetpacs-menu-item "Rename…" (jetpacs-action "orgzly.book.rename"
                                           :args `((book . ,name)))
                    :icon "edit")
    (jetpacs-menu-item "Make default" (jetpacs-action "orgzly.book.set-default"
                                                :args `((book . ,name)))
                    :icon "star")
    (jetpacs-menu-item "Delete…" (jetpacs-action "orgzly.book.delete"
                                           :args `((book . ,name)))
                    :icon "delete"))))

(defun orgzly-ui--book-card (b)
  "One notebook as Orgzly's book card: title plus icon-led detail lines."
  (let ((name (alist-get 'name b)))
    (jetpacs-card
     (list
      (jetpacs-row
       (jetpacs-box
        (list
         (jetpacs-column
          (jetpacs-rich-text
           (append
            (list (jetpacs-span name :bold t))
            (when (equal name orgzly-default-book)
              (list (jetpacs-span "  ★" :color orgzly-ui--muted-color))))
           :style 'title)
          (jetpacs-spacer :height 6)
          (orgzly-ui--icon-line
           "access_time"
           (format-time-string "%b %-d %H:%M"
                               (seconds-to-time (alist-get 'mtime b))))
          (orgzly-ui--icon-line
           "format_list_bulleted"
           (format "%d note%s" (alist-get 'count b)
                   (if (= (alist-get 'count b) 1) "" "s")))))
        :weight 1.0
        :on-tap (jetpacs-action "orgzly.book.open"
                             :args `((book . ,name))
                             :when-offline "drop"))
       (orgzly-ui--book-menu name)
       :align "center")))))

(defun orgzly-ui--books-body ()
  (let ((books (orgzly-data-books)))
    (if (null books)
        (jetpacs-empty-state :icon "library_books" :title "No notebooks"
                          :caption (format "Create your first notebook in %s"
                                           orgzly-directory)
                          :on-tap (jetpacs-action "orgzly.book.new")
                          :action-label "New notebook")
      (apply #'jetpacs-lazy-column (mapcar #'orgzly-ui--book-card books)))))

;; ─── Book (note list) view ───────────────────────────────────────────────────

(defun orgzly-ui--forest-of (entries min-level)
  "Recursive forest builder over document-ordered ENTRIES."
  (let (forest)
    (while entries
      (let* ((e (car entries))
             (level (alist-get 'level e)))
        (if (< level min-level)
            (setq entries nil)
          (let ((rest (cdr entries)) children)
            ;; Children: the run of following entries deeper than E.
            (let ((sub nil))
              (while (and rest (> (alist-get 'level (car rest)) level))
                (push (car rest) sub)
                (setq rest (cdr rest)))
              (setq children (orgzly-ui--forest-of (nreverse sub) (1+ level))))
            (push (cons e children) forest)
            (setq entries rest)))))
    (nreverse forest)))

(defun orgzly-ui--note-node (node)
  "NODE (ENTRY . CHILDREN) as one foldable outline element.
Every note is a collapsible: the chevron is the Orgzly bullet/fold
button (▸ leaf or folded, ▾ unfolded), children and the content preview
fold on-device, and swipe or long-press opens the quick-action popup."
  (let* ((entry (car node))
         (children (cdr node))
         (ref (orgzly-data-entry-ref entry))
         (content (orgzly-ui--content-node entry))
         (quick (jetpacs-action "orgzly.note.quick" :args ref
                             :when-offline "drop")))
    (jetpacs-collapsible
     (format "orgzly-fold:%s:%s" (alist-get 'file entry) (alist-get 'pos entry))
     (orgzly-ui--note-row entry :hide-content (not orgzly-display-content))
     (delq nil (cons content (mapcar #'orgzly-ui--note-node children)))
     :collapsed (and (null children) (null content))
     :on-long-tap quick
     :on-swipe quick)))

(defun orgzly-ui--selection-bar ()
  "Orgzly's multi-select action bar, shown while selecting."
  (jetpacs-surface
   (list
    (jetpacs-scroll-row
     (jetpacs-button "State" (jetpacs-action "orgzly.bulk.state") :variant "text"
                  :icon "flag")
     (jetpacs-button "Done" (jetpacs-action "orgzly.bulk.toggle-done")
                  :variant "text" :icon "check_circle")
     (jetpacs-date-button "Schedule"
                       (jetpacs-action "orgzly.bulk.plan"
                                    :args '((kind . "scheduled"))))
     (jetpacs-date-button "Deadline"
                       (jetpacs-action "orgzly.bulk.plan"
                                    :args '((kind . "deadline"))))
     (jetpacs-button "Refile" (jetpacs-action "orgzly.bulk.refile") :variant "text"
                  :icon "move_to_inbox")
     (jetpacs-button "Archive" (jetpacs-action "orgzly.bulk.archive")
                  :variant "text" :icon "archive")
     (jetpacs-button "Delete" (jetpacs-action "orgzly.bulk.delete")
                  :variant "text" :icon "delete")))
   :color "surface_container" :shape "rounded" :padding 4 :fill t))

(defun orgzly-ui--book-body ()
  (let* ((book orgzly-ui--current-book)
         (entries (orgzly-data-entries book))
         (preface (orgzly-data-book-preface book)))
    (apply #'jetpacs-lazy-column
           (delq nil
                 (append
                  (when orgzly-ui--select-mode
                    (list (orgzly-ui--selection-bar)))
                  (when preface
                    (list (jetpacs-box
                           (list (jetpacs-markup preface :syntax "org"
                                              :style 'caption))
                           :padding 8
                           :on-tap (jetpacs-action "orgzly.book.preface"
                                                :args `((book . ,book))
                                                :when-offline "drop"))))
                  (or (mapcar #'orgzly-ui--note-node
                              (orgzly-ui--forest-of entries 1))
                      (list (jetpacs-empty-state
                             :icon "note_add" :title "No notes"
                             :caption "Tap + to add the first note"
                             :on-tap (jetpacs-action "orgzly.note.new"
                                                  :args `((book . ,book)))
                             :action-label "New note"))))))))

(defun orgzly-ui--book-view (snackbar)
  (jetpacs-shell-nav-view
   (if orgzly-ui--select-mode
       (format "%d selected" (length orgzly-ui--selection))
     (or orgzly-ui--current-book "Book"))
   (orgzly-ui--book-body)
   :back-to "books"
   :actions (list
             (jetpacs-icon-button "search" (jetpacs-shell-switch-view "search")
                               :content-description "Search")
             (jetpacs-icon-button (if orgzly-ui--select-mode "close" "checklist")
                               (jetpacs-action "orgzly.book.select-mode"
                                            :when-offline "drop")
                               :content-description "Select notes"))
   :fab (unless orgzly-ui--select-mode
          (jetpacs-fab "add" :on-tap (jetpacs-action
                                   "orgzly.note.new"
                                   :args `((book . ,orgzly-ui--current-book)))))
   :snackbar snackbar))

;; ─── Note editor view ────────────────────────────────────────────────────────

(defun orgzly-ui--breadcrumbs (entry)
  "The Orgzly breadcrumbs: book › ancestors, each crumb tappable."
  (let* ((book (alist-get 'book entry))
         (entries (orgzly-data-entries book))
         (idx (cl-position-if
               (lambda (e) (and (equal (alist-get 'file e)
                                       (alist-get 'file entry))
                                (eql (alist-get 'pos e)
                                     (alist-get 'pos entry))))
               entries))
         (level (alist-get 'level entry))
         (crumbs nil))
    (when idx
      (let ((i (1- idx)))
        (while (and (>= i 0) (> level 1))
          (let ((e (nth i entries)))
            (when (< (alist-get 'level e) level)
              (push e crumbs)
              (setq level (alist-get 'level e))))
          (setq i (1- i)))))
    (jetpacs-box
     (list
      (jetpacs-rich-text
       (cons
        (jetpacs-span book :bold t
                   :on-tap (jetpacs-action "orgzly.book.open"
                                        :args `((book . ,book))
                                        :when-offline "drop"))
        (cl-loop for c in crumbs append
                 (list (jetpacs-span "  ›  " :color orgzly-ui--muted-color)
                       (jetpacs-span (alist-get 'title c)
                                  :on-tap (orgzly-ui--note-action
                                           "orgzly.note.open"
                                           (orgzly-data-entry-ref c))))))
       :style 'caption))
     :padding 8)))

(cl-defun orgzly-ui--meta-row (icon value hint &key on-tap on-clear)
  "One editor metadata row in Orgzly's note-fragment idiom:
leading ICON, the VALUE (or the muted HINT when empty), × to clear."
  (apply #'jetpacs-row
         (append
          (list
           (jetpacs-icon icon :size 20 :color "outline")
           (jetpacs-box
            (list (jetpacs-rich-text
                   (list (if value (jetpacs-span value)
                           (jetpacs-span hint :color orgzly-ui--muted-color)))
                   :style 'body))
            :weight 1.0 :padding 8 :on-tap on-tap))
          (when (and value on-clear)
            (list (jetpacs-icon-button
                   "close" on-clear
                   :content-description (concat "Clear " (downcase hint)))))
          (list :spacing 8 :align "center"))))

(defun orgzly-ui--properties-nodes (entry)
  (let* ((ref (orgzly-data-entry-ref entry))
         (props (ignore-errors (orgzly-data-properties ref))))
    (append
     (mapcar (lambda (p)
               (jetpacs-row
                (jetpacs-rich-text
                 (list (jetpacs-span (car p) :color orgzly-ui--muted-color))
                 :style 'label)
                (jetpacs-spacer :width 8)
                (jetpacs-text (cdr p) 'body nil nil t)
                (jetpacs-spacer :weight 1.0)
                (jetpacs-icon-button
                 "close"
                 (orgzly-ui--note-action "orgzly.note.del-property" ref
                                         (cons 'name (car p)))
                 :content-description "Delete property")
                :align "center"))
             props)
     (list (jetpacs-button "Add property"
                        (orgzly-ui--note-action "orgzly.note.add-property" ref)
                        :icon "add" :variant "text")))))

(defun orgzly-ui--note-body ()
  (let ((entry (orgzly-ui--entry-for-ref orgzly-ui--note-ref)))
    (if (null entry)
        (jetpacs-empty-state :icon "error" :title "Note not found"
                          :caption "It may have been moved or deleted")
      (let* ((ref (orgzly-data-entry-ref entry))
             (state (alist-get 'state entry))
             (priority (alist-get 'priority entry))
             (tags (mapcar #'substring-no-properties (alist-get 'tags entry)))
             (scheduled (alist-get 'scheduled entry))
             (deadline (alist-get 'deadline entry))
             (closed (alist-get 'closed entry)))
        (apply
         #'jetpacs-lazy-column
         (delq nil
               (list
                (orgzly-ui--breadcrumbs entry)
                (orgzly-ui--meta-row "folder_open" (alist-get 'book entry)
                                     "Notebook"
                                     :on-tap (orgzly-ui--note-action
                                              "orgzly.note.refile" ref))
                (jetpacs-text-input
                 (format "orgzly-title:%s:%s" (alist-get 'file entry)
                         (alist-get 'pos entry))
                 :value (alist-get 'title entry)
                 :hint "Title"
                 :on-submit (orgzly-ui--note-action "orgzly.note.rename" ref)
                 :single-line t)
                (jetpacs-divider)
                (orgzly-ui--meta-row
                 "label" (and tags (string-join tags " ")) "Tags"
                 :on-tap (orgzly-ui--note-action "orgzly.note.edit-tags" ref)
                 :on-clear (jetpacs-action "orgzly.note.set-tags"
                                        :args (append ref '((value . [])))))
                (orgzly-ui--meta-row
                 "flag" state "State"
                 :on-tap (orgzly-ui--note-action "orgzly.note.pick-state" ref)
                 :on-clear (orgzly-ui--note-action "orgzly.note.set-state" ref
                                                   '(state . "")))
                (orgzly-ui--meta-row
                 "star_border" (and priority (format "Priority %s" priority))
                 "Priority"
                 :on-tap (orgzly-ui--note-action "orgzly.note.pick-priority" ref)
                 :on-clear (orgzly-ui--note-action "orgzly.note.set-priority" ref
                                                   '(priority . "")))
                (orgzly-ui--meta-row
                 "today" (and scheduled (orgzly-ui--ts-label scheduled))
                 "Schedule"
                 :on-tap (orgzly-ui--note-action "orgzly.note.plan-dialog" ref
                                                 '(kind . "scheduled"))
                 :on-clear (orgzly-ui--note-action "orgzly.note.plan-clear" ref
                                                   '(kind . "scheduled")))
                (orgzly-ui--meta-row
                 "alarm" (and deadline (orgzly-ui--ts-label deadline))
                 "Deadline"
                 :on-tap (orgzly-ui--note-action "orgzly.note.plan-dialog" ref
                                                 '(kind . "deadline"))
                 :on-clear (orgzly-ui--note-action "orgzly.note.plan-clear" ref
                                                   '(kind . "deadline")))
                (when closed
                  (orgzly-ui--meta-row "task_alt"
                                       (orgzly-ui--ts-label closed) "Closed"))
                (jetpacs-divider)
                (apply #'jetpacs-column
                       (orgzly-ui--properties-nodes entry))
                (jetpacs-divider)
                (jetpacs-editor (format "orgzly:%s:%s" (alist-get 'file entry)
                                     (alist-get 'pos entry))
                             (alist-get 'content entry)
                             :on-save (orgzly-ui--note-action
                                       "orgzly.note.set-content" ref)
                             :syntax "org" :toolbar "org" :chromeless t))))))))

(defun orgzly-ui--note-menu (entry-or-ref)
  "The note editor's overflow menu: structure and clipboard operations."
  (let ((ref (if (alist-get 'book entry-or-ref)
                 (orgzly-data-entry-ref entry-or-ref)
               entry-or-ref)))
    (jetpacs-menu
     (list
      (jetpacs-menu-item "New note under"
                      (orgzly-ui--note-action "orgzly.note.new-under" ref)
                      :icon "playlist_add")
      (jetpacs-menu-item "Refile…"
                      (orgzly-ui--note-action "orgzly.note.refile" ref)
                      :icon "move_to_inbox")
      (jetpacs-menu-item "Archive"
                      (orgzly-ui--note-action "orgzly.note.archive" ref)
                      :icon "archive")
      (jetpacs-menu-item "Cut" (orgzly-ui--note-action "orgzly.note.cut" ref)
                      :icon "content_cut")
      (jetpacs-menu-item "Copy" (orgzly-ui--note-action "orgzly.note.copy" ref)
                      :icon "content_copy")
      (jetpacs-menu-item "Paste below"
                      (orgzly-ui--note-action "orgzly.note.paste" ref)
                      :icon "content_paste")
      (jetpacs-menu-item "Promote" (orgzly-ui--note-action "orgzly.note.promote" ref)
                      :icon "format_indent_decrease")
      (jetpacs-menu-item "Demote" (orgzly-ui--note-action "orgzly.note.demote" ref)
                      :icon "format_indent_increase")
      (jetpacs-menu-item "Move up" (orgzly-ui--note-action "orgzly.note.move-up" ref)
                      :icon "arrow_upward")
      (jetpacs-menu-item "Move down"
                      (orgzly-ui--note-action "orgzly.note.move-down" ref)
                      :icon "arrow_downward")
      (jetpacs-menu-item "Delete…" (orgzly-ui--note-action "orgzly.note.delete" ref)
                      :icon "delete")))))

(defun orgzly-ui--note-view (snackbar)
  (let ((ref orgzly-ui--note-ref))
    (jetpacs-shell-nav-view
     "Note"
     (orgzly-ui--note-body)
     :back-to (if orgzly-ui--current-book "book" "books")
     :actions (when ref
                (list (orgzly-ui--note-menu
                       (or (orgzly-ui--entry-for-ref ref) ref))))
     :snackbar snackbar)))

;; ─── Timestamp dialog (Orgzly's dialog_timestamp) ────────────────────────────

(defun orgzly-ui--plan-parts (ref kind)
  "Current (DATE TIME REPEATER) strings of REF's KIND planning, or nils."
  (let* ((entry (orgzly-ui--entry-for-ref ref))
         (ts (and entry (alist-get (intern kind) entry)))
         (time (and ts (alist-get 'time ts))))
    (list (and time (format-time-string "%Y-%m-%d" (seconds-to-time time)))
          (and ts (alist-get 'has-time ts)
               (format-time-string "%H:%M" (seconds-to-time time)))
          (and ts (alist-get 'repeater ts)))))

(defun orgzly-ui--plan-dialog (ref kind)
  "The timestamp dialog spec for REF's KIND planning line."
  (let* ((parts (orgzly-ui--plan-parts ref kind))
         (args (lambda (&rest extra)
                 (append ref (cons (cons 'kind kind) extra)))))
    (jetpacs-lazy-column
     (jetpacs-row
      (jetpacs-box
       (list (jetpacs-text (if (equal kind "deadline")
                            "Deadline time" "Scheduled time")
                        'title))
       :weight 1.0)
      (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
      :align "center")
     (jetpacs-row
      (jetpacs-icon (if (equal kind "deadline") "alarm" "today")
                 :size 20 :color "outline")
      (jetpacs-date-button (or (nth 0 parts) "Date")
                        (jetpacs-action "orgzly.note.plan-date"
                                     :args (funcall args '(dialog . t)))
                        :value (nth 0 parts))
      (jetpacs-time-button (or (nth 1 parts) "Time")
                        (jetpacs-action "orgzly.note.plan-time"
                                     :args (funcall args '(dialog . t)))
                        :value (nth 1 parts))
      (jetpacs-button (or (nth 2 parts) "Repeat")
                   (jetpacs-action "orgzly.note.plan-repeater"
                                :args (funcall args '(dialog . t)))
                   :variant "text")
      :spacing 8 :align "center")
     (jetpacs-row
      (jetpacs-spacer :weight 1.0)
      (jetpacs-button "Clear"
                   (jetpacs-action "orgzly.note.plan-clear"
                                :args (funcall args))
                   :variant "text" :icon "close")))))

(jetpacs-defaction "orgzly.note.plan-dialog"
  (lambda (args _)
    (jetpacs-send-dialog
     (orgzly-ui--plan-dialog (orgzly-ui--args-ref args)
                             (alist-get 'kind args)))))

;; ─── Tags dialog ─────────────────────────────────────────────────────────────

(defun orgzly-ui--known-tags ()
  "Every tag in use across books, plus configured favourites."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (e (orgzly-data-entries))
      (dolist (tag (alist-get 'tags e))
        (puthash (substring-no-properties tag) t tags)))
    (dolist (ta org-tag-alist)
      (when (and (consp ta) (stringp (car ta)))
        (puthash (car ta) t tags)))
    (sort (hash-table-keys tags) #'string-lessp)))

(defun orgzly-ui--tags-dialog (ref current)
  "The tag-picker dialog spec for REF with CURRENT tags selected."
  (jetpacs-lazy-column
   (jetpacs-row
    (jetpacs-box (list (jetpacs-text "Tags" 'title)) :weight 1.0)
    (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
    :align "center")
   (jetpacs-enum-list "orgzly-note-tags"
                   (orgzly-ui--known-tags)
                   :value current
                   :multi-select t :allow-add t
                   :on-change (orgzly-ui--note-action "orgzly.note.set-tags" ref))))

(jetpacs-defaction "orgzly.note.edit-tags"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (entry (orgzly-ui--entry-for-ref ref)))
      (jetpacs-send-dialog
       (orgzly-ui--tags-dialog
        ref (mapcar #'substring-no-properties
                    (and entry (alist-get 'tags entry))))))))

;; ─── Preface editor view ─────────────────────────────────────────────────────

(defun orgzly-ui--preface-view (snackbar)
  (let* ((book orgzly-ui--preface-book))
    (jetpacs-shell-nav-view
     (format "%s — preface" book)
     (jetpacs-editor (format "orgzly-preface:%s" book)
                  (or (orgzly-data-book-preface book) "")
                  :on-save (jetpacs-action "orgzly.book.set-preface"
                                        :args `((book . ,book)))
                  :syntax "org" :toolbar "org")
     :back-to "book"
     :snackbar snackbar)))

;; ─── View registrations ──────────────────────────────────────────────────────

(jetpacs-shell-define-view "books"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "books" (orgzly-ui--books-body)
                                  :snackbar snackbar
                                  :fab (jetpacs-fab "create_new_folder"
                                                 :on-tap (jetpacs-action
                                                          "orgzly.book.new"))))
  :tab '(:icon "library_books" :label "Books")
  :order 10)

(jetpacs-shell-define-view "note"
  :builder #'orgzly-ui--note-view
  :overlay (lambda () orgzly-ui--note-ref)
  :order 21)

(jetpacs-shell-define-view "preface"
  :builder #'orgzly-ui--preface-view
  :overlay (lambda () orgzly-ui--preface-book)
  :order 22)

(jetpacs-shell-define-view "book"
  :builder #'orgzly-ui--book-view
  :overlay (lambda () orgzly-ui--current-book)
  :order 23)

;; ─── Actions: books ──────────────────────────────────────────────────────────

(jetpacs-defaction "orgzly.book.open"
  (lambda (args _)
    (setq orgzly-ui--current-book (alist-get 'book args)
          orgzly-ui--note-ref nil
          orgzly-ui--preface-book nil)
    (jetpacs-shell-push nil :switch-to "book")))

(jetpacs-defaction "orgzly.book.new"
  (lambda (_ _)
    (let ((name (string-trim (read-string "Notebook name: "))))
      (unless (string-empty-p name)
        (orgzly-data-create-book name)
        (jetpacs-shell-notify (format "Created %s" name))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.rename"
  (lambda (args _)
    (let* ((book (alist-get 'book args))
           (new (string-trim (read-string (format "Rename %s to: " book) book))))
      (unless (or (string-empty-p new) (equal new book))
        (orgzly-data-rename-book book new)
        (when (equal orgzly-ui--current-book book)
          (setq orgzly-ui--current-book new))
        (jetpacs-shell-notify (format "Renamed to %s" new))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.delete"
  (lambda (args _)
    (let ((book (alist-get 'book args)))
      (when (y-or-n-p (format "Delete notebook %s and all its notes? " book))
        (orgzly-data-delete-book book)
        (when (equal orgzly-ui--current-book book)
          (orgzly-ui--reset-drill-in))
        (jetpacs-shell-notify (format "Deleted %s" book))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.set-default"
  (lambda (args _)
    (let ((book (alist-get 'book args)))
      (customize-save-variable 'orgzly-default-book book)
      (jetpacs-shell-notify (format "%s is now the default notebook" book))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.book.select-mode"
  (lambda (_ _)
    (setq orgzly-ui--select-mode (not orgzly-ui--select-mode)
          orgzly-ui--selection nil)
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.book.preface"
  (lambda (args _)
    (setq orgzly-ui--preface-book (alist-get 'book args))
    (jetpacs-shell-push nil :switch-to "preface")))

(jetpacs-defaction "orgzly.book.set-preface"
  (lambda (args _)
    (let* ((book (alist-get 'book args))
           (text (alist-get 'value args))
           (file (orgzly-data-book-file book)))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((end (if (re-search-forward org-outline-regexp-bol nil t)
                        (match-beginning 0)
                      (point-max))))
           (delete-region (point-min) end)
           (goto-char (point-min))
           (let ((clean (string-trim (or text ""))))
             (unless (string-empty-p clean)
               (insert clean "\n\n")))))
        (save-buffer))
      (orgzly-data-invalidate)
      (setq orgzly-ui--preface-book nil)
      (jetpacs-shell-notify "Preface saved")
      (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book "book" "books")))))

;; ─── Actions: single note ────────────────────────────────────────────────────

(defun orgzly-ui--after-mutation (message &optional ref)
  "Common tail of a note mutation: refresh the note ref, notify, push."
  (when ref (setq orgzly-ui--note-ref ref))
  (when message (jetpacs-shell-notify message))
  (jetpacs-shell-push))

(jetpacs-defaction "orgzly.note.open"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (setq orgzly-ui--note-ref ref)
      (unless orgzly-ui--current-book
        (setq orgzly-ui--current-book
              (orgzly-data-book-name (alist-get 'file ref))))
      (jetpacs-shell-push nil :switch-to "note"))))

(jetpacs-defaction "orgzly.note.new"
  (lambda (args _)
    (let* ((book (or (alist-get 'book args)
                     orgzly-ui--current-book orgzly-default-book))
           (title (string-trim (read-string "Note title: "))))
      (unless (string-empty-p title)
        (let ((ref (orgzly-data-new-note book title)))
          (setq orgzly-ui--current-book book
                orgzly-ui--note-ref ref)
          (jetpacs-shell-notify "Note created")
          (jetpacs-shell-push nil :switch-to "note"))))))

(jetpacs-defaction "orgzly.note.new-under"
  (lambda (args _)
    (let* ((parent (orgzly-ui--args-ref args))
           (book (orgzly-data-book-name (alist-get 'file parent)))
           (title (string-trim (read-string "Note title: "))))
      (unless (string-empty-p title)
        (let ((ref (orgzly-data-new-note book title :under parent)))
          (setq orgzly-ui--note-ref ref)
          (jetpacs-shell-notify "Note created")
          (jetpacs-shell-push nil :switch-to "note"))))))

(jetpacs-defaction "orgzly.note.rename"
  ;; From the editor's inline title field (args carry `value') or a
  ;; bridged prompt.
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (new (or (alist-get 'value args)
                    (read-string "Title: " (alist-get 'title ref)))))
      (unless (string-empty-p (string-trim new))
        (orgzly-data-set-title ref new)
        (orgzly-ui--after-mutation "Renamed"
                                   (append `((title . ,new))
                                           (assq-delete-all 'title ref)))))))

(jetpacs-defaction "orgzly.note.set-state"
  (lambda (args _)
    (orgzly-data-set-state (orgzly-ui--args-ref args) (alist-get 'state args))
    (orgzly-ui--after-mutation nil)))

(jetpacs-defaction "orgzly.note.toggle-done"
  (lambda (args _)
    (orgzly-data-toggle-done (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation nil)))

(jetpacs-defaction "orgzly.note.pick-state"
  (lambda (args _)
    (let* ((kws (orgzly-data-todo-keywords))
           (state (completing-read "State: "
                                   (append '("NONE") (car kws) (cdr kws))
                                   nil t)))
      (orgzly-data-set-state (orgzly-ui--args-ref args)
                             (unless (equal state "NONE") state))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-priority"
  (lambda (args _)
    (let ((p (alist-get 'priority args)))
      (orgzly-data-set-priority (orgzly-ui--args-ref args)
                                (unless (or (null p) (string-empty-p p)) p))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.pick-priority"
  (lambda (args _)
    (let* ((letters (cl-loop for c from org-priority-highest
                             to org-priority-lowest
                             collect (char-to-string c)))
           (p (completing-read "Priority: " (cons "None" letters) nil t)))
      (orgzly-data-set-priority (orgzly-ui--args-ref args)
                                (unless (equal p "None") p))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-tags"
  (lambda (args _)
    (let ((tags (alist-get 'value args)))
      (orgzly-data-set-tags (orgzly-ui--args-ref args)
                            (append tags nil))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-content"
  (lambda (args _)
    (orgzly-data-set-content (orgzly-ui--args-ref args)
                             (or (alist-get 'value args) ""))
    (orgzly-ui--after-mutation "Saved")))

;; Planning editors.  The timestamp dialog's date/time buttons inject the
;; picked value into args as `value'; repeater goes through a bridged
;; prompt.  Actions carrying (dialog . t) re-show the dialog with the
;; updated values, so it edits in place like Orgzly's.

(defun orgzly-ui--plan-apply (ref kind date time repeater &optional dialog)
  "Write the planning string assembled from DATE/TIME/REPEATER onto REF."
  (orgzly-data-set-planning
   ref (intern kind)
   (and date (string-join (delq nil (list date time repeater)) " ")))
  (when dialog
    (jetpacs-send-dialog (orgzly-ui--plan-dialog ref kind)))
  (orgzly-ui--after-mutation nil))

(jetpacs-defaction "orgzly.note.plan-date"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind)))
      (orgzly-ui--plan-apply ref kind (alist-get 'value args)
                             (nth 1 parts) (nth 2 parts)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-time"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind))
           (date (or (nth 0 parts) (format-time-string "%Y-%m-%d"))))
      (orgzly-ui--plan-apply ref kind date (alist-get 'value args)
                             (nth 2 parts)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-repeater"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind))
           (rep (string-trim
                 (read-string "Repeater (+1w, ++1m, .+2d; empty to clear): "
                              (or (nth 2 parts) "")))))
      (orgzly-ui--plan-apply ref kind
                             (or (nth 0 parts) (format-time-string "%Y-%m-%d"))
                             (nth 1 parts)
                             (unless (string-empty-p rep) rep)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-clear"
  (lambda (args _)
    (jetpacs-dismiss-dialog)
    (orgzly-ui--plan-apply (orgzly-ui--args-ref args) (alist-get 'kind args)
                           nil nil nil)))

;; Properties.

(jetpacs-defaction "orgzly.note.add-property"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (name (string-trim (read-string "Property name: ")))
           (value (and (not (string-empty-p name))
                       (read-string (format "%s value: " name)))))
      (unless (string-empty-p name)
        (orgzly-data-set-property ref (upcase name) value)
        (orgzly-ui--after-mutation nil)))))

(jetpacs-defaction "orgzly.note.del-property"
  (lambda (args _)
    (orgzly-data-set-property (orgzly-ui--args-ref args)
                              (alist-get 'name args) nil)
    (orgzly-ui--after-mutation nil)))

;; Structure ops.

(jetpacs-defaction "orgzly.note.delete"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (when (y-or-n-p (format "Delete \"%s\"? " (alist-get 'title ref)))
        (orgzly-data-delete-note ref)
        (when (equal (alist-get 'pos orgzly-ui--note-ref) (alist-get 'pos ref))
          (setq orgzly-ui--note-ref nil))
        (jetpacs-shell-notify "Deleted")
        (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                            "book" "books"))))))

(jetpacs-defaction "orgzly.note.refile"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (books (mapcar (lambda (b) (alist-get 'name b)) (orgzly-data-books)))
           (target (completing-read "Refile to book: " books nil t)))
      (orgzly-data-refile ref target)
      (setq orgzly-ui--note-ref nil)
      (jetpacs-shell-notify (format "Refiled to %s" target))
      (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                          "book" "books")))))

(jetpacs-defaction "orgzly.note.archive"
  (lambda (args _)
    (orgzly-data-archive (orgzly-ui--args-ref args))
    (setq orgzly-ui--note-ref nil)
    (jetpacs-shell-notify "Archived")
    (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                        "book" "books"))))

(jetpacs-defaction "orgzly.note.cut"
  (lambda (args _)
    (orgzly-data-cut-note (orgzly-ui--args-ref args))
    (setq orgzly-ui--note-ref nil)
    (jetpacs-shell-notify "Cut — paste from a note or book menu")
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.note.copy"
  (lambda (args _)
    (orgzly-data-copy-note (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation "Copied")))

(jetpacs-defaction "orgzly.note.paste"
  (lambda (args _)
    (let ((book (or (alist-get 'book args)
                    (and (alist-get 'file args)
                         (orgzly-data-book-name (alist-get 'file args)))
                    orgzly-ui--current-book)))
      (condition-case err
          (progn
            (orgzly-data-paste-note book (and (alist-get 'pos args)
                                              (orgzly-ui--args-ref args)))
            (orgzly-ui--after-mutation "Pasted"))
        (error (orgzly-ui--after-mutation
                (format "Nothing to paste (%s)" (error-message-string err))))))))

(dolist (op '(("orgzly.note.promote" . orgzly-data-promote)
              ("orgzly.note.demote" . orgzly-data-demote)
              ("orgzly.note.move-up" . orgzly-data-move-up)
              ("orgzly.note.move-down" . orgzly-data-move-down)))
  (let ((fn (cdr op)))
    (jetpacs-defaction (car op)
      (lambda (args _)
        (condition-case err
            (progn (funcall fn (orgzly-ui--args-ref args))
                   (orgzly-ui--after-mutation nil))
          (error (orgzly-ui--after-mutation (error-message-string err))))))))

;; ─── Actions: selection & bulk ───────────────────────────────────────────────

(defun orgzly-ui--toggle-selected (ref &optional force)
  (let ((present (orgzly-ui--selected-p ref)))
    (cond
     ((and present (not (eq force 'add)))
      (setq orgzly-ui--selection (delq present orgzly-ui--selection)))
     ((not present)
      (push ref orgzly-ui--selection)))))

(jetpacs-defaction "orgzly.note.select"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args))
          (checked (alist-get 'value args)))
      (orgzly-ui--toggle-selected ref (if (eq checked :false) 'remove 'add))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.note.select-toggle"
  (lambda (args _)
    (orgzly-ui--toggle-selected (orgzly-ui--args-ref args))
    (jetpacs-shell-push)))

(defun orgzly-ui--bulk (fn &optional message keep-selection)
  "Apply FN to every selected ref, then refresh."
  (let ((refs orgzly-ui--selection))
    (if (null refs)
        (jetpacs-shell-notify "Nothing selected")
      (dolist (ref refs) (funcall fn ref))
      (unless keep-selection
        (setq orgzly-ui--selection nil orgzly-ui--select-mode nil))
      (when message
        (jetpacs-shell-notify (format "%s (%d notes)" message (length refs))))))
  (jetpacs-shell-push))

(jetpacs-defaction "orgzly.bulk.state"
  (lambda (_ _)
    (let* ((kws (orgzly-data-todo-keywords))
           (state (completing-read "State for selected: "
                                   (append '("NONE") (car kws) (cdr kws))
                                   nil t)))
      (orgzly-ui--bulk (lambda (ref)
                         (orgzly-data-set-state
                          ref (unless (equal state "NONE") state)))
                       (format "State → %s" state) t))))

(jetpacs-defaction "orgzly.bulk.toggle-done"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-toggle-done "Toggled" t)))

(jetpacs-defaction "orgzly.bulk.plan"
  (lambda (args _)
    (let ((kind (intern (alist-get 'kind args)))
          (date (alist-get 'value args)))
      (orgzly-ui--bulk (lambda (ref) (orgzly-data-set-planning ref kind date))
                       (format "%s → %s" (capitalize (symbol-name kind)) date)
                       t))))

(jetpacs-defaction "orgzly.bulk.refile"
  (lambda (_ _)
    (let* ((books (mapcar (lambda (b) (alist-get 'name b)) (orgzly-data-books)))
           (target (completing-read "Refile selected to: " books nil t)))
      (orgzly-ui--bulk (lambda (ref) (orgzly-data-refile ref target))
                       (format "Refiled to %s" target)))))

(jetpacs-defaction "orgzly.bulk.archive"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-archive "Archived")))

(jetpacs-defaction "orgzly.bulk.delete"
  (lambda (_ _)
    (when (y-or-n-p (format "Delete %d selected notes? "
                            (length orgzly-ui--selection)))
      ;; Delete bottom-most first so earlier positions stay valid.
      (let ((refs (sort (copy-sequence orgzly-ui--selection)
                        (lambda (a b) (> (alist-get 'pos a)
                                         (alist-get 'pos b))))))
        (setq orgzly-ui--selection refs)
        (orgzly-ui--bulk #'orgzly-data-delete-note "Deleted")))))

(provide 'orgzly-ui)
;;; orgzly-ui.el ends here
