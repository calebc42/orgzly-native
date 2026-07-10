;;; orgzly-ui.el --- Books, note list & note editor views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; The Orgzly screens, as jetpacs shell views:
;;   "books"   tab      — the notebooks list (create/rename/delete/preface)
;;   "book"    overlay  — one book's outline: fold, per-note menus, batch
;;                        selection with Orgzly's multi-select toolbar
;;   "note"    overlay  — the note editor: state/priority chips, planning
;;                        with date/time/repeater, tags, properties, content
;;   "preface" overlay  — the book preface editor
;;
;; All mutations funnel through orgzly-data.el (which invalidates the scan
;; memo); every handler ends in a `jetpacs-shell-push'.  Prompts inside
;; handlers (`read-string', `completing-read', `y-or-n-p') surface as
;; native dialogs via the jetpacs minibuffer bridge.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)
(require 'orgzly-data)
(require 'orgzly-query)

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

(defconst orgzly-ui--todo-color "#43a047" "Open keyword color (Orgzly green).")
(defconst orgzly-ui--done-color "#9e9e9e" "Done keyword color (gray).")
(defconst orgzly-ui--priority-color "#e53935" "Priority cookie color.")
(defconst orgzly-ui--meta-color "#8a8a8a" "Metadata line color.")

(defun orgzly-ui--done-p (entry)
  (let ((kws (orgzly-data-todo-keywords)))
    (and (alist-get 'state entry)
         (member (alist-get 'state entry) (cdr kws)) t)))

(defun orgzly-ui--title-node (entry)
  "The note's headline as styled spans: state, priority, title, tags."
  (let* ((state (alist-get 'state entry))
         (done (orgzly-ui--done-p entry))
         (spans
          (append
           (when state
             (list (jetpacs-span (concat state " ") :bold t
                              :color (if done orgzly-ui--done-color
                                       orgzly-ui--todo-color))))
           (when (alist-get 'priority entry)
             (list (jetpacs-span (format "[#%s] " (alist-get 'priority entry))
                              :bold t :color orgzly-ui--priority-color)))
           (list (jetpacs-span (alist-get 'title entry) :strike done))
           (when (alist-get 'tags entry)
             (list (jetpacs-span
                    (concat "  :" (string-join (alist-get 'tags entry) ":") ":")
                    :color orgzly-ui--meta-color))))))
    (jetpacs-rich-text spans :style 'body)))

(defun orgzly-ui--ts-label (ts)
  "Compact display of TS: \"Jul 9\" / \"Jul 9 10:00 +1w\"."
  (let* ((time (alist-get 'time ts))
         (base (format-time-string
                (if (alist-get 'has-time ts) "%b %-d %H:%M" "%b %-d")
                (seconds-to-time time))))
    (concat base
            (when (alist-get 'repeater ts)
              (concat " " (alist-get 'repeater ts))))))

(defun orgzly-ui--meta-line (entry &optional show-book)
  "The secondary row under a note title, or nil when it would be empty."
  (let* ((bits (delq nil
                     (list
                      (when-let ((s (alist-get 'scheduled entry)))
                        (concat "S: " (orgzly-ui--ts-label s)))
                      (when-let ((d (alist-get 'deadline entry)))
                        (concat "D: " (orgzly-ui--ts-label d)))
                      (when-let ((ev (car (alist-get 'events entry))))
                        (concat "E: " (orgzly-ui--ts-label ev)))
                      (when (> (or (alist-get 'content-lines entry) 0) 0)
                        (format "%d line%s" (alist-get 'content-lines entry)
                                (if (= (alist-get 'content-lines entry) 1) "" "s")))
                      (when show-book (alist-get 'book entry))))))
    (when bits
      (jetpacs-text (string-join bits "  ·  ") 'caption nil orgzly-ui--meta-color))))

(defun orgzly-ui--note-menu (entry)
  "The per-note overflow menu."
  (let ((ref (orgzly-data-entry-ref entry)))
    (jetpacs-menu
     (list
      (jetpacs-menu-item "Open" (orgzly-ui--note-action "orgzly.note.open" ref)
                      :icon "open_in_new")
      (jetpacs-menu-item "Cycle state"
                      (orgzly-ui--note-action "orgzly.note.toggle-done" ref)
                      :icon "check_circle")
      (jetpacs-menu-item "Set state…"
                      (orgzly-ui--note-action "orgzly.note.pick-state" ref)
                      :icon "flag")
      (jetpacs-menu-item "Schedule…"
                      (orgzly-ui--note-action "orgzly.note.pick-plan" ref
                                              '(kind . "scheduled"))
                      :icon "today")
      (jetpacs-menu-item "Deadline…"
                      (orgzly-ui--note-action "orgzly.note.pick-plan" ref
                                              '(kind . "deadline"))
                      :icon "alarm")
      (jetpacs-menu-item "New note under"
                      (orgzly-ui--note-action "orgzly.note.new-under" ref)
                      :icon "playlist_add")
      (jetpacs-menu-item "Refile…"
                      (orgzly-ui--note-action "orgzly.note.refile" ref)
                      :icon "drive_file_move")
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
                      :icon "chevron_left")
      (jetpacs-menu-item "Demote" (orgzly-ui--note-action "orgzly.note.demote" ref)
                      :icon "chevron_right")
      (jetpacs-menu-item "Move up" (orgzly-ui--note-action "orgzly.note.move-up" ref)
                      :icon "arrow_upward")
      (jetpacs-menu-item "Move down"
                      (orgzly-ui--note-action "orgzly.note.move-down" ref)
                      :icon "arrow_downward")
      (jetpacs-menu-item "Delete…" (orgzly-ui--note-action "orgzly.note.delete" ref)
                      :icon "delete")))))

(defun orgzly-ui--selected-p (entry)
  (let ((file (alist-get 'file entry)) (pos (alist-get 'pos entry)))
    (cl-find-if (lambda (r) (and (equal (alist-get 'file r) file)
                                 (eql (alist-get 'pos r) pos)))
                orgzly-ui--selection)))

(cl-defun orgzly-ui--note-row (entry &key show-book (indent 0) (menu t))
  "One note as a tappable row; the building block of every list."
  (let ((ref (orgzly-data-entry-ref entry)))
    (apply #'jetpacs-row
           (append
            (when (> indent 0) (list (jetpacs-spacer :width (* 14 indent))))
            (when orgzly-ui--select-mode
              (list (jetpacs-checkbox
                     (format "orgzly-sel:%s:%s" (alist-get 'file entry)
                             (alist-get 'pos entry))
                     :checked (and (orgzly-ui--selected-p entry) t)
                     :on-change (orgzly-ui--note-action "orgzly.note.select" ref))))
            (list
             (jetpacs-box
              (list (apply #'jetpacs-column
                           (delq nil (list (orgzly-ui--title-node entry)
                                           (orgzly-ui--meta-line entry show-book)))))
              :weight 1.0
              :on-tap (if orgzly-ui--select-mode
                          (orgzly-ui--note-action "orgzly.note.select-toggle" ref)
                        (orgzly-ui--note-action "orgzly.note.open" ref))))
            (when (and menu (not orgzly-ui--select-mode))
              (list (orgzly-ui--note-menu entry)))
            (list :align "center")))))

;; ─── Books view ──────────────────────────────────────────────────────────────

(defun orgzly-ui--book-menu (name)
  (jetpacs-menu
   (list
    (jetpacs-menu-item "Open" (jetpacs-action "orgzly.book.open"
                                        :args `((book . ,name))
                                        :when-offline "drop")
                    :icon "open_in_new")
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

(defun orgzly-ui--books-body ()
  (let ((books (orgzly-data-books)))
    (if (null books)
        (jetpacs-empty-state :icon "library_books" :title "No notebooks"
                          :caption (format "Create your first notebook in %s"
                                           orgzly-directory)
                          :on-tap (jetpacs-action "orgzly.book.new")
                          :action-label "New notebook")
      (apply #'jetpacs-lazy-column
             (mapcar
              (lambda (b)
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
                            (list (jetpacs-span "  ★" :color orgzly-ui--todo-color))))
                         :style 'body)
                        (jetpacs-text
                         (format "%d note%s  ·  %s"
                                 (alist-get 'count b)
                                 (if (= (alist-get 'count b) 1) "" "s")
                                 (format-time-string
                                  "%b %-d %H:%M"
                                  (seconds-to-time (alist-get 'mtime b))))
                         'caption nil orgzly-ui--meta-color)))
                      :weight 1.0
                      :on-tap (jetpacs-action "orgzly.book.open"
                                           :args `((book . ,name))
                                           :when-offline "drop"))
                     (orgzly-ui--book-menu name)
                     :align "center")))))
              books)))))

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

(defun orgzly-ui--note-nodes (forest &optional depth)
  "Widget nodes for FOREST; children fold under a collapsible."
  (let ((depth (or depth 0)))
    (mapcar
     (lambda (node)
       (let* ((entry (car node))
              (children (cdr node))
              (row (orgzly-ui--note-row entry :indent depth)))
         (if (null children)
             (jetpacs-card (list row))
           (jetpacs-collapsible
            (format "orgzly-fold:%s:%s" (alist-get 'file entry)
                    (alist-get 'pos entry))
            row
            (orgzly-ui--note-nodes children (1+ depth))
            :on-long-tap (orgzly-ui--note-action
                          "orgzly.note.open" (orgzly-data-entry-ref entry))))))
     forest)))

(defun orgzly-ui--selection-bar ()
  "Orgzly's multi-select toolbar, shown while notes are selected."
  (let ((n (length orgzly-ui--selection)))
    (jetpacs-card
     (list
      (jetpacs-column
       (jetpacs-text (format "%d selected" n) 'label)
       (jetpacs-scroll-row
        (jetpacs-button "State" (jetpacs-action "orgzly.bulk.state") :variant "tonal")
        (jetpacs-button "Done" (jetpacs-action "orgzly.bulk.toggle-done") :variant "tonal")
        (jetpacs-date-button "Schedule"
                          (jetpacs-action "orgzly.bulk.plan"
                                       :args '((kind . "scheduled"))))
        (jetpacs-date-button "Deadline"
                          (jetpacs-action "orgzly.bulk.plan"
                                       :args '((kind . "deadline"))))
        (jetpacs-button "Refile" (jetpacs-action "orgzly.bulk.refile") :variant "tonal")
        (jetpacs-button "Archive" (jetpacs-action "orgzly.bulk.archive") :variant "tonal")
        (jetpacs-button "Delete" (jetpacs-action "orgzly.bulk.delete") :variant "tonal")))))))

(defun orgzly-ui--book-body ()
  (let* ((book orgzly-ui--current-book)
         (entries (orgzly-data-entries book))
         (preface (orgzly-data-book-preface book)))
    (apply #'jetpacs-lazy-column
           (delq nil
                 (append
                  (when (and orgzly-ui--select-mode orgzly-ui--selection)
                    (list (orgzly-ui--selection-bar)))
                  (when preface
                    (list (jetpacs-card
                           (list (jetpacs-markup preface :syntax "org"))
                           :on-tap (jetpacs-action "orgzly.book.preface"
                                                :args `((book . ,book))
                                                :when-offline "drop"))))
                  (or (orgzly-ui--note-nodes (orgzly-ui--forest-of entries 1))
                      (list (jetpacs-empty-state
                             :icon "note_add" :title "No notes"
                             :caption "Tap + to add the first note"
                             :on-tap (jetpacs-action "orgzly.note.new"
                                                  :args `((book . ,book)))
                             :action-label "New note"))))))))

(defun orgzly-ui--book-view (snackbar)
  (jetpacs-shell-nav-view
   (or orgzly-ui--current-book "Book")
   (orgzly-ui--book-body)
   :back-to "books"
   :actions (list
             (jetpacs-icon-button (if orgzly-ui--select-mode "close" "checklist")
                               (jetpacs-action "orgzly.book.select-mode"
                                            :when-offline "drop")
                               :content-description "Select notes"))
   :fab (jetpacs-fab "add" :on-tap (jetpacs-action
                                 "orgzly.note.new"
                                 :args `((book . ,orgzly-ui--current-book))))
   :snackbar snackbar))

;; ─── Note editor view ────────────────────────────────────────────────────────

(defun orgzly-ui--state-chips (entry)
  (let* ((kws (orgzly-data-todo-keywords))
         (ref (orgzly-data-entry-ref entry))
         (current (alist-get 'state entry)))
    (apply #'jetpacs-scroll-row
           (cons
            (jetpacs-chip "NONE" :selected (null current)
                       :on-tap (orgzly-ui--note-action
                                "orgzly.note.set-state" ref '(state . "")))
            (mapcar (lambda (kw)
                      (jetpacs-chip kw :selected (equal kw current)
                                 :on-tap (orgzly-ui--note-action
                                          "orgzly.note.set-state" ref
                                          (cons 'state kw))))
                    (append (car kws) (cdr kws)))))))

(defun orgzly-ui--priority-chips (entry)
  (let* ((ref (orgzly-data-entry-ref entry))
         (current (alist-get 'priority entry))
         (letters (cl-loop for c from org-priority-highest to org-priority-lowest
                           collect (char-to-string c))))
    (apply #'jetpacs-scroll-row
           (cons
            (jetpacs-chip "No priority" :selected (null current)
                       :on-tap (orgzly-ui--note-action
                                "orgzly.note.set-priority" ref '(priority . "")))
            (mapcar (lambda (p)
                      (jetpacs-chip (concat "#" p) :selected (equal p current)
                                 :on-tap (orgzly-ui--note-action
                                          "orgzly.note.set-priority" ref
                                          (cons 'priority p))))
                    letters)))))

(defun orgzly-ui--planning-row (entry kind label)
  "One planning line editor: date, time, repeater, clear."
  (let* ((ref (orgzly-data-entry-ref entry))
         (ts (alist-get kind entry))
         (time (and ts (alist-get 'time ts)))
         (kind-arg (cons 'kind (symbol-name kind))))
    (apply #'jetpacs-row
           (append
            (list
             (jetpacs-text label 'label nil orgzly-ui--meta-color)
             (jetpacs-spacer :width 8)
             (jetpacs-date-button
              (if time (format-time-string "%Y-%m-%d" (seconds-to-time time)) "Date")
              (orgzly-ui--note-action "orgzly.note.plan-date" ref kind-arg)
              :value (and time (format-time-string "%Y-%m-%d" (seconds-to-time time))))
             (jetpacs-time-button
              (if (and ts (alist-get 'has-time ts))
                  (format-time-string "%H:%M" (seconds-to-time time))
                "Time")
              (orgzly-ui--note-action "orgzly.note.plan-time" ref kind-arg)
              :value (and ts (alist-get 'has-time ts)
                          (format-time-string "%H:%M" (seconds-to-time time))))
             (jetpacs-button (or (and ts (alist-get 'repeater ts)) "Repeat")
                          (orgzly-ui--note-action "orgzly.note.plan-repeater" ref kind-arg)
                          :variant "text")
             (jetpacs-spacer :weight 1.0))
            (when ts
              (list (jetpacs-icon-button
                     "close"
                     (orgzly-ui--note-action "orgzly.note.plan-clear" ref kind-arg)
                     :content-description (concat "Clear " label))))
            (list :align "center")))))

(defun orgzly-ui--known-tags ()
  "Every tag in use across books, plus configured favourites."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (e (orgzly-data-entries))
      (dolist (tag (alist-get 'tags e)) (puthash (substring-no-properties tag) t tags)))
    (dolist (ta org-tag-alist)
      (when (and (consp ta) (stringp (car ta)))
        (puthash (car ta) t tags)))
    (sort (hash-table-keys tags) #'string-lessp)))

(defun orgzly-ui--properties-card (entry)
  (let* ((ref (orgzly-data-entry-ref entry))
         (props (ignore-errors (orgzly-data-properties ref))))
    (jetpacs-card
     (list
      (jetpacs-column
       (apply #'jetpacs-column
              (mapcar (lambda (p)
                        (jetpacs-row
                         (jetpacs-text (car p) 'label nil orgzly-ui--meta-color)
                         (jetpacs-spacer :width 8)
                         (jetpacs-text (cdr p) 'body nil nil t)
                         (jetpacs-spacer :weight 1.0)
                         (jetpacs-icon-button
                          "close"
                          (orgzly-ui--note-action "orgzly.note.del-property" ref
                                                  (cons 'name (car p)))
                          :content-description "Delete property")
                         :align "center"))
                      props))
       (jetpacs-button "Add property"
                    (orgzly-ui--note-action "orgzly.note.add-property" ref)
                    :icon "add" :variant "text"))))))

(defun orgzly-ui--note-body ()
  (let ((entry (orgzly-ui--entry-for-ref orgzly-ui--note-ref)))
    (if (null entry)
        (jetpacs-empty-state :icon "error" :title "Note not found"
                          :caption "It may have been moved or deleted")
      (let ((ref (orgzly-data-entry-ref entry)))
        (jetpacs-lazy-column
         (jetpacs-card
          (list (jetpacs-column
                 (orgzly-ui--title-node entry)
                 (jetpacs-text (format "%s  ·  level %d"
                                    (alist-get 'book entry)
                                    (alist-get 'level entry))
                            'caption nil orgzly-ui--meta-color)))
          :on-tap (orgzly-ui--note-action "orgzly.note.rename" ref))
         (jetpacs-section-header "State")
         (orgzly-ui--state-chips entry)
         (jetpacs-section-header "Priority")
         (orgzly-ui--priority-chips entry)
         (jetpacs-section-header "Planning")
         (jetpacs-card
          (list (apply #'jetpacs-column
                       (delq nil
                             (list
                              (orgzly-ui--planning-row entry 'scheduled "Scheduled")
                              (orgzly-ui--planning-row entry 'deadline "Deadline")
                              (when-let ((closed (alist-get 'closed entry)))
                                (jetpacs-text (concat "Closed: "
                                                   (orgzly-ui--ts-label closed))
                                           'caption nil orgzly-ui--meta-color))
                              (when-let ((created (alist-get 'created entry)))
                                (jetpacs-text (concat "Created: "
                                                   (orgzly-ui--ts-label created))
                                           'caption nil orgzly-ui--meta-color)))))))
         (jetpacs-section-header "Tags")
         (jetpacs-enum-list "orgzly-note-tags"
                         (orgzly-ui--known-tags)
                         :value (mapcar #'substring-no-properties
                                        (alist-get 'tags entry))
                         :multi-select t :allow-add t
                         :on-change (orgzly-ui--note-action
                                     "orgzly.note.set-tags" ref))
         (jetpacs-section-header "Properties")
         (orgzly-ui--properties-card entry)
         (jetpacs-section-header "Content")
         (jetpacs-editor (format "orgzly:%s:%s" (alist-get 'file entry)
                              (alist-get 'pos entry))
                      (alist-get 'content entry)
                      :on-save (orgzly-ui--note-action "orgzly.note.set-content" ref)
                      :syntax "org" :toolbar "org" :chromeless t))))))

(defun orgzly-ui--note-view (snackbar)
  (let ((ref orgzly-ui--note-ref))
    (jetpacs-shell-nav-view
     "Note"
     (orgzly-ui--note-body)
     :back-to (if orgzly-ui--current-book "book" "books")
     :actions (delq nil
                    (list
                     (jetpacs-icon-button "edit"
                                       (orgzly-ui--note-action "orgzly.note.rename" ref)
                                       :content-description "Rename")
                     (and ref (orgzly-ui--note-menu
                               (or (orgzly-ui--entry-for-ref ref) ref)))))
     :snackbar snackbar)))

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
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (new (read-string "Title: " (alist-get 'title ref))))
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

;; Planning editors.  The date/time buttons inject the picked value into
;; args as `value'; repeater goes through a bridged prompt.

(defun orgzly-ui--plan-parts (ref kind)
  "Current (DATE TIME REPEATER) strings of REF's KIND planning, or nils."
  (let* ((entry (orgzly-ui--entry-for-ref ref))
         (ts (and entry (alist-get (intern kind) entry)))
         (time (and ts (alist-get 'time ts))))
    (list (and time (format-time-string "%Y-%m-%d" (seconds-to-time time)))
          (and ts (alist-get 'has-time ts)
               (format-time-string "%H:%M" (seconds-to-time time)))
          (and ts (alist-get 'repeater ts)))))

(defun orgzly-ui--plan-apply (ref kind date time repeater)
  "Write the planning string assembled from DATE/TIME/REPEATER onto REF."
  (orgzly-data-set-planning
   ref (intern kind)
   (and date (string-join (delq nil (list date time repeater)) " ")))
  (orgzly-ui--after-mutation nil))

(jetpacs-defaction "orgzly.note.plan-date"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind)))
      (orgzly-ui--plan-apply ref kind (alist-get 'value args)
                             (nth 1 parts) (nth 2 parts)))))

(jetpacs-defaction "orgzly.note.plan-time"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind))
           (date (or (nth 0 parts) (format-time-string "%Y-%m-%d"))))
      (orgzly-ui--plan-apply ref kind date (alist-get 'value args)
                             (nth 2 parts)))))

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
                             (unless (string-empty-p rep) rep)))))

(jetpacs-defaction "orgzly.note.plan-clear"
  (lambda (args _)
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
