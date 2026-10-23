;;; orgzly.el --- Orgzly Native for current Jetpacs -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Trusted applet entrypoint.  Org files stay in the configured notebook root.
;; Navigation is deferred; submitted file effects complete before acceptance.
;;; Code:
(require 'orgzly-ui)
(require 'jetpacs-apps)
(require 'jetpacs-shell)

(defvar orgzly--timers nil "Pending app-owned navigation and refresh timers.")

(defun orgzly--later (fn)
  "Schedule app-owned FN through Jetpacs' device-flow boundary."
  (setq orgzly--timers (cl-remove-if-not #'timerp orgzly--timers))
  (let (timer)
    (setq timer (jetpacs-flow-continue
                 (lambda ()
                   (setq orgzly--timers (delq timer orgzly--timers))
                   (condition-case err (funcall fn)
                     (error (message "Orgzly presentation: %s" (error-message-string err)))))))
    (push timer orgzly--timers)))

(defun orgzly--open (route &optional child)
  "Schedule ROUTE on Orgzly's surface; CHILD retains the current parent screen."
  (setq orgzly-state-page 0)
  ;; The data document follows the outline screen on the stack.
  (setq orgzly-state-outline
        (pcase (plist-get route :kind)
          ('book (plist-get route :book))
          (_ (and child orgzly-state-outline))))
  (orgzly--later
   (lambda ()
     (with-jetpacs-owner "orgzly"
       (unless child (jetpacs-chrome-reset-screens "orgzly" t))
       (if (eq (plist-get route :kind) 'books) (jetpacs-shell-push "orgzly")
         (jetpacs-chrome-push-screen
          "orgzly" (concat "oz-" (symbol-name (plist-get route :kind)))
          (lambda (back) (orgzly-ui-screen route back)))))))
  'accepted)

(defun orgzly--refresh-presentation ()
  "Refresh the data now and defer only presentation work."
  (orgzly-refresh)
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))))

(defun orgzly--committed (route)
  "Refresh after a committed effect and present ROUTE without changing its outcome.
A failed refresh cannot turn an already saved edit into a rejected occurrence."
  (condition-case err
      (progn (orgzly-refresh) (orgzly--open route))
    (error (message "Orgzly saved; refresh needed: %s" (error-message-string err))))
  'accepted)

(defun orgzly--on-home (_args _params)
  "Refresh trusted notebooks and open the app's home destination."
  (orgzly--guard (lambda () (orgzly-refresh) (orgzly--open '(:kind books)))))

(defun orgzly--guard (fn)
  "Run submitted FN with closed stale/refused outcomes and no hidden prompts."
  (condition-case err
      (funcall fn)
    (orgzly-stale 'stale)
    (error (message "Orgzly: %s" (error-message-string err)) 'rejected)))

(defun orgzly--field (params id &optional max)
  "Read the required captured field ID from PARAMS, bounded by MAX bytes."
  (let ((value (plist-get (plist-get params :fields) (intern (concat ":" (orgzly-field-id id))))))
    (unless (and (stringp value) (<= (string-bytes value) (or max 2048)))
      (error "Missing or oversized field %s" id))
    value))

(defun orgzly--on-open (args _params)
  "Open the fixed app destination requested by ARGS."
  (let ((kind (cdr (assoc (plist-get args :view)
                          '(("books" . books) ("agenda" . agenda) ("search" . search)
                            ("settings" . settings) ("new-book" . new-book)
                            ("save-search" . save-search))))))
    (if kind (orgzly--open (list :kind kind)) 'rejected)))

(defun orgzly--on-refresh (_args _params)
  "Refresh all bounded notebooks and schedule presentation."
  (orgzly--guard (lambda () (orgzly--refresh-presentation) 'accepted)))

(defun orgzly--on-book-open (args _params)
  "Open the current notebook identified by opaque ARGS."
  (orgzly--guard
   (lambda ()
     (let ((book (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'name book) (error "Expected a notebook"))
       (orgzly--open (list :kind 'book :book (alist-get 'name book)))))))

(defun orgzly--on-book-edit (args _params)
  "Open metadata for the notebook identified by ARGS."
  (orgzly--guard
   (lambda ()
     (let ((book (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'name book) (error "Expected a notebook"))
       (orgzly--open (list :kind 'book-edit :id (alist-get 'id book)) t)))))

(defun orgzly--on-note-open (args _params)
  "Open the exact current note identified by ARGS.
A literal row names the note in `:id'; a templated outline row carries
no id of its own and the Companion injects the item's key as `:item'."
  (orgzly--guard
   (lambda ()
     (let ((entry (orgzly-state-resolve (or (plist-get args :id) (plist-get args :item)) t)))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (orgzly--open (list :kind 'note :id (alist-get 'id entry)) t)))))

(defun orgzly--on-note-new (args _params)
  "Open a new-note form in the current notebook from ARGS."
  (orgzly--guard
   (lambda ()
     (let ((book (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'name book) (error "Expected a notebook"))
       (orgzly--open (list :kind 'new-note :book (alist-get 'name book)
                           :book-id (alist-get 'id book)) t)))))

(defun orgzly--on-note-child (args _params)
  "Open a child-note form under the current note from ARGS."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id) t))
            (book (orgzly-state-book (alist-get 'book entry))))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (orgzly--open (list :kind 'new-note :book (alist-get 'name book)
                           :book-id (alist-get 'id book) :parent-id (alist-get 'id entry)) t)))))

(defun orgzly--outline-shape ()
  "Return what decides the outline screen's shape beyond its rows."
  (let ((count (length (orgzly-state-visible (orgzly-state-entries orgzly-state-outline)))))
    (list (car (orgzly-ui-page-window count)) (> count orgzly-state-page-size)
          (null (orgzly-state-entries orgzly-state-outline)))))

(defun orgzly--present-outline (shape)
  "Present the outline after a change: patch `/rows' when SHAPE still holds.
SHAPE is `orgzly--outline-shape' from before the change.  Bindings
change values, never the tree: while the pager and the empty notice are
as they were, the rows are synced in place (SPEC 13.8.5): the shell
diffs them by note id against the rows it last sent and patches only the
changed icon and the rows that left or entered (`insert', amendment
#196), or sets `/rows' when that is smaller; otherwise the screen is
re-pushed whole."
  (if (equal shape (orgzly--outline-shape))
      (let ((rows (orgzly-ui-outline-rows orgzly-state-outline)))
        (orgzly--later
         (lambda () (jetpacs-shell-data-sync "orgzly" "/rows" rows :key "id"))))
    (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))))

(defun orgzly--toggle-fold (id)
  "Toggle the fold of note ID in the explicit fold state."
  (setq orgzly-state-folded (if (member id orgzly-state-folded)
                                (delete id orgzly-state-folded) (cons id orgzly-state-folded))))

(defun orgzly--on-note-tap (args _params)
  "Act on the templated outline row whose item key ARGS carries as `:item'.
A note with children folds or unfolds, patching the rows in place; a
leaf opens its note.  The row has one action for every item because a
template's shape cannot vary per row (SPEC 13.8.5)."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :item))) (id (alist-get 'id entry)))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (if (not (alist-get 'has-children entry))
           (orgzly--open (list :kind 'note :id id) t)
         (let ((shape (orgzly--outline-shape)))
           (orgzly--toggle-fold id)
           (orgzly--present-outline shape))
         'accepted)))))

(defun orgzly--on-fold (args _params)
  "Toggle the snapshot note fold named by ARGS without reading disk."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id))) (id (alist-get 'id entry)))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (orgzly--toggle-fold id)
       (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))) 'accepted))))

(defun orgzly--planning (text)
  "Parse validated Org timestamp TEXT into lossless planning parts."
  (unless (string-empty-p text)
    (let* ((raw (if (string-prefix-p "<" text) text (concat "<" text ">")))
           (ts (orgzly-data--parse-ts raw)))
      (unless (and ts (string-match "\\`<\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" raw))
        (error "Use an Org timestamp such as <2026-09-05 Sat 09:00 +1w>"))
      (let ((date (match-string 1 raw)))
        `((date . ,date)
          (time . ,(and (alist-get 'has-time ts) (format-time-string "%H:%M" (alist-get 'time ts))))
          (end . ,(when (alist-get 'end-time ts) (format-time-string "%H:%M" (alist-get 'end-time ts))))
          (repeater . ,(alist-get 'repeater ts)) (warning . ,(alist-get 'warning ts)))))))

(defun orgzly--on-note-save (args params)
  "Commit validated captured PARAMS to ARGS' exact note before acceptance."
  (orgzly--guard
   (lambda ()
     (let* ((id (plist-get args :id))
            (orgzly-field-scope (orgzly-field-scope
                                 (list :kind (if id 'note 'new-note) :id id
                                       :book-id (plist-get args :book-id)
                                       :parent-id (plist-get args :parent-id))))
            (entry (and id (orgzly-state-resolve id t)))
            (book (if entry (orgzly-state-book (alist-get 'book entry))
                    (orgzly-state-resolve (plist-get args :book-id) t)))
            (parent (when (plist-get args :parent-id) (orgzly-state-resolve (plist-get args :parent-id) t)))
            (title (orgzly--field params "oz-title")) (state (orgzly--field params "oz-state"))
            (priority (orgzly--field params "oz-priority")) (tags (orgzly--field params "oz-tags"))
            (content (orgzly--field params "oz-content" 64000))
            (scheduled (orgzly--planning (orgzly--field params "oz-scheduled")))
            (deadline (orgzly--planning (orgzly--field params "oz-deadline")))
            (name (alist-get 'name book)))
       (unless (and name (not (string-empty-p (string-trim title)))
                    (not (string-match-p "[\n\r]" title))
                    (or (string-empty-p priority) (string-match-p "\\`[A-C]\\'" priority))
                    (or (null parent) (equal name (alist-get 'book parent))))
         (error "Invalid note fields"))
       (with-current-buffer (orgzly-data--book-buffer name)
         (unless (or (string-empty-p state) (member state org-todo-keywords-1))
           (error "Unknown Org TODO state"))
         (atomic-change-group
           (let ((orgzly-data--transaction t))
             (unless entry
               (setq entry (orgzly-data-new-note name title :under parent)))
             (orgzly-data-set-content entry content)
             (orgzly-data-set-tags entry (split-string tags "[ :]+" t))
             (orgzly-data-set-priority entry (unless (string-empty-p priority) priority))
             (orgzly-data-set-planning entry 'scheduled scheduled)
             (orgzly-data-set-planning entry 'deadline deadline)
             (orgzly-data-set-state entry state)
             (orgzly-data-set-title entry title))
           (save-buffer)))
       (orgzly--committed (list :kind 'book :book name))))))

(defun orgzly--on-operate (args _params)
  "Commit the closed structural operation in ARGS to its current note."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id) t))
            (op (plist-get args :operation)) (book (alist-get 'book entry))
            (fn (cdr (assoc op '(("toggle" . orgzly-data-toggle-done)
                                 ("promote" . orgzly-data-promote) ("demote" . orgzly-data-demote)
                                 ("up" . orgzly-data-move-up) ("down" . orgzly-data-move-down)
                                 ("copy" . orgzly-data-copy-note) ("delete" . orgzly-data-delete-note))))))
       (unless book (error "Expected a note"))
       (cond (fn (funcall fn entry))
             ((equal op "paste") (orgzly-data-paste-note book entry))
             (t (error "Unsupported note operation")))
       (orgzly--committed (list :kind 'book :book book))))))

(defun orgzly--on-book-save (args params)
  "Save captured notebook PARAMS against ARGS' revision, or create a new book."
  (orgzly--guard
   (lambda ()
     (let* ((id (plist-get args :id))
            (orgzly-field-scope (orgzly-field-scope (list :kind (if id 'book-edit 'new-book) :id id)))
            (book (and id (orgzly-state-resolve id t)))
            (name (orgzly--field params "oz-name" 512))
            (preface (orgzly--field params "oz-preface" 64000))
            (file (orgzly-data-book-file name)))
       (when (string-match-p "^\\*+ " preface) (error "Preface cannot contain headings"))
       ;; Rename is a separate atomic filesystem effect; do not mix it with
       ;; content editing under one accepted occurrence.
       (cond
        ((and book (not (equal name (alist-get 'name book))))
         (unless (equal preface (alist-get 'preface book))
           (error "Save preface changes before renaming the notebook"))
         (orgzly-data-rename-book (alist-get 'name book) name))
        ((null book)
         (when (file-exists-p file) (error "Notebook already exists"))
         (make-directory (file-name-directory file) t)
         (write-region preface nil file nil 'silent nil 'excl))
        (t
         (with-current-buffer (orgzly-data--book-buffer name)
           (atomic-change-group
             (goto-char (point-min))
             (let ((end (if (re-search-forward org-outline-regexp-bol nil t)
                            (line-beginning-position) (point-max))))
               (delete-region (point-min) end)
               (goto-char (point-min))
               (insert (string-trim-right preface))
               (unless (bolp) (insert "\n"))
               (save-buffer))))))
       (orgzly--committed (list :kind 'book :book name))))))

(defun orgzly--on-book-delete (args _params)
  "Delete ARGS' exact notebook after Companion confirmation."
  (orgzly--guard
   (lambda ()
     (let ((book (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'name book) (error "Expected a notebook"))
       (orgzly-data-delete-book (alist-get 'name book))
       (orgzly--committed '(:kind books))))))

(defun orgzly--check-query (query)
  "Validate bounded QUERY, including a maximum 31-day agenda horizon."
  (unless (and (stringp query) (<= (string-bytes query) 512)) (error "Query is too long"))
  (let ((days (plist-get (orgzly-query-parse query) :agenda-days)))
    (when (and days (not (<= 1 days 31))) (error "Agenda horizon must be 1–31 days")))
  query)

(defun orgzly--on-search (args params)
  "Run ARGS' injected query or captured PARAMS against the snapshot."
  (orgzly--guard
   (lambda ()
     (let ((orgzly-field-scope (orgzly-field-scope '(:kind search))))
       (setq orgzly-state-query (orgzly--check-query (or (plist-get args :value) (orgzly--field params "oz-query" 512)))))
     (orgzly--open '(:kind search)))))

(defun orgzly--on-saved (args _params)
  "Run the bounded saved query named by ARGS."
  (orgzly--guard
   (lambda ()
     (let ((query (cdr (assoc (plist-get args :name) (plist-get orgzly-state :searches)))))
       (unless query (signal 'orgzly-stale nil))
       (setq orgzly-state-query (orgzly--check-query query))
       (orgzly--open '(:kind search))))))

(defun orgzly--save-searches (searches)
  "Persist validated SEARCHES before replacing the live snapshot."
  (unless (<= (length searches) 32) (error "At most 32 saved searches"))
  (customize-save-variable 'orgzly-saved-searches searches)
  (setq orgzly-state (plist-put (copy-sequence orgzly-state) :searches (copy-tree searches))))

(defun orgzly--on-search-save (_args params)
  "Persist a named dotted query captured in PARAMS."
  (orgzly--guard
   (lambda ()
     (let* ((orgzly-field-scope (orgzly-field-scope '(:kind save-search)))
            (name (orgzly--field params "oz-name" 128))
            (query (orgzly--check-query (orgzly--field params "oz-query" 512)))
            (searches (copy-tree orgzly-saved-searches)))
       (when (string-empty-p (string-trim name)) (error "Search name is required"))
       (setf (alist-get name searches nil nil #'equal) query)
       (orgzly--save-searches searches)
       (orgzly--open '(:kind search))))))

(defun orgzly--on-search-delete (args _params)
  "Persist removal of the search named by ARGS after confirmation."
  (orgzly--guard
   (lambda ()
     (unless (assoc (plist-get args :name) orgzly-saved-searches) (signal 'orgzly-stale nil))
     (orgzly--save-searches (assoc-delete-all (plist-get args :name) (copy-tree orgzly-saved-searches)))
     (orgzly--open '(:kind search)))))

(defun orgzly--on-page (args _params)
  "Select ARGS' bounded result page and refresh presentation."
  (let ((page (plist-get args :page)))
    (if (not (and (integerp page) (<= 0 page 50))) 'rejected
      (setq orgzly-state-page page)
      (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))) 'accepted)))

(defun orgzly--on-theme (_args _params)
  "Toggle the process-local light/dark Orgzly preset."
  (setq orgzly-state-dark (not orgzly-state-dark))
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))) 'accepted)

(defun orgzly-register ()
  "Register the optional Orgzly applet idempotently without reading notebooks."
  (orgzly-design-register)
  (with-jetpacs-owner "orgzly"
    (jetpacs-chrome-define-root "orgzly" "books"
                                (lambda (back) (orgzly-ui-screen '(:kind books) back)) :required t
                                :data-builder #'orgzly-ui-document)
    (jetpacs-defaction "orgzly.home" #'orgzly--on-home :doc "Refresh and open Orgzly notebooks")
    (jetpacs-defaction "orgzly.open" #'orgzly--on-open :doc "Open an Orgzly destination" :args '((:name view :type "enum" :required t)))
    (jetpacs-defaction "orgzly.refresh" #'orgzly--on-refresh :doc "Refresh the bounded Orgzly notebook snapshot")
    (jetpacs-defaction "orgzly.book.open" #'orgzly--on-book-open :doc "Open the notebook represented by an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.book.edit" #'orgzly--on-book-edit :doc "Open notebook metadata for an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.book.save" #'orgzly--on-book-save :doc "Commit captured notebook metadata or create a notebook" :args '((:name id :type "ref")))
    (jetpacs-defaction "orgzly.book.delete" #'orgzly--on-book-delete :doc "Delete a confirmed current notebook" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.open" #'orgzly--on-note-open :doc "Open the note represented by an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.new" #'orgzly--on-note-new :doc "Open a new-note form in the current notebook" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.child" #'orgzly--on-note-child :doc "Open a child-note form under the current note" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.fold" #'orgzly--on-fold :doc "Toggle the current outline note's descendants" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.tap" #'orgzly--on-note-tap :doc "Fold or unfold the tapped outline note, or open a leaf; the row's item key is injected")
    (jetpacs-defaction "orgzly.note.save" #'orgzly--on-note-save :doc "Commit captured note fields before accepting the edit" :args '((:name id :type "ref") (:name book-id :type "ref") (:name parent-id :type "ref")))
    (jetpacs-defaction "orgzly.note.operate" #'orgzly--on-operate :doc "Commit a closed structural operation to the current note" :args '((:name id :type "ref" :required t) (:name operation :type "enum" :required t)))
    (jetpacs-defaction "orgzly.search.run" #'orgzly--on-search :doc "Run the captured bounded dotted query")
    (jetpacs-defaction "orgzly.search.saved" #'orgzly--on-saved :doc "Run a named saved query" :args '((:name name :type "string" :required t)))
    (jetpacs-defaction "orgzly.search.save" #'orgzly--on-search-save :doc "Persist a captured named query")
    (jetpacs-defaction "orgzly.search.delete" #'orgzly--on-search-delete :doc "Remove a confirmed saved query" :args '((:name name :type "string" :required t)))
    (jetpacs-defaction "orgzly.page" #'orgzly--on-page :doc "Select a bounded outline or search page" :args '((:name page :type "integer" :required t)))
    (jetpacs-defaction "orgzly.theme" #'orgzly--on-theme :doc "Toggle Orgzly light and dark appearance"))
  (jetpacs-defapp "orgzly" :label "Orgzly" :icon "book" :surfaces '("orgzly")
                  :chrome 'primary :dock-core nil :home-route "books" :order 46
                  :destinations '((:key "books" :label "Notebooks" :icon "book"
                                    :verb "orgzly.home"))
                  :requires-extensions '("jetpacs.components" "jetpacs.design"))
  t)

(defun orgzly-unregister ()
  "Remove Orgzly's registrations, timers, style presets and private state.
`jetpacs-apps-unregister' sweeps every action, root and app entry
attributed to the owner."
  (dolist (timer orgzly--timers) (when (timerp timer) (cancel-timer timer)))
  (setq orgzly--timers nil orgzly-state nil orgzly-state-folded nil
        orgzly-state-page 0 orgzly-state-query "" orgzly-state-outline nil)
  (orgzly-data-invalidate)
  (jetpacs-apps-unregister "orgzly")
  (dolist (id '("orgzly.light" "orgzly.dark")) (jetpacs-design-unregister-profile id))
  t)

;;;###autoload
(defun orgzly ()
  "Refresh trusted local notebooks and open Orgzly on the connected Companion."
  (interactive)
  (orgzly-register) (orgzly-refresh)
  (jetpacs-apps-seed-current "orgzly" "books")
  (jetpacs-chrome-reset-screens "orgzly"))

(defun orgzly-unload-function ()
  "Remove Orgzly's live registrations before feature unloading."
  (orgzly-unregister) nil)

(orgzly-register)
(provide 'orgzly)
;;; orgzly.el ends here
