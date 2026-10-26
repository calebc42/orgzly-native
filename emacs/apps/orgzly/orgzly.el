;;; orgzly.el --- Orgzly Native for current Jetpacs -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Trusted applet entrypoint.  Org files stay in the configured notebook root.
;; Navigation is deferred; submitted file effects complete before acceptance.
;; `require' only loads; the `orgzly' command or the Apps screen registers.
;;; Code:
(require 'orgzly-ui)
(require 'orgzly-reminders)
(require 'jetpacs-apps)
(require 'jetpacs-shell)
(require 'jetpacs-dialog-slot)
(declare-function ebp-client-dialog-show "ebp")

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
  (setq orgzly-state-page 0 orgzly-state-searching nil)
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
                          '(("books" . books) ("searches" . searches) ("search" . searches)
                            ("settings" . settings) ("new-book" . new-book)
                            ("save-search" . save-search))))))
    (cond
     ((and (equal (plist-get args :view) "settings") (plist-get args :page))
      (if (assoc (plist-get args :page) orgzly-ui-settings)
          (orgzly--open (list :kind 'settings :page (plist-get args :page)) t)
        'rejected))
     ((equal (plist-get args :view) "agenda")
      (orgzly--open (list :kind 'query :name "Agenda" :query ".it.done ad.7")))
     (kind (orgzly--open (list :kind kind)))
     (t 'rejected))))

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
  "Open the exact current note identified by ARGS' `:id'."
  (orgzly--guard
   (lambda ()
     (let ((entry (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (setq orgzly-state-draft nil orgzly-state-editing nil)
       (orgzly--open (list :kind 'note :id (alist-get 'id entry)) t)))))

(defun orgzly--on-note-new (args _params)
  "Open a new-note form in the current notebook from ARGS."
  (orgzly--guard
   (lambda ()
     (let ((book (orgzly-state-resolve (plist-get args :id) t)))
       (unless (alist-get 'name book) (error "Expected a notebook"))
       ;; A new note starts empty, as Revived's does.
       (setq orgzly-state-draft nil orgzly-state-editing nil)
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

(defun orgzly--present-outline ()
  "Re-push the outline after a fold change."
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))))

(defun orgzly--toggle-fold (id)
  "Toggle the fold of note ID in the explicit fold state."
  (setq orgzly-state-folded (if (member id orgzly-state-folded)
                                (delete id orgzly-state-folded) (cons id orgzly-state-folded))))

(defun orgzly--on-fold (args _params)
  "Toggle the snapshot note fold named by ARGS without reading disk."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id)))
            (id (alist-get 'id entry)))
       (unless (alist-get 'pos entry) (error "Expected a note"))
       (orgzly--toggle-fold id)
       (orgzly--present-outline)
       'accepted))))

(defun orgzly--on-link-open (args _params)
  "Open the link ARGS name by note, field and index in the snapshot.
The device never sends a target: it names the note's `:id', the
`:field' (\"title\" or \"content\") and the `:link' index, and the
target is read back from the snapshot.  Only web and mail addresses
open, through `browse-url'."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id)))
            (text (pcase (plist-get args :field)
                    ("title" (alist-get 'title entry))
                    ("content" (string-trim (or (alist-get 'content entry) "")))))
            (index (plist-get args :link))
            (links (and text (seq-filter #'cdr (orgzly-ui-link-segments text))))
            (target (and (natnump index) (cdr (nth index links)))))
       (if (and target (string-match-p "\\`\\(?:https?\\|mailto\\):" target))
           (progn (browse-url target) 'accepted)
         'rejected)))))

(defun orgzly--on-fold-all (_args _params)
  "Cycle the open outline as Revived's Fold/Unfold All does.
Anything folded unfolds everything; otherwise every foldable note folds."
  (orgzly--guard
   (lambda ()
     (let ((entries (orgzly-state-entries orgzly-state-outline)))
       (setq orgzly-state-folded
             (if (cl-some (lambda (e) (member (alist-get 'id e) orgzly-state-folded)) entries)
                 (cl-remove-if (lambda (id) (cl-find id entries :key (lambda (e) (alist-get 'id e))
                                                      :test #'equal))
                               orgzly-state-folded)
               (append (mapcar (lambda (e) (alist-get 'id e))
                               (cl-remove-if-not #'orgzly-ui--foldable-p entries))
                       orgzly-state-folded)))
       (orgzly--present-outline)
       'accepted))))

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
            (title (or (orgzly--field-maybe params "oz-title") (orgzly-ui-note-text "title" entry) ""))
            (state (orgzly--note-input params "state" entry))
            (priority (orgzly--note-input params "priority" entry))
            ;; A tags field off the screen keeps the note's tags.
            (tags (if (orgzly-ui-note-field-shown-p 'tags entry)
                      (orgzly--field params "oz-tags")
                    (orgzly-ui-note-tags entry)))
            (content (let ((v (plist-get (plist-get params :fields)
                                         (intern (concat ":" (orgzly-field-id "oz-content"))))))
                       (if (and (stringp v) (<= (string-bytes v) 64000)) v
                         (or (orgzly-ui-note-text "content" entry) ""))))
            (scheduled (orgzly--planning (orgzly--note-input params "scheduled" entry)))
            (deadline (orgzly--planning (orgzly--note-input params "deadline" entry)))
            (prop-name (string-trim (or (orgzly--field-maybe params "oz-prop-name") "")))
            (prop-value (string-trim (or (orgzly--field-maybe params "oz-prop-value") "")))
            (name (alist-get 'name book)))
       (unless (and name (not (string-empty-p (string-trim title)))
                    (not (string-match-p "[\n\r]" title))
                    (or (string-empty-p priority) (string-match-p "\\`[A-C]\\'" priority))
                    (or (null parent) (equal name (alist-get 'book parent)))
                    (or (string-empty-p prop-name)
                        (string-match-p "\\`[^[:space:]:]+\\'" prop-name)))
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
             (orgzly-data-set-title entry title)
             (unless (string-empty-p prop-name)
               (orgzly-data-set-property entry prop-name prop-value)))
           (save-buffer)))
       (setq orgzly-state-draft nil orgzly-state-editing nil)
       (orgzly--committed (list :kind 'book :book name))))))

(defun orgzly--note-modified-p (args params)
  "Whether the note form of ARGS differs from the note, as PARAMS capture it.
Typed fields come from PARAMS, picked ones from the draft; a new note is
modified once anything is filled in."
  (let* ((id (plist-get args :id))
         (orgzly-field-scope (orgzly-field-scope
                              (list :kind (if id 'note 'new-note) :id id
                                    :book-id (plist-get args :book-id)
                                    :parent-id (plist-get args :parent-id))))
         (entry (and id (orgzly-state-resolve id t)))
         (text (lambda (field)
                 (or (orgzly--field-maybe params (concat "oz-" field))
                     (orgzly-ui-note-text field entry) ""))))
    (or (not (equal (funcall text "title") (or (alist-get 'title entry) "")))
        (not (equal (funcall text "content") (or (alist-get 'content entry) "")))
        (not (equal (split-string (or (orgzly--field-maybe params "oz-tags")
                                      (orgzly-ui-note-tags entry))
                                  "[ :]+" t)
                    (alist-get 'tags entry)))
        (not (string-blank-p (or (orgzly--field-maybe params "oz-prop-name") "")))
        (cl-some (lambda (field)
                   (not (equal (orgzly-ui-note-value field entry)
                               (let ((orgzly-state-draft nil)) (orgzly-ui-note-value field entry)))))
                 '("state" "priority" "scheduled" "deadline")))))

(defun orgzly--leave-note ()
  "Drop the note form's edits and return to the screen below it."
  (setq orgzly-state-draft nil orgzly-state-editing nil)
  (orgzly--later (lambda () (with-jetpacs-owner "orgzly" (jetpacs-chrome-pop-screen "orgzly")))))

(defun orgzly--on-note-back (args params)
  "Leave the note form of ARGS, asking first when PARAMS show an edit.
Revived's prompt: Save commits as Done does, Discard drops the edit."
  (orgzly--guard
   (lambda ()
     (if (not (orgzly--note-modified-p args params))
         (orgzly--leave-note)
       (with-jetpacs-owner "orgzly"
         (jetpacs-dialog-slot-show
          (jetpacs-wire-id "modified" (or (plist-get args :id) "new"))
          (orgzly-design-wrap-dialog (orgzly-ui-modified-dialog) orgzly-state-dark)
          :on-conclude
          (lambda (status result _error)
            (when (equal status "submitted")
              (pcase (plist-get result :value)
                ("save" (orgzly--on-note-save args params))
                ("discard" (orgzly--leave-note))))))))
     'accepted)))

(defun orgzly--field-maybe (params id)
  "Return captured field ID from PARAMS, or nil when it was not captured."
  (let ((value (plist-get (plist-get params :fields)
                          (intern (concat ":" (orgzly-field-id id))))))
    (and (stringp value) (<= (string-bytes value) 2048) value)))

(defun orgzly--note-input (params field entry)
  "FIELD of the note being saved: `oz-FIELD' captured in PARAMS, else the draft.
Without either it is ENTRY's own value, so an untouched field is kept."
  (or (orgzly--field-maybe params (concat "oz-" field))
      (orgzly-ui-note-value field entry)))

(defvar orgzly--reset-fields nil
  "Logical note fields the next draft push re-seeds from the draft.")

(defun orgzly--draft-put (scope field value)
  "Record picked FIELD VALUE for the note form SCOPE and re-present it.
Fields in `orgzly--reset-fields' drop their typed drafts for the new value."
  (unless (equal (plist-get orgzly-state-draft :scope) scope)
    (setq orgzly-state-draft (list :scope scope)))
  (setq orgzly-state-draft (plist-put orgzly-state-draft (intern (concat ":" field)) value))
  (let ((reset (let ((orgzly-field-scope scope)) (mapcar #'orgzly-field-id orgzly--reset-fields))))
    (setq orgzly--reset-fields nil)
    (orgzly--later (lambda () (jetpacs-shell-push "orgzly" :reset-input-ids reset)))))

(defun orgzly--on-note-section (args _params)
  "Fold or unfold the note screen's ARGS `:section'."
  (let ((section (pcase (plist-get args :section) ("metadata" 'metadata) ("content" 'content))))
    (if (null section) 'rejected
      (setq orgzly-state-note-folds
            (if (memq section orgzly-state-note-folds) (delq section orgzly-state-note-folds)
              (cons section orgzly-state-note-folds)))
      (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))
      'accepted)))

(defun orgzly--on-note-metadata (args _params)
  "Set Revived's metadata visibility from ARGS.
`:show' is all or selected; `:always-set' toggles showing every row that
has a value."
  (let ((show (plist-get args :show)))
    (cond
     ((member show '("all" "selected"))
      (setq orgzly-state-metadata-visibility (intern show)))
     ((plist-get args :always-set)
      (setq orgzly-state-metadata-always-set (not orgzly-state-metadata-always-set)))
     (t (setq show 'rejected)))
    (if (eq show 'rejected) 'rejected
      (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))
      'accepted)))

(defun orgzly--on-note-edit (args _params)
  "Switch ARGS' `:field' of the open note form `:scope' to in-place editing."
  (let ((field (plist-get args :field)) (scope (plist-get args :scope)))
    (if (not (and (member field '("title" "content")) (stringp scope))) 'rejected
      (unless (equal (plist-get orgzly-state-editing :scope) scope)
        (setq orgzly-state-editing (list :scope scope :fields nil)))
      (cl-pushnew field (plist-get orgzly-state-editing :fields) :test #'equal)
      (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))
      'accepted)))

(defun orgzly--on-note-clear (args _params)
  "Clear the picked value ARGS name in the open note's draft."
  (let ((field (plist-get args :field)) (scope (plist-get args :scope)))
    (if (not (and (member field '("state" "priority" "scheduled" "deadline" "tags")) (stringp scope)))
        'rejected
      ;; Tags are typed, so clearing them replaces the field's draft too.
      (when (equal field "tags") (setq orgzly--reset-fields '("oz-tags")))
      (orgzly--draft-put scope field "")
      'accepted)))

(defun orgzly--tags-insert (text tag)
  "TEXT with TAG put in as Revived's tag field does.
A last word TAG begins with is the one being completed, so TAG replaces
it; otherwise TAG is appended.  One space follows, ready for the next."
  (let* ((text (or text ""))
         (start (if (string-match "\\([^ ]*\\)\\'" text) (match-beginning 1) (length text)))
         (word (substring text start)))
    (concat (if (and (not (string-empty-p word))
                     (string-prefix-p (downcase word) (downcase tag)))
                (substring text 0 start)
              (concat (string-trim-right text) (if (string-blank-p text) "" " ")))
            tag " ")))

(defun orgzly--on-note-tag (args params)
  "Put ARGS' `:tag' into the Tags field of form `:scope', captured in PARAMS."
  (let* ((scope (plist-get args :scope)) (tag (plist-get args :tag))
         (orgzly-field-scope scope))
    (if (not (and (stringp scope) (stringp tag) (member tag (orgzly-ui-all-tags)))) 'rejected
      (setq orgzly--reset-fields '("oz-tags"))
      (orgzly--draft-put scope "tags"
                         (orgzly--tags-insert (or (orgzly--field-maybe params "oz-tags")
                                                  (plist-get orgzly-state-draft :tags))
                                              tag))
      'accepted)))

(defun orgzly--pick-choices (field)
  "The choices offered when picking FIELD."
  (pcase field
    ("state" (let ((kws (orgzly-data-todo-keywords))) (append (car kws) (cdr kws))))
    ("priority" (mapcar #'char-to-string (number-sequence org-priority-highest org-priority-lowest)))))

(defun orgzly--on-note-pick (args _params)
  "Open the picker for ARGS' note `:field' in form `:scope'.
State and priority offer their values; scheduled and deadline take an Org
timestamp.  The result lands in the draft, and Done saves it."
  (let* ((field (plist-get args :field)) (scope (plist-get args :scope))
         (title (pcase field ("state" "State") ("priority" "Priority")
                       ("scheduled" "Scheduled time") ("deadline" "Deadline time"))))
    (cond
     ((not (and title (stringp scope))) 'rejected)
     ((member field '("scheduled" "deadline"))
      (orgzly--ts-open scope field (plist-get args :raw))
      'accepted)
     (t
      (with-jetpacs-owner "orgzly"
        (jetpacs-dialog-slot-show
         (jetpacs-wire-id "pick" field)
         (orgzly-design-wrap-dialog
          (orgzly-ui-picker-dialog field title (orgzly--pick-choices field)
                                   (plist-get orgzly-state-draft (intern (concat ":" field))))
          orgzly-state-dark)
         :on-conclude
         (lambda (status result _error)
           (when (equal status "submitted")
             (let ((value (or (plist-get result :value)
                              (plist-get (plist-get result :fields)
                                         (intern (concat ":" (jetpacs-wire-id "pick-ts" field)))))))
               (cond
                ((equal value "__clear__") (orgzly--draft-put scope field ""))
                ((and (member field '("state" "priority"))
                      (member value (orgzly--pick-choices field)))
                 (orgzly--draft-put scope field value))
                ((and (member field '("scheduled" "deadline")) (stringp value)
                      (ignore-errors (orgzly--planning value)))
                 (orgzly--draft-put scope field
                                    (if (string-prefix-p "<" value) value (concat "<" value ">"))))))))))
      'accepted))))

;;; The timestamp dialog

(defconst orgzly--ts-time-re "\\`\\([0-9]\\{1,2\\}\\):\\([0-9][0-9]\\)\\'"
  "A clock time as Org writes it.")
(defconst orgzly--ts-repeater-re "\\`\\(?:\\.\\+\\|\\+\\+\\|\\+\\)[0-9]\\{1,4\\}[hdwmy]\\'"
  "A repeater cookie the dialog writes.")
(defconst orgzly--ts-delay-re "\\`--?[0-9]\\{1,4\\}[hdwmy]\\'"
  "A delay or warning-period cookie the dialog writes.")

(defun orgzly--ts-hm (time)
  "TIME as zero-padded HH:MM, or nil."
  (when (and (stringp time) (string-match orgzly--ts-time-re time))
    (format "%02d:%s" (string-to-number (match-string 1 time)) (match-string 2 time))))

(defun orgzly--ts-seed (raw)
  "The dialog's parts for Org timestamp RAW, or for now when RAW is empty.
Unset parts keep Revived's defaults: the current time, `.+1w' and `-1d',
unchecked."
  (let* ((now (format-time-string "%H:%M"))
         (parts (list :date (format-time-string "%Y-%m-%d") :time now :end now
                      :repeater ".+1w" :delay "-1d")))
    (when (and (stringp raw) (string-match "\\`[<[]\\(.*\\)[]>]\\'" raw))
      (dolist (token (split-string (match-string 1 raw) " " t))
        (cond
         ((string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\'" token)
          (setq parts (plist-put parts :date token)))
         ((string-match "\\`\\([0-9:]+\\)\\(?:-\\([0-9:]+\\)\\)?\\'" token)
          ;; Both groups first: `orgzly--ts-hm' matches again.
          (let* ((groups (list (match-string 1 token) (match-string 2 token)))
                 (start (orgzly--ts-hm (car groups)))
                 (end (orgzly--ts-hm (cadr groups))))
            (when start
              (setq parts (plist-put (plist-put parts :time start) :time-on t))
              (setq parts (plist-put parts :end (or end start)))
              (when end (setq parts (plist-put parts :end-on t))))))
         ((string-match-p orgzly--ts-repeater-re token)
          (setq parts (plist-put (plist-put parts :repeater token) :repeater-on t)))
         ((string-match-p orgzly--ts-delay-re token)
          (setq parts (plist-put (plist-put parts :delay token) :delay-on t))))))
    parts))

(defun orgzly--ts-string (ts)
  "The active Org timestamp TS's checked parts spell."
  (let ((time (encode-time (parse-time-string (concat (plist-get ts :date) " 00:00:00")))))
    (concat "<" (plist-get ts :date) " " (format-time-string "%a" time)
            (when (plist-get ts :time-on)
              (concat " " (plist-get ts :time)
                      (when (plist-get ts :end-on) (concat "-" (plist-get ts :end)))))
            (when (plist-get ts :repeater-on) (concat " " (plist-get ts :repeater)))
            (when (plist-get ts :delay-on) (concat " " (plist-get ts :delay)))
            ">")))

(defun orgzly--ts-merge-checks (fields)
  "Fold the captured checkbox FIELDS into `orgzly-state-ts'."
  (dolist (check orgzly-ui-ts-checks)
    (let ((value (plist-get fields (intern (concat ":" (orgzly-ui-ts-check-id check))))))
      (when (memq value '(t :json-false))
        (setq orgzly-state-ts (plist-put orgzly-state-ts (intern (format ":%s" check))
                                         (eq value t)))))))

(defun orgzly--ts-show ()
  "Show `orgzly-state-ts' as the timestamp dialog, under a fresh id.
A pick re-shows it, so the request it replaces concludes unanswered."
  (let* ((seq (1+ (or (plist-get orgzly-state-ts :seq) 0)))
         (id (jetpacs-wire-id "ts" (number-to-string seq))))
    (setq orgzly-state-ts (plist-put (plist-put orgzly-state-ts :seq seq) :dialog-id id))
    (with-jetpacs-owner "orgzly"
      (jetpacs-dialog-slot-show
       id (orgzly-design-wrap-dialog (orgzly-ui-timestamp-dialog orgzly-state-ts) orgzly-state-dark)
       :on-conclude
       (lambda (status result _error)
         (when (and (equal status "submitted")
                    (equal (plist-get orgzly-state-ts :dialog-id) id))
           (orgzly--ts-merge-checks (plist-get result :fields))
           (orgzly--ts-conclude (plist-get result :value))))))))

(defun orgzly--ts-conclude (value)
  "Apply the timestamp dialog's submitted VALUE: \"set\" or \"clear\"."
  (let* ((ts orgzly-state-ts)
         (field (plist-get ts :field))
         (scope (plist-get ts :scope))
         (stamp (orgzly--ts-string ts)))
    (setq orgzly-state-ts nil)
    (pcase field
      ((or "scheduled" "deadline")
       (orgzly--draft-put scope field (if (equal value "set") stamp "")))
      ((or "title" "content")
       (when (equal value "set")
         (let ((text (or (plist-get ts :text) "")))
           (setq orgzly--reset-fields (list (concat "oz-" field)))
           (orgzly--draft-put scope field
                              (concat text (unless (or (string-empty-p text)
                                                       (string-suffix-p " " text))
                                             " ")
                                      stamp))))))))

(defun orgzly--ts-open (scope field raw &optional text)
  "Open the timestamp dialog for FIELD of form SCOPE, seeded from RAW.
TEXT is the captured text an inserted timestamp is appended to."
  (setq orgzly-state-ts (append (list :scope scope :field field :text text)
                                (orgzly--ts-seed raw)))
  (orgzly--ts-show))

(defun orgzly--on-ts-part (args params)
  "Set ARGS' `:part' of the open timestamp dialog and show it again.
Picks inject `:value'; the shortcuts name their day.  Every action
captures the checkboxes into PARAMS, so a re-show keeps what the user
ticked, and must come from the dialog on screen."
  (let* ((part (plist-get args :part))
         (value (plist-get args :value))
         (today (format-time-string "%Y-%m-%d"))
         (day (lambda (n) (format-time-string "%Y-%m-%d" (time-add nil (days-to-time n))))))
    (cond
     ((null orgzly-state-ts) 'stale)
     ((not (equal (plist-get params :dialog_id) (plist-get orgzly-state-ts :dialog-id))) 'stale)
     (t
      (orgzly--ts-merge-checks (plist-get params :fields))
      (let ((set (lambda (&rest pairs)
                   (cl-loop for (k v) on pairs by #'cddr
                            do (setq orgzly-state-ts (plist-put orgzly-state-ts k v))))))
        (pcase part
          ("date" (if (and (stringp value)
                           (string-match-p "\\`[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]\\'" value))
                      (funcall set :date value)
                    (setq part nil)))
          ("today" (funcall set :date today))
          ("tomorrow" (funcall set :date (funcall day 1)))
          ("week" (funcall set :date (funcall day 7)))
          ;; Picking a time uses it, as Revived's dialog does.
          ("time" (if (orgzly--ts-hm value) (funcall set :time (orgzly--ts-hm value) :time-on t)
                    (setq part nil)))
          ("end" (if (orgzly--ts-hm value) (funcall set :end (orgzly--ts-hm value) :end-on t)
                   (setq part nil)))
          ("checks" nil)
          ((or "repeater" "delay") (orgzly--ts-cookie part) (setq part 'asked))
          (_ (setq part nil))))
      (cond ((null part) 'rejected)
            ((eq part 'asked) 'accepted)
            (t (orgzly--later #'orgzly--ts-show) 'accepted))))))

(defun orgzly--ts-period-parts (part cookie)
  "COOKIE of timestamp PART split as (TYPE VALUE UNIT), with Revived's defaults."
  (if (string-match "\\`\\([.+-]*\\)\\([0-9]+\\)\\([hdwmy]\\)\\'" (or cookie ""))
      (list (match-string 1 cookie) (match-string 2 cookie) (match-string 3 cookie))
    (if (equal part "repeater") '(".+" "1" "w") '("-" "1" "d"))))

(defun orgzly--ts-cookie (part)
  "Pick the timestamp dialog's repeater or delay PART in Revived's period dialog.
It opens over the timestamp dialog, which shows again with the pick."
  (pcase-let* ((key (intern (concat ":" part)))
               (kind (orgzly-ui-period-kind part (plist-get orgzly-state-ts :field)))
               (`(,type ,value ,unit) (orgzly--ts-period-parts part (plist-get orgzly-state-ts key)))
               (type (if (member type (plist-get kind :types)) type (car (plist-get kind :types))))
               (client (jetpacs-client)))
    (when client
      (ebp-client-dialog-show
       client (jetpacs-wire-id "ts-period" part)
       (orgzly-design-wrap-dialog (orgzly-ui-period-dialog kind type value unit nil)
                                  orgzly-state-dark (orgzly-design-period-sheet orgzly-state-dark))
       :callback
       (lambda (status result _error)
         (let* ((fields (plist-get result :fields))
                (type (or (plist-get fields :oz-period-type) type))
                (n (plist-get fields :oz-period-value))
                (unit (car (rassoc (plist-get fields :oz-period-unit) orgzly-ui-period-units)))
                (cookie (and n unit (concat type n unit))))
           (when (and orgzly-state-ts (equal status "submitted") cookie
                      (string-match-p (if (equal part "repeater") orgzly--ts-repeater-re
                                        orgzly--ts-delay-re)
                                      cookie))
             (setq orgzly-state-ts (plist-put (plist-put orgzly-state-ts key cookie)
                                              (intern (concat (symbol-name key) "-on")) t))
             (orgzly--later #'orgzly--ts-show))))))))

(defun orgzly--on-note-timestamp (args params)
  "Insert a timestamp into the edited field of form ARGS' `:scope'.
Revived's toolbar button: content while PARAMS capture it, else the
title, whose captured text the timestamp is appended to."
  (let* ((scope (plist-get args :scope))
         (orgzly-field-scope scope)
         (field (if (orgzly--field-maybe params "oz-content") "content" "title")))
    (if (not (stringp scope)) 'rejected
      (orgzly--ts-open scope field "" (orgzly--field-maybe params (concat "oz-" field)))
      'accepted)))

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
        (t (orgzly--write-preface name preface)))
       (orgzly--committed (list :kind 'book :book name))))))

(defun orgzly--write-preface (name preface)
  "Replace notebook NAME's preface with PREFACE and save it."
  (with-current-buffer (orgzly-data--book-buffer name)
    (atomic-change-group
      (goto-char (point-min))
      (let ((end (if (re-search-forward org-outline-regexp-bol nil t)
                     (line-beginning-position) (point-max))))
        (delete-region (point-min) end)
        (goto-char (point-min))
        (insert (string-trim-right preface))
        (unless (bolp) (insert "\n"))
        (save-buffer)))))

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
     (let ((orgzly-field-scope (or (and (stringp (plist-get args :scope)) (plist-get args :scope))
                                   (orgzly-field-scope '(:kind search)))))
       (setq orgzly-state-query (orgzly--check-query (or (plist-get args :value) (orgzly--field params "oz-query" 512)))))
     (orgzly--open (list :kind 'query :query orgzly-state-query)))))

(defun orgzly--on-saved (args _params)
  "Run the bounded saved query named by ARGS."
  (orgzly--guard
   (lambda ()
     (let ((query (cdr (assoc (plist-get args :name) (plist-get orgzly-state :searches)))))
       (unless query (signal 'orgzly-stale nil))
       (setq orgzly-state-query (orgzly--check-query query))
       (orgzly--open (list :kind 'query :name (plist-get args :name) :query query))))))

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
       (orgzly--open '(:kind searches))))))

(defun orgzly--on-search-delete (args _params)
  "Persist removal of the search named by ARGS after confirmation."
  (orgzly--guard
   (lambda ()
     (unless (assoc (plist-get args :name) orgzly-saved-searches) (signal 'orgzly-stale nil))
     (orgzly--save-searches (assoc-delete-all (plist-get args :name) (copy-tree orgzly-saved-searches)))
     (orgzly--open '(:kind searches)))))

(defun orgzly--setting-item (name)
  "The settings item whose variable is named NAME, or nil."
  (cl-loop for page in orgzly-ui-settings
           thereis (cl-loop for (_header . items) in (nthcdr 3 page)
                            thereis (cl-find-if (lambda (item)
                                                  (and (memq (car item) '(boolean string))
                                                       (equal (symbol-name (nth 2 item)) name)))
                                                items))))

(defun orgzly--save-setting (var value)
  "Save VAR as VALUE through Customize and re-present the screen."
  (customize-save-variable var value)
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly"))))

(defun orgzly--on-setting (args _params)
  "Toggle ARGS' boolean setting, or ask for its new string value.
Only variables listed in variable `orgzly-ui-settings' can be named."
  (let ((item (orgzly--setting-item (plist-get args :name))))
    (pcase item
      ('nil 'rejected)
      (`(boolean ,_ ,var . ,_) (orgzly--save-setting var (not (symbol-value var))) 'accepted)
      (`(string ,title ,var)
       (with-jetpacs-owner "orgzly"
         (jetpacs-dialog-slot-show
          (jetpacs-wire-id "setting" (symbol-name var))
          (orgzly-design-wrap-dialog (orgzly-ui-text-dialog title (format "%s" (symbol-value var)))
                              orgzly-state-dark)
          :on-conclude
          (lambda (status result _error)
            (let ((value (plist-get (plist-get result :fields) :oz-setting)))
              (when (and (equal status "submitted") (stringp value)
                         (<= (length value) 512) (not (string-match-p "[\n\r]" value)))
                (orgzly--save-setting var value))))))
       'accepted))))

(defun orgzly--create-book (name)
  "Create the empty notebook NAME and present the refreshed notebook list."
  (orgzly--guard
   (lambda ()
     (let ((file (orgzly-data-book-file (string-trim name))))
       (when (string-empty-p (string-trim name)) (error "Notebook name is required"))
       (when (file-exists-p file) (error "Notebook already exists"))
       (make-directory (file-name-directory file) t)
       (write-region "" nil file nil 'silent nil 'excl)
       (orgzly--committed '(:kind books))))))

(defun orgzly--on-book-new (_args _params)
  "Ask for a new notebook's name, as Revived's New notebook dialog does."
  (with-jetpacs-owner "orgzly"
    (jetpacs-dialog-slot-show
     "new-book"
     (orgzly-design-wrap-dialog (orgzly-ui-name-dialog "New notebook" "Name" "Create") orgzly-state-dark)
     :on-conclude
     (lambda (status result _error)
       (let ((name (plist-get (plist-get result :fields) :oz-dialog-name)))
         (when (and (equal status "submitted") (stringp name) (<= (length name) 512))
           (orgzly--create-book name))))))
  'accepted)

(defun orgzly--on-search-open (_args _params)
  "Show the toolbar's search field on the current screen."
  (setq orgzly-state-searching t)
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))
  'accepted)

(defun orgzly--on-search-close (_args _params)
  "Close the toolbar's search field."
  (setq orgzly-state-searching nil)
  (orgzly--later (lambda () (jetpacs-shell-push "orgzly")))
  'accepted)

(defun orgzly--on-search-help (_args _params)
  "Open the search documentation Revived's help button links to."
  (browse-url "https://www.orgzlyrevived.com/docs#search")
  'accepted)

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

;;; Revived's overflow commands

(defconst orgzly--clock-open-re
  "^CLOCK: *\\(\\[[0-9]\\{4,\\}-[0-9][0-9]-[0-9][0-9] ?[^]\r\n>]*?[0-9]\\{1,2\\}:[0-9][0-9]\\]\\)[ \t]*$"
  "A CLOCK line that was clocked in and not out.")

(defun orgzly--clock-now ()
  "Now as an inactive Org timestamp, as Revived writes clock lines."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun orgzly--logbook-bounds (content)
  "The (START . END) of CONTENT's LOGBOOK drawer body, or nil."
  (when (string-match "^[ \t]*:LOGBOOK:[ \t]*\n\\(\\(?:.*\n\\)*?\\)[ \t]*:END:" content)
    (cons (match-beginning 1) (match-end 1))))

(defun orgzly--clock (op content)
  "CONTENT after Revived's clock OP: `in', `out' or `cancel'."
  (let* ((content (or content ""))
         (book (orgzly--logbook-bounds content))
         (open (and book (let ((case-fold-search nil))
                           (with-temp-buffer
                             (insert (substring content (car book) (cdr book)))
                             (goto-char (point-min))
                             (when (re-search-forward orgzly--clock-open-re nil t)
                               (list (+ (car book) (1- (line-beginning-position)))
                                     (+ (car book) (1- (line-end-position)))
                                     (match-string 1))))))))
    (pcase op
      ('in
       (cond
        ((null book)
         (concat ":LOGBOOK:\nCLOCK: " (orgzly--clock-now) "\n:END:"
                 (unless (string-empty-p content) (concat "\n\n" content))))
        (open content)
        (t (concat (substring content 0 (car book)) "CLOCK: " (orgzly--clock-now) "\n"
                   (substring content (car book))))))
      ('out
       (if (not open) content
         (let* ((in (org-time-string-to-time (nth 2 open)))
                (minutes (/ (truncate (float-time (time-subtract nil in))) 60)))
           (concat (substring content 0 (nth 1 open))
                   "--" (orgzly--clock-now) (format " => %d:%02d" (/ minutes 60) (% minutes 60))
                   (substring content (nth 1 open))))))
      ('cancel
       (if (not open) content
         (let ((rest (concat (substring content 0 (nth 0 open))
                             (substring content (min (length content) (1+ (nth 1 open)))))))
           ;; An emptied LOGBOOK goes, as Org removes it.
           (if (string-match "\\`[ \t]*:LOGBOOK:[ \t]*\n[ \t]*:END:[ \t]*\n?\n?" rest)
               (substring rest (match-end 0))
             (replace-regexp-in-string "^[ \t]*:LOGBOOK:[ \t]*\n[ \t]*:END:[ \t]*\n" "" rest)))))
      (_ content))))

(defun orgzly--on-note-content-op (args params)
  "Run Revived's content operation ARGS' `:op' on note form `:scope'.
Clock in, out and cancel edit the LOGBOOK; sort orders the lines.  The
text is the content PARAMS capture, else the draft's, else the note's;
the result lands in the draft until Done saves it."
  (let* ((scope (plist-get args :scope))
         (orgzly-field-scope scope)
         (id (plist-get args :id))
         (entry (and id (ignore-errors (orgzly-state-resolve id t))))
         (op (pcase (plist-get args :op)
               ("clock-in" 'in) ("clock-out" 'out) ("clock-cancel" 'cancel) ("sort" 'sort))))
    (if (not (and (stringp scope) op)) 'rejected
      (let ((text (or (orgzly--field-maybe params "oz-content")
                      (orgzly-ui-note-text "content" entry) "")))
        ;; Only a field the screen shows can be reset; content in view mode
        ;; shows the draft as text instead.
        (setq orgzly--reset-fields (and (orgzly-ui-note-editing-p "content") '("oz-content")))
        (orgzly--draft-put scope "content"
                           (if (eq op 'sort)
                               (string-join (sort (split-string text "\n") #'string<) "\n")
                             (orgzly--clock op text))))
      'accepted)))

(defun orgzly--choices (id title choices on-pick)
  "Show TITLE's dialog ID offering CHOICES, (LABEL . VALUE) pairs.
ON-PICK gets the chosen VALUE."
  (with-jetpacs-owner "orgzly"
    (jetpacs-dialog-slot-show
     (jetpacs-wire-id "choose" id)
     (orgzly-design-wrap-dialog
      (orgzly-ui-choice-dialog title (mapcar (lambda (c) (cons (car c) (jetpacs-dialog-submit :value (cdr c))))
                                             choices))
      orgzly-state-dark)
     :on-conclude (lambda (status result _error)
                    (when (equal status "submitted")
                      (let ((value (plist-get result :value)))
                        (when (rassoc value choices) (funcall on-pick value))))))))

(defun orgzly--on-note-clock (args params)
  "Offer Revived's Clock submenu for the note of ARGS, captured in PARAMS."
  (orgzly--choices "clock" "Clock" '(("Clock in" . "clock-in") ("Clock out" . "clock-out")
                                     ("Clock cancel" . "clock-cancel"))
                   (lambda (op) (orgzly--on-note-content-op (plist-put (copy-sequence args) :op op)
                                                            params)))
  'accepted)

(defun orgzly--note-org (entry)
  "ENTRY as an Org heading, as Revived's Share > Note exports it."
  (concat "* " (mapconcat #'identity
                          (delq nil (list (alist-get 'state entry)
                                          (and (alist-get 'priority entry)
                                               (format "[#%s]" (alist-get 'priority entry)))
                                          (alist-get 'title entry)))
                          " ")
          (and (alist-get 'tags entry)
               (concat " :" (string-join (alist-get 'tags entry) ":") ":"))
          "\n"
          (let ((content (alist-get 'content entry)))
            (if (and content (not (string-empty-p content))) (concat content "\n") ""))))

(defun orgzly--on-note-share (args _params)
  "Offer Revived's Share submenu for the note ARGS name: note, title or content."
  (orgzly--guard
   (lambda ()
     (let* ((entry (orgzly-state-resolve (plist-get args :id) t))
            (parts (cl-remove-if
                    (lambda (c) (string-empty-p (cdr c)))
                    (list (cons "Note" (orgzly--note-org entry))
                          (cons "Title" (or (alist-get 'title entry) ""))
                          (cons "Content" (or (alist-get 'content entry) ""))))))
       (with-jetpacs-owner "orgzly"
         (jetpacs-dialog-slot-show
          (jetpacs-wire-id "share" (plist-get args :id))
          (orgzly-design-wrap-dialog
           (orgzly-ui-choice-dialog "Share" (mapcar (lambda (c) (cons (car c) (jetpacs-share (cdr c))))
                                                    parts))
           orgzly-state-dark)))
       'accepted))))

(defun orgzly--on-book-tags (args _params)
  "Ask for the #+FILETAGS of the notebook ARGS name, as Revived's Tags does."
  (orgzly--guard
   (lambda ()
     (let* ((book (orgzly-state-resolve (plist-get args :id) t))
            (name (alist-get 'name book))
            (preface (or (orgzly-data-book-preface name) ""))
            (current (when (string-match "^[ \t]*#\\+FILETAGS:[ \t]*\\(.*\\)$" preface)
                       (string-join (split-string (match-string 1 preface) ":" t "[ \t]+") " "))))
       (unless name (error "Expected a notebook"))
       (with-jetpacs-owner "orgzly"
         (jetpacs-dialog-slot-show
          (jetpacs-wire-id "filetags" name)
          (orgzly-design-wrap-dialog (orgzly-ui-name-dialog "Tags" "Tags" "Set" current)
                                     orgzly-state-dark)
          :on-conclude
          (lambda (status result _error)
            (let ((text (plist-get (plist-get result :fields) :oz-dialog-name)))
              (when (and (equal status "submitted") (stringp text))
                (orgzly--guard
                 (lambda ()
                   (let* ((tags (split-string text "[ \t:]+" t))
                          (line (and tags (format "#+FILETAGS: :%s:" (string-join tags ":"))))
                          (lines (split-string preface "\n"))
                          (at (cl-position-if (lambda (l) (string-match-p "\\`[ \t]*#\\+FILETAGS:" l))
                                              lines)))
                     (cond ((and at line) (setf (nth at lines) line))
                           (at (setq lines (append (cl-subseq lines 0 at) (nthcdr (1+ at) lines))))
                           (line (push line lines)))
                     (orgzly--write-preface name (string-join lines "\n"))
                     (orgzly--committed (list :kind 'book :book name))))))))))
       'accepted))))

(defun orgzly--import-candidates ()
  "Org files in `orgzly-import-directory' that are not notebooks yet."
  (when (and orgzly-import-directory (file-directory-p orgzly-import-directory))
    (cl-remove-if (lambda (f) (file-exists-p (orgzly-data-book-file (file-name-base f))))
                  (directory-files orgzly-import-directory t "\\.org\\'"))))

(defun orgzly--on-book-import (_args _params)
  "Import an Org file as a notebook, as Revived's Import Org file does.
Revived opens Android's document picker; this lists the Org files of
`orgzly-import-directory'."
  (let ((files (orgzly--import-candidates)))
    (orgzly--choices "import" "Import Org file"
                     (or (mapcar (lambda (f) (cons (file-name-nondirectory f) (file-name-nondirectory f)))
                                 files)
                         '(("No Org files to import" . "none")))
                     (lambda (base)
                       (let ((file (and (not (equal base "none"))
                                        (expand-file-name base orgzly-import-directory))))
                         (when (member file files)
                           (orgzly--guard
                            (lambda ()
                              (let ((target (orgzly-data-book-file (file-name-base file))))
                                (make-directory (file-name-directory target) t)
                                (copy-file file target)
                                (orgzly--committed '(:kind books))))))))))
  'accepted)

(defun orgzly-register ()
  "Register the optional Orgzly applet idempotently without reading notebooks."
  (orgzly-design-register)
  (with-jetpacs-owner "orgzly"
    (jetpacs-chrome-define-root "orgzly" "books"
                                (lambda (back) (orgzly-ui-screen '(:kind books) back)) :required t)
    (jetpacs-defaction "orgzly.home" #'orgzly--on-home :doc "Refresh and open Orgzly notebooks")
    (jetpacs-defaction "orgzly.open" #'orgzly--on-open :doc "Open an Orgzly destination, a settings page by its page name" :args '((:name view :type "enum" :required t) (:name page :type "string")))
    (jetpacs-defaction "orgzly.refresh" #'orgzly--on-refresh :doc "Refresh the bounded Orgzly notebook snapshot")
    (jetpacs-defaction "orgzly.book.open" #'orgzly--on-book-open :doc "Open the notebook represented by an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.book.edit" #'orgzly--on-book-edit :doc "Open notebook metadata for an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.book.save" #'orgzly--on-book-save :doc "Commit captured notebook metadata or create a notebook" :args '((:name id :type "ref")))
    (jetpacs-defaction "orgzly.book.delete" #'orgzly--on-book-delete :doc "Delete a confirmed current notebook" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.open" #'orgzly--on-note-open :doc "Open the note represented by an opaque revision id" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.new" #'orgzly--on-note-new :doc "Open a new-note form in the current notebook" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.child" #'orgzly--on-note-child :doc "Open a child-note form under the current note" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.fold" #'orgzly--on-fold :doc "Toggle the current outline note's descendants and content" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.section" #'orgzly--on-note-section :doc "Fold or unfold the note screen's metadata or content" :args '((:name section :type "enum" :required t)))
    (jetpacs-defaction "orgzly.note.metadata" #'orgzly--on-note-metadata :doc "Show all or selected metadata rows, or toggle always showing set ones" :args '((:name show :type "enum") (:name always-set :type "boolean")))
    (jetpacs-defaction "orgzly.note.edit" #'orgzly--on-note-edit :doc "Edit the open note's title or content in place" :args '((:name field :type "enum" :required t) (:name scope :type "string" :required t)))
    (jetpacs-defaction "orgzly.note.clear" #'orgzly--on-note-clear :doc "Clear a picked value of the open note" :args '((:name field :type "enum" :required t) (:name scope :type "string" :required t)))
    (jetpacs-defaction "orgzly.note.tag" #'orgzly--on-note-tag :doc "Put a known tag into the open note's Tags field" :args '((:name scope :type "string" :required t) (:name tag :type "string" :required t)))
    (jetpacs-defaction "orgzly.ts.part" #'orgzly--on-ts-part :doc "Set a part of the open timestamp dialog: its day, times, cookies or checkboxes" :args '((:name part :type "enum" :required t)))
    (jetpacs-defaction "orgzly.note.timestamp" #'orgzly--on-note-timestamp :doc "Insert a picked timestamp into the open note's edited title or content" :args '((:name scope :type "string" :required t)))
    (jetpacs-defaction "orgzly.note.pick" #'orgzly--on-note-pick :doc "Pick the open note's state, priority or planning time" :args '((:name field :type "enum" :required t) (:name scope :type "string" :required t) (:name raw :type "string")))
    (jetpacs-defaction "orgzly.link.open" #'orgzly--on-link-open :doc "Open a note's web or mail link, named by note, field and index" :args '((:name id :type "ref" :required t) (:name field :type "enum" :required t) (:name link :type "integer" :required t)))
    (jetpacs-defaction "orgzly.fold.all" #'orgzly--on-fold-all :doc "Unfold the open outline when anything is folded, else fold every foldable note")
    (jetpacs-defaction "orgzly.note.save" #'orgzly--on-note-save :doc "Commit captured note fields before accepting the edit" :args '((:name id :type "ref") (:name book-id :type "ref") (:name parent-id :type "ref")))
    (jetpacs-defaction "orgzly.note.back" #'orgzly--on-note-back :doc "Leave the open note, asking to save or discard an edit first" :args '((:name id :type "ref") (:name book-id :type "ref") (:name parent-id :type "ref")))
    (jetpacs-defaction "orgzly.note.content-op" #'orgzly--on-note-content-op :doc "Clock in, out or cancel, or sort the lines of the open note's content" :args '((:name scope :type "string" :required t) (:name op :type "enum" :required t) (:name id :type "ref")))
    (jetpacs-defaction "orgzly.note.clock" #'orgzly--on-note-clock :doc "Offer clock in, out or cancel for the open note" :args '((:name scope :type "string" :required t) (:name id :type "ref")))
    (jetpacs-defaction "orgzly.book.tags" #'orgzly--on-book-tags :doc "Set a notebook's file tags" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.book.import" #'orgzly--on-book-import :doc "Import an Org file as a notebook")
    (jetpacs-defaction "orgzly.note.share" #'orgzly--on-note-share :doc "Share the open note, its title or its content" :args '((:name id :type "ref" :required t)))
    (jetpacs-defaction "orgzly.note.operate" #'orgzly--on-operate :doc "Commit a closed structural operation to the current note" :args '((:name id :type "ref" :required t) (:name operation :type "enum" :required t)))
    (jetpacs-defaction "orgzly.search.run" #'orgzly--on-search :doc "Run the captured bounded dotted query from the form named by scope" :args '((:name scope :type "string")))
    (jetpacs-defaction "orgzly.search.saved" #'orgzly--on-saved :doc "Run a named saved query" :args '((:name name :type "string" :required t)))
    (jetpacs-defaction "orgzly.book.new" #'orgzly--on-book-new :doc "Ask for and create a new notebook")
    (jetpacs-defaction "orgzly.setting" #'orgzly--on-setting :doc "Toggle or edit one listed Orgzly setting" :args '((:name name :type "string" :required t)))
    (jetpacs-defaction "orgzly.search.open" #'orgzly--on-search-open :doc "Show the toolbar search field")
    (jetpacs-defaction "orgzly.search.close" #'orgzly--on-search-close :doc "Close the toolbar search field")
    (jetpacs-defaction "orgzly.search.help" #'orgzly--on-search-help :doc "Open the search query documentation")
    (jetpacs-defaction "orgzly.search.save" #'orgzly--on-search-save :doc "Persist a captured named query")
    (jetpacs-defaction "orgzly.search.delete" #'orgzly--on-search-delete :doc "Remove a confirmed saved query" :args '((:name name :type "string" :required t)))
    (jetpacs-defaction "orgzly.page" #'orgzly--on-page :doc "Select a bounded outline or search page" :args '((:name page :type "integer" :required t)))
    (jetpacs-defaction "orgzly.theme" #'orgzly--on-theme :doc "Toggle Orgzly light and dark appearance"))
  (jetpacs-defapp "orgzly" :label "Orgzly" :icon "book" :surfaces '("orgzly")
                  :chrome 'standalone :home-route "books" :order 46
                  :destinations '((:key "books" :label "Notebooks" :icon "book"
                                    :verb "orgzly.home"))
                  :requires-extensions '("jetpacs.components" "jetpacs.design"))
  (orgzly-reminders-register)
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

(provide 'orgzly)
;;; orgzly.el ends here
