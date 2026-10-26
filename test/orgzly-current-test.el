;;; orgzly-current-test.el --- Current applet contract tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'orgzly)
(orgzly-register)  ; `require' only loads
(require 'jetpacs-data)
(require 'orgzly-data-test)

(defmacro orgzly-test-with-snapshot (&rest body)
  "Run BODY against temporary notebooks and a fixed snapshot clock."
  (declare (indent 0))
  `(orgzly-data-test--with-books
    (let ((orgzly-state nil) (orgzly-state-folded nil) (orgzly-state-query "")
          (orgzly-state-page 0) (orgzly-state-dark nil)
          (orgzly-saved-searches (copy-tree orgzly-saved-searches)))
      (orgzly-refresh (encode-time 0 30 14 9 7 2026))
      (cl-letf (((symbol-function 'orgzly--later) (lambda (_fn) nil))) ,@body))))

(defun orgzly-test-fields (&optional title new)
  "Return captured note fields with optional TITLE."
  (let* ((route (if new (list :kind 'new-note :book-id (alist-get 'id (orgzly-state-book "inbox")))
                  (list :kind 'note :id (alist-get 'id (car (orgzly-state-entries "gtd"))))))
         (orgzly-field-scope (orgzly-field-scope route))
         (fields (list :oz-title (or title "Changed title") :oz-state "NEXT" :oz-priority "B"
                       :oz-tags "work review" :oz-scheduled "<2026-07-12 Sun 10:00-11:00 +1w -2d>"
                       :oz-deadline "" :oz-content "Updated content\n- [ ] Review")))
    (list :fields (cl-loop for (key value) on fields by #'cddr
                           append (list (intern (concat ":" (orgzly-field-id (substring (symbol-name key) 1)))) value)))))

(ert-deftest orgzly-current-rejects-traversal-and-aliases ()
  (orgzly-test-with-snapshot
    (dolist (name '("../outside" "a/b" "a\\b" "" "." ".."))
      (should-error (orgzly-data-book-file name)))
    (make-symbolic-link (expand-file-name "gtd.org" orgzly-directory)
                        (expand-file-name "alias.org" orgzly-directory))
    (should-error (orgzly-data-book-file "alias"))))

(ert-deftest orgzly-current-stale-bytes-refuse-edit ()
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd"))) (file (alist-get 'file entry)))
      (with-temp-buffer (insert "* External edit\n") (write-region (point-min) (point-max) file nil 'silent))
      (should (eq 'stale (orgzly--on-note-save (list :id (alist-get 'id entry)) (orgzly-test-fields)))))))

(ert-deftest orgzly-current-unsaved-buffer-is-preserved ()
  (orgzly-test-with-snapshot
    (with-current-buffer (orgzly-data--book-buffer "gtd")
      (goto-char (point-max)) (insert "UNSAVED")
      (should-error (orgzly-refresh) :type 'orgzly-stale)
      (should (string-suffix-p "UNSAVED" (buffer-string)))
      (set-buffer-modified-p nil))))

(ert-deftest orgzly-current-note-save-is-durable-and-roundtrips-cookies ()
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd"))) (id (alist-get 'id entry)))
      (should (eq 'accepted (orgzly--on-note-save (list :id id) (orgzly-test-fields))))
      (let ((new (car (orgzly-state-entries "gtd"))))
        (should (equal "Changed title" (alist-get 'title new)))
        (should (equal "NEXT" (alist-get 'state new)))
        (should (equal "B" (alist-get 'priority new)))
        (should (equal "Updated content\n- [ ] Review" (alist-get 'content new)))
        (should (equal "+1w" (alist-get 'repeater (alist-get 'scheduled new))))
        (should (equal "-2d" (alist-get 'warning (alist-get 'scheduled new))))
        (should (alist-get 'end-time (alist-get 'scheduled new))))
      (should (eq 'stale (orgzly--on-note-save (list :id id) (orgzly-test-fields)))))))

(ert-deftest orgzly-current-compound-edit-rolls-back-before-saving ()
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (before (orgzly-state-revision (alist-get 'file entry))))
      (cl-letf (((symbol-function 'orgzly-data-set-priority) (lambda (&rest _) (error "Injected edit failure"))))
        (should (eq 'rejected (orgzly--on-note-save (list :id (alist-get 'id entry)) (orgzly-test-fields)))))
      (should (equal before (orgzly-state-revision (alist-get 'file entry)))))))

(ert-deftest orgzly-current-new-note-has-one-save ()
  (orgzly-test-with-snapshot
    (let ((book (orgzly-state-book "inbox")) (saves 0) (original (symbol-function 'save-buffer)))
      (cl-letf (((symbol-function 'save-buffer) (lambda (&rest args) (cl-incf saves) (apply original args))))
        (should (eq 'accepted (orgzly--on-note-save (list :book-id (alist-get 'id book)) (orgzly-test-fields "Captured" t)))))
      (should (= 1 saves))
      (should (= 2 (length (orgzly-state-entries "inbox")))))))

(ert-deftest orgzly-current-failed-new-note-leaves-no-heading ()
  (orgzly-test-with-snapshot
    (let* ((book (orgzly-state-book "inbox")) (file (alist-get 'file book))
           (before (orgzly-state-revision file)))
      (cl-letf (((symbol-function 'orgzly-data-set-title) (lambda (&rest _) (error "Injected failure"))))
        (should (eq 'rejected (orgzly--on-note-save (list :book-id (alist-get 'id book)) (orgzly-test-fields nil t)))))
      (should (equal before (orgzly-state-revision file))))))

(ert-deftest orgzly-current-rejects-missing-and-invalid-fields ()
  (orgzly-test-with-snapshot
    (let ((args (list :id (alist-get 'id (car (orgzly-state-entries "gtd"))))))
      (should (eq 'rejected (orgzly--on-note-save args nil)))
      (should (eq 'rejected (orgzly--on-note-save args (orgzly-test-fields "Bad\nheading")))))
    (should (eq 'stale (orgzly--on-operate '(:id "unknown" :operation "eval") nil)))
    (should-error (orgzly--check-query "ad.9999999"))))

(ert-deftest orgzly-current-folding-does-not-change-files ()
  (orgzly-test-with-snapshot
    (let* ((entries (orgzly-state-entries "gtd")) (parent (nth 1 entries))
           (before (orgzly-state-revision (alist-get 'file parent))))
      (should (eq 'accepted (orgzly--on-fold (list :id (alist-get 'id parent)) nil)))
      (should (= (- (length entries) 2) (length (orgzly-state-visible entries))))
      (should (equal before (orgzly-state-revision (alist-get 'file parent)))))))

(ert-deftest orgzly-current-builders-are-pure-and-profile-scoped ()
  (orgzly-test-with-snapshot
    (let* ((id (alist-get 'id (car (orgzly-state-entries "gtd"))))
           (book-id (alist-get 'id (orgzly-state-book "gtd")))
           (before (copy-tree orgzly-state)))
      (dolist (route (list '(:kind books) '(:kind book :book "gtd") '(:kind searches)
                           '(:kind query :name "Agenda" :query ".it.done ad.7")
                           '(:kind query :query "i.todo")
                           '(:kind settings) '(:kind new-book) '(:kind save-search)
                           (list :kind 'note :id id) (list :kind 'book-edit :id book-id)
                           (list :kind 'new-note :book "gtd" :book-id book-id)))
        (let ((screen (orgzly-ui-screen route)))
          (should (equal "jetpacs.design_scope" (plist-get screen :t)))
          (should-not (string-match-p "jetpacs.material3" (jetpacs-node->canonical-json screen)))
          (cl-letf (((symbol-function 'orgzly-data-entries) (lambda (&rest _) (error "Read during render")))
                    ((symbol-function 'current-time) (lambda () (error "Clock during render")))
                    ((symbol-function 'find-file-noselect) (lambda (&rest _) (error "Disk during render"))))
            (should (equal (jetpacs-node->canonical-json screen)
                           (jetpacs-node->canonical-json (orgzly-ui-screen route)))))))
      (should (equal before orgzly-state)))))

(ert-deftest orgzly-current-action-payloads-contain-no-paths ()
  (orgzly-test-with-snapshot
    (let ((orgzly-state-outline "gtd"))
      (dolist (json (list (jetpacs-node->canonical-json (orgzly-ui-screen '(:kind book :book "gtd")))
                          (jetpacs-node->canonical-json (orgzly-ui-screen '(:kind books)))))
        (should-not (string-match-p (regexp-quote orgzly-directory) json))
        (should-not (string-match-p "\"pos\":" json))))))

;; Not on a device in the batch run: the dialog builds against the
;; reference phone's width and the default twelve-hour clock.
(ert-deftest orgzly-current-timestamp-dialog-round-trips ()
  "The timestamp dialog seeds from an Org timestamp, writes its checked
parts back, and builds Revived's day, time, repeater and delay rows."
  (let ((ts (orgzly--ts-seed "<2026-07-12 Sun 09:30-10:00 .+1w -2d>")))
    (should (equal (plist-get ts :date) "2026-07-12"))
    (should (equal (plist-get ts :time) "09:30"))
    (should (equal (plist-get ts :end) "10:00"))
    (should (plist-get ts :end-on))
    (should (equal (orgzly--ts-string ts) "<2026-07-12 Sun 09:30-10:00 .+1w -2d>"))
    (should (equal (orgzly--ts-string (plist-put (copy-sequence ts) :time-on nil))
                   "<2026-07-12 Sun .+1w -2d>")))
  ;; Unset parts keep Revived's unchecked defaults.
  (let ((ts (orgzly--ts-seed "")))
    (should (equal (plist-get ts :repeater) ".+1w"))
    (should (equal (plist-get ts :delay) "-1d"))
    (should-not (plist-get ts :time-on))
    (should (string-match-p "\\`<[0-9-]\\{10\\} [A-Za-z]+>\\'" (orgzly--ts-string ts))))
  (let* ((ts (append '(:scope "s" :field "scheduled") (orgzly--ts-seed "<2026-07-12 Sun>")))
         (json (jetpacs-node->canonical-json
                (orgzly-design-wrap-dialog (orgzly-ui-timestamp-dialog ts) nil))))
    (dolist (text '("\"DAY\"" "\"Sun, Jul 12\"" "\"Tomorrow\"" "\"REPEATER\"" "\".+1w\""
                    "\"date_button\"" "\"time_button\"" "\"checkbox\"" "\"Set\""))
      (should (string-search text json)))
    ;; The end time waits for the time.
    (should (string-match-p "\"enabled\":false" json)))
  ;; A phone window keeps the measured width; a tablet pane widens it.
  (should (= (orgzly-design-dialog-width nil) 316.44))
  (should (= (orgzly-design-dialog-width '(:width_dp 635 :height_dp 800)) 460.0)))

(ert-deftest orgzly-current-inserted-timestamp-resets-the-field ()
  "Set in an inserted-timestamp dialog appends the stamp to the captured
text and asks the next push to replace the field's typed draft."
  (orgzly-test-with-snapshot
    (let ((orgzly-state-draft nil) (orgzly--reset-fields nil) pushed)
      (cl-letf (((symbol-function 'orgzly--later) (lambda (fn) (funcall fn)))
                ((symbol-function 'jetpacs-shell-push)
                 (lambda (_owner &rest args) (setq pushed args))))
        (setq orgzly-state-ts (append '(:scope "form" :field "title" :text "Call mom")
                                      (orgzly--ts-seed "<2026-07-12 Sun>")))
        (orgzly--ts-conclude "set"))
      (should (equal (plist-get orgzly-state-draft :title) "Call mom <2026-07-12 Sun>"))
      (should (equal (plist-get pushed :reset-input-ids)
                     (list (jetpacs-wire-id "form" "oz-title"))))
      (should-not orgzly-state-ts))))

(ert-deftest orgzly-current-tags-pick-from-the-known-tags ()
  "The Tags \"...\" lists every known tag in a dropdown before its button;
a picked tag completes the word being typed or is appended, and replaces
the field's typed draft."
  (should (equal (orgzly--tags-insert "" "work") "work "))
  (should (equal (orgzly--tags-insert "home" "work") "home work "))
  (should (equal (orgzly--tags-insert "home wo" "work") "home work "))
  (should (equal (orgzly--tags-insert "home WO" "work") "home work "))
  (should (equal (orgzly--tags-insert "home " "work") "home work "))
  (orgzly-test-with-snapshot
    (let* ((tags (orgzly-ui-all-tags))
           (entry (car (orgzly-state-entries "gtd")))
           (screen (orgzly-ui-screen (list :kind 'note :id (alist-get 'id entry))))
           (json (jetpacs-node->canonical-json screen))
           (orgzly-state-draft nil) (orgzly--reset-fields nil) pushed)
      (should tags)
      (should (equal tags (sort (copy-sequence tags) #'string<)))
      (should (equal tags (delete-dups (copy-sequence tags))))
      (should (string-search "\"popup_before_anchor\"" json))
      (dolist (tag tags) (should (string-search (format "\"label\":\"%s\"" tag) json)))
      (cl-letf (((symbol-function 'orgzly--later) (lambda (fn) (funcall fn)))
                ((symbol-function 'jetpacs-shell-push)
                 (lambda (_owner &rest args) (setq pushed args))))
        (should (eq (orgzly--on-note-tag (list :scope "form" :tag "no-such-tag") nil) 'rejected))
        (should (eq (orgzly--on-note-tag
                     (list :scope "form" :tag (car tags))
                     (list :fields (list (intern (concat ":" (jetpacs-wire-id "form" "oz-tags")))
                                         "x")))
                    'accepted)))
      (should (equal (plist-get orgzly-state-draft :tags) (concat "x " (car tags) " ")))
      (should (equal (plist-get pushed :reset-input-ids) (list (jetpacs-wire-id "form" "oz-tags")))))))

(ert-deftest orgzly-current-outline-rows-follow-revived ()
  "The outline is Revived's item_head rows: guides by depth, a bullet, the
title as rich spans, a fold button on foldable notes, and a back gesture
to the notebooks under the drawer's menu button."
  (orgzly-test-with-snapshot
    (let* ((orgzly-state-outline "gtd")
           (entries (orgzly-state-entries "gtd"))
           (rows (orgzly-ui-outline-rows "gtd"))
           (call (car entries)) (projects (nth 1 entries)) (ship (nth 2 entries))
           (back (jetpacs-view-switch "books"))
           (screen (orgzly-ui-screen '(:kind book :book "gtd") back))
           (json (jetpacs-node->canonical-json screen)))
      (should-not (string-match-p "\"template\"" json))
      (should (= (length rows) (length (orgzly-state-visible entries))))
      ;; State bold in the TODO color, priority bold, tags 14sp after the title.
      (let ((spans (append (gethash "spans" (aref rows 0)) nil)))
        (should (equal (plist-get (car spans) :text) "TODO"))
        (should (equal (plist-get (car spans) :font_weight) "bold"))
        (should (equal (plist-get (car spans) :color) (orgzly-design-color 'todo nil)))
        (should (equal (plist-get (nth 1 spans) :text) "  #A"))
        (should (equal (plist-get (car (last spans)) :text) "  family phone"))
        (should (equal (plist-get (car (last spans)) :scale) 0.875)))
      ;; A parent folds with the en dash, a leaf with content too, a done
      ;; note fades, and depth draws guides.
      (should (equal (gethash "fold" (aref rows 1)) "–"))
      (should (equal (gethash "fold" (aref rows 0)) "–"))
      (should (gethash "faded" (aref rows 2)))
      (should (= 1 (gethash "depth" (aref rows 2))))
      (should (equal (gethash "id" (aref rows 1)) (alist-get 'id projects)))
      (should (equal (gethash "id" (aref rows 2)) (alist-get 'id ship)))
      (should (string-match-p (format "\"id\":\"%s\"" (alist-get 'id call)) json))
      ;; The hamburger stays: no arrow, the drawer present, back declared.
      (let* ((scaffold (aref (plist-get screen :children) 0))
             (leading (aref (plist-get (plist-get scaffold :top_bar) :children) 0)))
        (should (equal (plist-get scaffold :t) "scaffold"))
        (should (plist-get scaffold :drawer))
        (should (equal (plist-get scaffold :on_back) back))
        (should-not (equal (plist-get leading :icon) "arrow_back"))))))

(ert-deftest orgzly-current-links-open-from-the-snapshot ()
  "A link span names note, field and index; Emacs opens the snapshot's
target, only for web and mail addresses."
  (should (equal (orgzly-ui-link-segments "See [[https://a.example/x][docs]] or https://b.example.")
                 '(("See " . nil) ("docs" . "https://a.example/x") (" or " . nil)
                   ("https://b.example" . "https://b.example") ("." . nil))))
  (should (equal (orgzly-ui-link-segments "[[file:notes.org]]")
                 '(("file:notes.org" . "file:notes.org"))))
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (id (alist-get 'id entry))
           opened)
      (setf (alist-get 'content entry) "Read [[https://a.example/x][docs]] and [[file:x.org][x]]")
      (let* ((spans (orgzly-ui-org-spans (alist-get 'content entry) id "content"))
             (link (cl-find-if (lambda (s) (plist-get s :on_tap)) spans)))
        (should (equal (plist-get link :text) "docs"))
        (should (plist-get link :underline))
        (should (equal (plist-get (plist-get link :on_tap) :args)
                       (list :id id :field "content" :link 0)))
        (should-not (string-match-p "a.example" (jetpacs-node->canonical-json
                                                 (plist-get link :on_tap)))))
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (should (eq 'accepted (orgzly--on-link-open (list :id id :field "content" :link 0) nil)))
        (should (equal opened "https://a.example/x"))
        (setq opened nil)
        (should (eq 'rejected (orgzly--on-link-open (list :id id :field "content" :link 1) nil)))
        (should (eq 'rejected (orgzly--on-link-open (list :id id :field "content" :link 7) nil)))
        (should-not opened)))))

(ert-deftest orgzly-current-book-create-and-preface ()
  (orgzly-test-with-snapshot
    (should (eq 'accepted (let ((orgzly-field-scope (orgzly-field-scope '(:kind new-book))))
                            (orgzly--on-book-save nil (list :fields
                              (list (intern (concat ":" (orgzly-field-id "oz-name"))) "new"
                                    (intern (concat ":" (orgzly-field-id "oz-preface"))) "#+TITLE: A notebook\nWelcome\n"))))))
    (should (equal "A notebook" (alist-get 'title (orgzly-state-book "new"))))))

(ert-deftest orgzly-current-all-registered-actions-are-documented ()
  (maphash (lambda (name _handler)
             (when (string-prefix-p "orgzly." name)
               (should (stringp (plist-get (jetpacs-action-schema name) :doc)))))
           jetpacs-action-handlers))

(ert-deftest orgzly-current-cached-forms-have-disjoint-field-identities ()
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (id (alist-get 'id entry))
           (book-id (alist-get 'id (orgzly-state-book "gtd")))
           (note (orgzly-ui-screen (list :kind 'note :id id)))
           (child (orgzly-ui-screen (list :kind 'new-note :book "gtd" :book-id book-id :parent-id id)))
           (document (jetpacs-column note child)))
      (jetpacs-shell--gate-ids document nil))))

(ert-deftest orgzly-current-saved-effect-survives-refresh-failure ()
  (orgzly-test-with-snapshot
    (let ((entry (car (orgzly-state-entries "gtd"))))
      (cl-letf (((symbol-function 'orgzly-refresh) (lambda (&rest _) (error "Other book busy"))))
        (should (eq 'accepted (orgzly--on-note-save (list :id (alist-get 'id entry)) (orgzly-test-fields)))))
      (should (equal "Changed title" (alist-get 'title (car (orgzly-data-entries "gtd"))))))))

(ert-deftest orgzly-lifecycle-is-symmetric ()
  "Loading orgzly.el registers nothing, and orgzly-unregister undoes
everything orgzly-register did: the trusted launcher's two lifecycle
checks, run from the foundation's applet tooling."
  (require 'jetpacs-devtools)
  (load (expand-file-name "../jetpacs-applet-mcp/jetpacs-applet-tooling.el"
                          (file-name-directory
                           (locate-library "jetpacs-surfaces")))
        nil 'nomessage)
  (should-not (jetpacs-applet-tooling-load-recording-registrations
               (locate-library "orgzly.el" t)))
  (let ((report (jetpacs-applet-tooling-check-lifecycle-symmetry "orgzly")))
    (should (string-match-p "Status: SYMMETRIC" report))
    (should (string-match-p "Registered on entry: yes" report)))
  (should (assoc "orgzly" jetpacs-apps--registry)))

(ert-deftest orgzly-reminders-follow-the-app-lifecycle ()
  "Registering Orgzly starts its reminder projection; unregistering, the
owner's teardown, removes it."
  (unwind-protect
      (progn
        (orgzly-register)
        (should (jetpacs-reminders-projection-p "orgzly"))
        (should (jetpacs--owned-names "reminder-projection" "orgzly"))
        (orgzly-unregister)
        (should-not (jetpacs-reminders-projection-p "orgzly"))
        (should-not (jetpacs--owned-names "reminder-projection" "orgzly")))
    (orgzly-register)))

(ert-deftest orgzly-current-clock-follows-revived ()
  (cl-letf (((symbol-function 'orgzly--clock-now) (lambda () "[2026-10-25 Sun 15:00]")))
    (should (equal (orgzly--clock 'in "") ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 15:00]\n:END:"))
    (should (equal (orgzly--clock 'in "Body")
                   ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 15:00]\n:END:\n\nBody"))
    ;; Already clocked in: unchanged.
    (let ((open ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 13:30]\n:END:\n\nBody"))
      (should (equal (orgzly--clock 'in open) open))
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _) (* 90 60.0))))
        (should (equal (orgzly--clock 'out open)
                       ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 13:30]--[2026-10-25 Sun 15:00] => 1:30\n:END:\n\nBody")))
      (should (equal (orgzly--clock 'cancel open) "Body")))
    ;; Clocked out: in adds a new line at the drawer's start.
    (let ((closed ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 13:30]--[2026-10-25 Sun 14:00] =>  0:30\n:END:"))
      (should (equal (orgzly--clock 'in closed)
                     (concat ":LOGBOOK:\nCLOCK: [2026-10-25 Sun 15:00]\n"
                             "CLOCK: [2026-10-25 Sun 13:30]--[2026-10-25 Sun 14:00] =>  0:30\n:END:")))
      (should (equal (orgzly--clock 'out closed) closed))
      (should (equal (orgzly--clock 'cancel closed) closed)))))

(ert-deftest orgzly-current-back-asks-only-after-an-edit ()
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (args (list :id (alist-get 'id entry)))
           (orgzly-field-scope (orgzly-field-scope (list :kind 'note :id (alist-get 'id entry))))
           (same (list :fields (list (intern (concat ":" (orgzly-field-id "oz-tags")))
                                     (string-join (alist-get 'tags entry) " "))))
           (shown nil) (popped nil))
      (cl-letf (((symbol-function 'jetpacs-dialog-slot-show) (lambda (&rest _) (setq shown t)))
                ((symbol-function 'orgzly--leave-note) (lambda () (setq popped t))))
        (should-not (orgzly--note-modified-p args same))
        (should (eq (orgzly--on-note-back args same) 'accepted))
        (should (and popped (not shown)))
        (setq popped nil)
        (should (orgzly--note-modified-p args (orgzly-test-fields)))
        (orgzly--on-note-back args (orgzly-test-fields))
        (should (and shown (not popped)))
        ;; A picked value in the draft counts as an edit too.
        (setq orgzly-state-draft (list :scope orgzly-field-scope :priority "C"))
        (should (orgzly--note-modified-p args same))))))

(ert-deftest orgzly-current-overflow-menus-match-revived ()
  (orgzly-test-with-snapshot
    (let* ((orgzly-menu-extras nil)
           (labels (lambda (route) (mapcar (lambda (i) (plist-get i :label))
                                           (orgzly-ui--overflow-items route))))
           (entry (car (orgzly-state-entries "gtd"))))
      (should (equal (funcall labels '(:kind books)) '("Import Org file" "Sync" "Settings")))
      (should (equal (funcall labels '(:kind book :book "gtd"))
                     '("Edit preface" "Tags" "Sync" "Settings")))
      (should (equal (funcall labels (list :kind 'note :id (alist-get 'id entry)))
                     '("Clock" "Metadata" "Delete" "Share" "Sync" "Settings" "Sort note")))
      (let ((orgzly-menu-extras t))
        (should (member "Jetpacs apps" (funcall labels '(:kind books))))))))

(ert-deftest orgzly-current-book-tags-rewrite-filetags ()
  (orgzly-test-with-snapshot
    (let* ((book (orgzly-state-book "gtd")) on-conclude)
      (cl-letf (((symbol-function 'jetpacs-dialog-slot-show)
                 (lambda (_id _node &rest kw) (setq on-conclude (plist-get kw :on-conclude)))))
        (should (eq (orgzly--on-book-tags (list :id (alist-get 'id book)) nil) 'accepted))
        (funcall on-conclude "submitted" '(:fields (:oz-dialog-name "alpha beta")) nil)
        (should (string-match-p "^#\\+FILETAGS: :alpha:beta:$" (orgzly-data-book-preface "gtd")))))))

(defun orgzly-test-find-nodes (type node)
  "Every node of TYPE in the tree NODE, depth first."
  (let (found)
    (cl-labels ((walk (n)
                  (cond ((and (consp n) (keywordp (car n)))
                         (when (equal (plist-get n :t) type) (push n found))
                         (cl-loop for (_k v) on n by #'cddr do (walk v)))
                        ((vectorp n) (mapc #'walk n))
                        ((consp n) (mapc #'walk n)))))
      (walk node))
    (nreverse found)))

(ert-deftest orgzly-current-period-hint-follows-the-type-wheel ()
  (let* ((orgzly-state-dark nil)
         (repeat (orgzly-ui-period-dialog (orgzly-ui-period-kind "repeater" "scheduled")
                                          ".+" "1" "w" nil))
         (switch (car (orgzly-test-find-nodes "jetpacs.state_switch" repeat))))
    (should switch)
    (should (equal (plist-get switch :source) "oz-period-type"))
    (should (equal (plist-get switch :values) ["+" "++" ".+"]))
    (should (= (length (plist-get switch :children)) 3)))
  ;; A warning period has one type, no wheel to follow, and no hint.
  (let ((orgzly-state-dark nil))
    (should-not (orgzly-test-find-nodes
                 "jetpacs.state_switch"
                 (orgzly-ui-period-dialog (orgzly-ui-period-kind "delay" "deadline") "-" "1" "d" nil)))))

(ert-deftest orgzly-current-tags-field-suggests-known-tags ()
  (orgzly-test-with-snapshot
    (let* ((input (car (orgzly-test-find-nodes
                        "text_input" (orgzly-ui--input 'value "oz-tags" "" "Tags" nil nil
                                                       (orgzly-ui-all-tags))))))
      (should (equal (append (plist-get input :suggestions) nil) (orgzly-ui-all-tags)))
      (should (orgzly-ui-all-tags)))
    ;; Without known tags the member is absent, not empty.
    (should-not (plist-get (car (orgzly-test-find-nodes
                                 "text_input" (orgzly-ui--input 'value "oz-tags" "" "Tags")))
                           :suggestions))))

(ert-deftest orgzly-current-note-overflow-has-revived-submenus ()
  (orgzly-test-with-snapshot
    (let* ((id (alist-get 'id (car (orgzly-state-entries "gtd"))))
           (route (list :kind 'note :id id))
           (items (cl-letf (((symbol-function 'jetpacs-feature-advertised-p)
                             (lambda (feature &rest _) (equal feature "menu.submenu"))))
                    (orgzly-ui--overflow-items route)))
           (row (lambda (label) (cl-find label items :key (lambda (i) (plist-get i :label))
                                       :test #'equal))))
      (should (equal (mapcar (lambda (i) (plist-get i :label)) (append (plist-get (funcall row "Clock") :items) nil))
                     '("Clock in" "Clock out" "Clock cancel")))
      (should (equal (mapcar (lambda (i) (plist-get i :label)) (append (plist-get (funcall row "Metadata") :items) nil))
                     '("Show all" "Show selected" "Always show set")))
      (should (equal (mapcar (lambda (i) (plist-get i :label)) (append (plist-get (funcall row "Share") :items) nil))
                     '("Share note" "Share title" "Share content")))
      (should-not (plist-member (funcall row "Clock") :on_tap)))
    ;; Without the feature the rows dispatch, as before.
    (let ((items (cl-letf (((symbol-function 'jetpacs-feature-advertised-p) #'ignore))
                   (orgzly-ui--overflow-items
                    (list :kind 'note :id (alist-get 'id (car (orgzly-state-entries "gtd"))))))))
      (should (cl-every (lambda (i) (plist-get i :on_tap)) items)))))

(ert-deftest orgzly-current-metadata-visibility-follows-revived ()
  (let ((orgzly-selected-metadata '(tags state scheduled)))
    (let ((orgzly-state-metadata-visibility 'all) (orgzly-state-metadata-always-set nil))
      (should (orgzly-ui--metadata-shown-p 'deadline nil)))
    (let ((orgzly-state-metadata-visibility 'selected) (orgzly-state-metadata-always-set nil))
      (should (orgzly-ui--metadata-shown-p 'tags nil))
      (should-not (orgzly-ui--metadata-shown-p 'deadline t)))
    (let ((orgzly-state-metadata-visibility 'selected) (orgzly-state-metadata-always-set t))
      (should (orgzly-ui--metadata-shown-p 'deadline t))
      (should-not (orgzly-ui--metadata-shown-p 'priority nil))))
  (let ((orgzly-state-metadata-visibility 'all) (orgzly-state-metadata-always-set t))
    (cl-letf (((symbol-function 'orgzly--later) #'ignore))
      (should (eq (orgzly--on-note-metadata '(:show "selected") nil) 'accepted))
      (should (eq orgzly-state-metadata-visibility 'selected))
      (orgzly--on-note-metadata '(:always-set t) nil)
      (should-not orgzly-state-metadata-always-set)
      (should (eq (orgzly--on-note-metadata '(:show "bogus") nil) 'rejected)))))

(ert-deftest orgzly-current-clock-in-on-viewed-content-resets-nothing ()
  "Content shown as text has no field to reset; the draft carries the clock."
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (route (list :kind 'note :id (alist-get 'id entry)))
           (scope (orgzly-field-scope route))
           (orgzly-state-editing nil) (orgzly-state-draft nil) (orgzly--reset-fields nil))
      (should (eq (orgzly--on-note-content-op
                   (list :scope scope :op "clock-in" :id (alist-get 'id entry)) nil)
                  'accepted))
      (should-not orgzly--reset-fields)
      (should (string-match-p "CLOCK: \\[" (plist-get orgzly-state-draft :content))))))

(ert-deftest orgzly-current-hidden-fields-are-not-captured ()
  "Show selected or a folded section takes fields off the screen; Done
must not capture them, and saving keeps what they held."
  (orgzly-test-with-snapshot
    (let* ((entry (cl-find-if (lambda (e) (null (alist-get 'tags e))) (orgzly-state-entries "gtd")))
           (route (list :kind 'note :id (alist-get 'id entry))))
      (let ((orgzly-state-metadata-visibility 'selected)
            (orgzly-selected-metadata '(state scheduled))
            (orgzly-state-metadata-always-set t))
        (should-not (member "oz-tags" (orgzly-ui-note-captures route)))
        (should-not (member "oz-prop-name" (orgzly-ui-note-captures route))))
      (let ((orgzly-state-note-folds '(metadata)))
        (should-not (member "oz-tags" (orgzly-ui-note-captures route))))
      (should (member "oz-tags" (orgzly-ui-note-captures route))))))

(ert-deftest orgzly-current-metadata-rows-draw-radios-and-a-check-box ()
  (let* ((orgzly-state-metadata-visibility 'selected) (orgzly-state-metadata-always-set nil)
         (items (orgzly-ui--metadata-items)))
    (should (equal (mapcar (lambda (i) (plist-get i :checked_icon)) items)
                   '("radio_button_checked" "radio_button_checked" "check_box")))
    (should (equal (mapcar (lambda (i) (plist-get i :unchecked_icon)) items)
                   '("radio_button_unchecked" "radio_button_unchecked" "check_box_outline_blank")))
    (should (equal (mapcar (lambda (i) (plist-get i :checked)) items) '(:json-false t :json-false)))))

(ert-deftest orgzly-current-tags-clear-follows-the-field ()
  "The Companion shows Clear while the tags field has text: no round trip."
  (orgzly-test-with-snapshot
    (let* ((route (list :kind 'note :id (alist-get 'id (car (orgzly-state-entries "gtd")))))
           (orgzly-field-scope (orgzly-field-scope route))
           (switch (orgzly-ui--tags-buttons (orgzly-field-scope route))))
      (should (equal (plist-get switch :t) "jetpacs.state_switch"))
      (should (equal (plist-get switch :source) (orgzly-field-id "oz-tags")))
      (should (equal (plist-get switch :values) [""]))
      (should (orgzly-test-find-nodes "icon_button" (plist-get switch :otherwise))))))

(provide 'orgzly-current-test)
;;; orgzly-current-test.el ends here

;;; Device colors (SPEC 20.1.3, amendment #205)

(defconst orgzly-test-scheme
  (cl-loop for (key value) on
           '(:primary "#b4c5ff" :on_primary "#1a2d60" :secondary "#c1c5dd"
    :on_secondary "#2b3042" :surface "#121318" :on_surface "#e3e2e9"
    :outline "#8e9099" :error "#ffb4ab" :on_error "#690005"
    :background "#121318" :on_background "#e3e2e9"
    :success "#7fb77e" :warning "#e0b44c"
    :on_surface_variant "#c9cad4" :surface_container_low "#1a1b20"
    :surface_container "#1e1f25" :surface_container_high "#282a2f"
    :surface_container_lowest "#0c0e12" :primary_container "#7a90c8"
    :on_primary_container "#000000" :secondary_container "#8a90a5"
    :on_secondary_container "#000000")
           by #'cddr
           ;; Keys uninterned, as ebp.el decodes a never-seen key (SPEC 23.5).
           nconc (list (make-symbol (symbol-name key)) value))
  "The phone's dark scheme as a Material Companion reports it.")

(ert-deftest orgzly-design-derives-the-palette-from-the-device ()
  "Each color is the Material role Revived draws it with, a composite at
Android's alpha, or the fixed resource; no report keeps the measured palette."
  (should (equal (orgzly-design-blend "#FFFFFF" 0.5 "#000000") "#808080"))
  (let ((orgzly-design--palettes nil) (orgzly-design--profiles nil))
    (cl-letf (((symbol-function 'jetpacs-colors)
               (lambda () (list :polarity "dark" :light orgzly-test-scheme
                                :dark orgzly-test-scheme))))
      (should (equal (orgzly-design-color 'container t) "#1A1B20"))
      (should (equal (orgzly-design-color 'menu t) "#1E1F25"))
      (should (equal (orgzly-design-color 'dialog t) "#282A2F"))
      (should (equal (orgzly-design-color 'fab t) "#7A90C8"))
      (should (equal (orgzly-design-color 'link t) "#B4C5FF"))
      (should (equal (orgzly-design-color 'guide t)
                     (orgzly-design-blend "#FFFFFF" 0.12 "#121318")))
      (should (equal (orgzly-design-color 'dim t)
                     (orgzly-design-blend "#C9CAD4" 0.45 "#121318")))
      (should (equal (orgzly-design-color 'todo t) "#EF9A9A"))
      (should (equal (cdr (assoc "on_surface" (orgzly-design-roles t))) "#C9CAD4"))
      (should (equal (cdr (assoc "outline" (orgzly-design-roles t)))
                     (orgzly-design-color 'guide t)))
      (should (equal (cdr (assoc "error" (orgzly-design-roles t))) "#EF9A9A"))
      (should (equal (plist-get (cdr (assoc "primary"
                                            (plist-get (orgzly-design-current-profile t)
                                                       :theme-roles)))
                                :value)
                     "#B4C5FF")))
    ;; A Companion without Material's vocabulary: the standard roles
    ;; land, the rest keep the measured colors.
    (cl-letf (((symbol-function 'jetpacs-colors)
               (lambda () (list :polarity "light"
                                :light '(:primary "#396286" :on_primary "#ffffff"
                                         :secondary "#585e71" :on_secondary "#ffffff"
                                         :surface "#f8f9fe" :on_surface "#2d333a"
                                         :outline "#74777f")
                                :dark orgzly-test-scheme))))
      (should (equal (orgzly-design-color 'link nil) "#396286"))
      (should (equal (orgzly-design-color 'container nil) "#F1F4FA"))
      (should (equal (orgzly-design-color 'text nil) "#5A6067")))
    ;; No report: the measured palettes, byte for byte.
    (cl-letf (((symbol-function 'jetpacs-colors) #'ignore))
      (should (equal (orgzly-design-color 'container t) "#1A1B20"))
      (should (equal (orgzly-design-roles nil) orgzly-design-light)))))

(ert-deftest orgzly-follows-the-device-appearance-until-chosen ()
  "The preset follows the reported polarity; a toggle makes it the user's."
  (let ((orgzly-state-dark t) (orgzly-state-theme-chosen nil))
    (cl-letf (((symbol-function 'jetpacs-colors) #'ignore))
      (orgzly--follow-device)
      (should orgzly-state-dark))
    (cl-letf (((symbol-function 'jetpacs-colors)
               (lambda () '(:polarity "light" :light nil :dark nil)))
              ((symbol-function 'orgzly--later) #'ignore))
      (orgzly--follow-device)
      (should-not orgzly-state-dark)
      (orgzly--on-theme nil nil)
      (should orgzly-state-dark)
      (should orgzly-state-theme-chosen)
      (orgzly--on-colors-changed nil nil)
      (should orgzly-state-dark))))

(ert-deftest orgzly-design-presets-stay-measured-under-device-colors ()
  "Re-registering with a device report builds the same immutable presets."
  (let ((orgzly-design--palettes nil) (orgzly-design--profiles nil))
    (cl-letf (((symbol-function 'jetpacs-colors)
               (lambda () (list :polarity "dark" :light orgzly-test-scheme
                                :dark orgzly-test-scheme))))
      (should (equal (plist-get (cdr (assoc "surface"
                                            (plist-get (orgzly-design-current-profile t) :tokens)))
                                :value)
                     "#121318"))
      (orgzly-design-register))))

;;; The synchronized content editor (SPEC 19)

(defun orgzly-test--with-content-editor (scope text cursor body)
  "Run BODY with a fake client whose mirror holds SCOPE's content TEXT at CURSOR.
BODY gets a thunk returning the `edit.apply' frames sent, newest first,
then the client, document and editor id."
  (let* ((client (ebp-client-create :receipt-file (make-temp-file "orgzly-editor-receipts")))
         (document (orgzly-ui-content-document scope))
         (id (let ((orgzly-field-scope scope)) (orgzly-field-id "oz-content")))
         (sent nil))
    (setf (ebp-client-state client) 'ready
          (ebp-client-granted client) ["editor.sync"])
    (puthash (cons document id)
             (list :session (make-string 32 ?0) :seq 0 :text text :cursor cursor)
             (ebp-client-editors client))
    (cl-letf (((symbol-function 'jetpacs-client) (lambda () client))
              ((symbol-function 'ebp-client--request)
               (lambda (_c method params _cb &optional _t)
                 (push (cons method params) sent))))
      (funcall body (lambda () sent) client document id))))

(ert-deftest orgzly-current-content-is-a-synchronized-editor ()
  "With `editor.sync' granted the edited content is an editor node on the
form's own document, wearing the content field's styles and Revived's
hint; Done captures no content field, the mirror is read instead."
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (route (list :kind 'note :id (alist-get 'id entry)))
           (scope (orgzly-field-scope route))
           (orgzly-state-editing (list :scope scope :fields '("content"))))
      (cl-letf (((symbol-function 'jetpacs-granted-p) (lambda (&rest _) t)))
        (let* ((screen (orgzly-ui-screen route))
               (json (jetpacs-node->canonical-json screen)))
          (should (string-search (format "\"document\":\"orgzly:content:%s\"" scope) json))
          (should (string-search "\"hint\":\"Content\"" json))
          (should (string-search "\"slot\":\"editor.placeholder\"" json))
          (should-not (member "oz-content" (orgzly-ui-note-captures route)))))
      (cl-letf (((symbol-function 'jetpacs-granted-p) (lambda (&rest _) nil)))
        (should-not (string-search "\"t\":\"editor\""
                                   (jetpacs-node->canonical-json (orgzly-ui-screen route))))
        (should (member "oz-content" (orgzly-ui-note-captures route)))))))

(ert-deftest orgzly-current-timestamp-lands-at-the-content-caret ()
  "Set in the content's timestamp dialog splices the stamp at the device
caret with the caret after it, as Revived inserts it; the mirror's text
then feeds the draft, and no field is reset."
  (orgzly-test-with-snapshot
    (let ((orgzly-state-draft nil) (orgzly--reset-fields nil))
      (orgzly-test--with-content-editor
       "form" "Call mom later" 8
       (lambda (sent _client document id)
         (setq orgzly-state-ts (append '(:scope "form" :field "content")
                                       (orgzly--ts-seed "<2026-07-12 Sun>")))
         (orgzly--ts-conclude "set")
         (let ((params (cdar (funcall sent))))
           (should (eq (caar (funcall sent)) 'edit.apply))
           (should (equal (plist-get params :document) document))
           (should (equal (plist-get params :editor_id) id))
           (should (= (plist-get params :start) 8))
           (should (= (plist-get params :del) 0))
           (should (equal (plist-get params :text) "<2026-07-12 Sun>"))
           (should (= (plist-get params :cursor) (+ 8 (length "<2026-07-12 Sun>")))))
         (should-not orgzly--reset-fields)
         (should-not orgzly-state-ts))))))

(ert-deftest orgzly-current-content-operation-splices-the-editor ()
  "Clock in on a synchronized content editor replaces its whole text in
one splice instead of re-seeding the field."
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd")))
           (route (list :kind 'note :id (alist-get 'id entry)))
           (scope (orgzly-field-scope route))
           (orgzly-state-draft nil) (orgzly--reset-fields nil) (orgzly-state-editing nil))
      (orgzly-test--with-content-editor
       scope "Body text" 0
       (lambda (sent _client _document _id)
         (should (eq (orgzly--on-note-content-op
                      (list :scope scope :op "clock-in" :id (alist-get 'id entry)) nil)
                     'accepted))
         (let ((params (cdar (funcall sent))))
           (should (= (plist-get params :start) 0))
           (should (= (plist-get params :del) (length "Body text")))
           (should (string-match-p "CLOCK: \\[" (plist-get params :text))))
         (should-not orgzly--reset-fields))))))

(ert-deftest orgzly-current-editor-text-feeds-the-draft ()
  "Every reported editor text lands in the draft of its form, so Done and
the view read what the device shows; another document is ignored."
  (let ((orgzly-state-draft nil))
    (orgzly--on-edit-change nil "scratch.el" "hub-eval" "(+ 1 2)")
    (should-not orgzly-state-draft)
    (orgzly--on-edit-change nil (orgzly-ui-content-document "form") "x" "typed body")
    (should (equal orgzly-state-draft '(:scope "form" :content "typed body")))
    (orgzly--on-edit-change nil (orgzly-ui-content-document "form") "x" nil)
    (should (equal (plist-get orgzly-state-draft :content) "typed body"))))

(ert-deftest orgzly-current-save-keeps-a-note-without-priority ()
  "Saving a note that has no priority, with none picked, must not ask Org
to remove a cookie that is not there."
  (orgzly-test-with-snapshot
    (let* ((entry (cl-find-if (lambda (e) (null (alist-get 'priority e)))
                              (orgzly-state-entries "gtd")))
           (id (alist-get 'id entry))
           (orgzly-field-scope (orgzly-field-scope (list :kind 'note :id id)))
           (fields (list :fields
                         (cl-loop for (key value) on (list :oz-title "Kept title" :oz-tags ""
                                                           :oz-priority "" :oz-state ""
                                                           :oz-scheduled "" :oz-deadline "")
                                  by #'cddr
                                  append (list (intern (concat ":" (orgzly-field-id (substring (symbol-name key) 1))))
                                               value)))))
      (should entry)
      (should (eq 'accepted (orgzly--on-note-save (list :id id) fields)))
      ;; The save refreshed the snapshot, so the note is found by its title.
      (let ((saved (cl-find "Kept title" (orgzly-state-entries "gtd")
                            :key (lambda (e) (alist-get 'title e)) :test #'equal)))
        (should saved)
        (should (null (alist-get 'priority saved)))))))

(ert-deftest orgzly-current-refused-save-is-told-to-the-device ()
  "A refused submission toasts its reason, so a Save that cannot commit
does not leave the user in a note whose prompt returns on every Back."
  (let (toasts)
    (cl-letf (((symbol-function 'jetpacs-toast) (lambda (text &rest _) (push text toasts))))
      (should (eq 'rejected (orgzly--guard (lambda () (error "Invalid note fields")))))
      (should (equal toasts '("Orgzly: Invalid note fields")))
      (should (eq 'stale (orgzly--guard (lambda () (signal 'orgzly-stale nil)))))
      (should (= (length toasts) 2)))))

;;; Revived's note popup (SPEC 17.3 popup swipe sides, amendment #207)

(ert-deftest orgzly-current-rows-fling-open-revived-popups ()
  "Every outline row carries two popup swipe sides with Revived's default
buttons; a search row keeps only the right one."
  (orgzly-test-with-snapshot
    (let* ((json (jetpacs-node->canonical-json (orgzly-ui-screen '(:kind book :book "gtd"))))
           (entry (car (orgzly-state-entries "gtd")))
           (swipes (orgzly-ui-note-swipes (alist-get 'id entry)))
           (start (plist-get swipes :swipe-start))
           (end (plist-get swipes :swipe-end)))
      (should (string-search "\"presentation\":\"popup\"" json))
      (should (equal (mapcar (lambda (a) (plist-get a :label)) (append (plist-get start :actions) nil))
                     '("Schedule" "Deadline" "State" "Done")))
      (should (equal (mapcar (lambda (a) (plist-get a :icon)) (append (plist-get end :actions) nil))
                     '("delete" "add_above" "add_under" "add_below")))
      (should (equal (plist-get (plist-get (aref (plist-get end :actions) 0) :on_trigger) :confirm)
                     "Delete this note?"))
      (should-not (plist-member (orgzly-ui-note-swipes (alist-get 'id entry) t) :swipe-end))
      (should (string-search "\"presentation\":\"popup\""
                             (jetpacs-node->canonical-json
                              (orgzly-ui-screen '(:kind query :query "i.todo"))))))))

(ert-deftest orgzly-current-popup-places-a-new-note-against-a-row ()
  "New above and below open a new-note form against the sibling, and Done
creates the note there; new under goes beneath it as a child."
  (orgzly-test-with-snapshot
    (let* ((entries (orgzly-state-entries "gtd"))
           (second (nth 1 entries)) (id (alist-get 'id second)) opened)
      (cl-letf (((symbol-function 'orgzly--open) (lambda (route &rest _) (setq opened route))))
        (should (eq (orgzly--on-note-place (list :id id :place "above") nil) 'accepted))
        (should (equal (plist-get opened :sibling-id) id))
        (should (plist-get opened :before))
        (should (eq (orgzly--on-note-place (list :id id :place "under") nil) 'accepted))
        (should (equal (plist-get opened :parent-id) id))
        (should (eq (orgzly--on-note-place (list :id id :place "sideways") nil) 'rejected)))
      (let* ((route (list :kind 'new-note :book "gtd" :book-id (alist-get 'id (orgzly-state-book "gtd"))
                          :sibling-id id :before t))
             (args (orgzly-ui--note-args route))
             (orgzly-field-scope (orgzly-field-scope route))
             (fields (list :fields (cl-loop for (key value) on (list :oz-title "Placed above" :oz-tags ""
                                                                     :oz-priority "" :oz-state ""
                                                                     :oz-scheduled "" :oz-deadline "")
                                            by #'cddr
                                            append (list (intern (concat ":" (orgzly-field-id (substring (symbol-name key) 1))))
                                                         value)))))
        (should (equal (plist-get args :sibling-id) id))
        (should (eq (orgzly--on-note-save args fields) 'accepted))
        (let ((titles (mapcar (lambda (e) (alist-get 'title e)) (orgzly-state-entries "gtd"))))
          (should (member (alist-get 'title second) titles))
          (should (= (cl-position "Placed above" titles :test #'equal)
                     (1- (cl-position (alist-get 'title second) titles :test #'equal)))))))))

(ert-deftest orgzly-current-popup-writes-planning-and-state-at-once ()
  "Set in a row's timestamp dialog writes the planning line; a picked state
is written without a form; both refresh in place."
  (orgzly-test-with-snapshot
    (let* ((entry (car (orgzly-state-entries "gtd"))) (id (alist-get 'id entry))
           (title (alist-get 'title entry)) pushed)
      (cl-letf (((symbol-function 'jetpacs-shell-push) (lambda (&rest _) (setq pushed t)))
                ((symbol-function 'orgzly--later) (lambda (fn) (funcall fn)))
                ((symbol-function 'orgzly--ts-show) #'ignore))
        (should (eq (orgzly--on-note-plan (list :id id :field "deadline") nil) 'accepted))
        (should (equal (plist-get orgzly-state-ts :field) "row-deadline"))
        (should (equal (plist-get orgzly-state-ts :id) id))
        (setq orgzly-state-ts (plist-put orgzly-state-ts :date "2026-08-01"))
        (orgzly--ts-conclude "set")
        (let ((saved (cl-find title (orgzly-state-entries "gtd") :key (lambda (e) (alist-get 'title e)) :test #'equal)))
          (should (string-prefix-p "<2026-08-01" (alist-get 'raw (alist-get 'deadline saved))))
          (should pushed)
          (setq pushed nil)
          (let (on-pick)
            (cl-letf (((symbol-function 'orgzly--choices)
                       (lambda (_id _title choices pick) (should (assoc "None" choices)) (setq on-pick pick))))
              (should (eq (orgzly--on-note-state (list :id (alist-get 'id saved)) nil) 'accepted)))
            (funcall on-pick "DONE")
            (should (equal (alist-get 'state (cl-find title (orgzly-state-entries "gtd")
                                                      :key (lambda (e) (alist-get 'title e)) :test #'equal))
                           "DONE"))
            (should pushed)))))))
