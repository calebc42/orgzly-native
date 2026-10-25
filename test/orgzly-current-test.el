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

(provide 'orgzly-current-test)
;;; orgzly-current-test.el ends here
