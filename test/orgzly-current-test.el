;;; orgzly-current-test.el --- Current applet contract tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'orgzly)
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
      (dolist (route (list '(:kind books) '(:kind book :book "gtd") '(:kind search) '(:kind agenda)
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
    (let ((json (jetpacs-node->canonical-json (orgzly-ui-screen '(:kind book :book "gtd")))))
      (should-not (string-match-p (regexp-quote orgzly-directory) json))
      (should-not (string-match-p "\"pos\":" json)))))

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

(provide 'orgzly-current-test)
;;; orgzly-current-test.el ends here
