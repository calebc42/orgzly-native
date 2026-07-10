;;; orgzly-data-test.el --- ERT tests for the books/notes data layer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'orgzly-data)
(require 'orgzly-query)

(defvar orgzly-data-test--dir nil)

(defconst orgzly-data-test--gtd "\
* TODO [#A] Call mom :family:phone:
SCHEDULED: <2026-07-09 Thu 10:00>
:PROPERTIES:
:CREATED: [2026-07-01 Wed 09:00]
:END:
about the trip
* Projects :project:
** DONE Ship report
CLOSED: [2026-07-08 Wed 18:00] DEADLINE: <2026-07-08 Wed>
** TODO Water plants
SCHEDULED: <2026-07-09 Thu +1w>
* Meeting <2026-07-15 Wed 09:00>
Bring the slides
")

(defconst orgzly-data-test--inbox "\
Inbox preface text.

* Loose thought
")

(defmacro orgzly-data-test--with-books (&rest body)
  "Run BODY with a fresh temp `orgzly-directory' holding the fixtures."
  `(let* ((orgzly-data-test--dir (make-temp-file "orgzly-test" t))
          (orgzly-directory orgzly-data-test--dir)
          (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELLED")))
          (orgzly-new-note-state nil)
          (orgzly-new-note-prepend nil)
          (org-inhibit-startup t))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "gtd.org" orgzly-directory)
             (insert orgzly-data-test--gtd))
           (with-temp-file (expand-file-name "inbox.org" orgzly-directory)
             (insert orgzly-data-test--inbox))
           (orgzly-data-invalidate)
           ,@body)
       ;; Kill buffers visiting temp files, then remove the tree.
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p orgzly-data-test--dir
                                     (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory orgzly-data-test--dir t))))

(defun orgzly-data-test--entry (title &optional book)
  "Entry whose title is TITLE or starts with it (\"Meeting <ts>\")."
  (cl-find-if (lambda (e) (string-prefix-p title (alist-get 'title e)))
              (orgzly-data-entries book)))

;; ─── Books ───────────────────────────────────────────────────────────────────

(ert-deftest orgzly-data-books-list ()
  (orgzly-data-test--with-books
   (let ((books (orgzly-data-books)))
     (should (equal (mapcar (lambda (b) (alist-get 'name b)) books)
                    '("gtd" "inbox")))
     (should (= (alist-get 'count (car books)) 5))
     (should (= (alist-get 'count (cadr books)) 1)))))

(ert-deftest orgzly-data-book-crud ()
  (orgzly-data-test--with-books
   (orgzly-data-create-book "work")
   (should (file-exists-p (orgzly-data-book-file "work")))
   (should-error (orgzly-data-create-book "work"))
   (orgzly-data-rename-book "work" "job")
   (should-not (file-exists-p (orgzly-data-book-file "work")))
   (should (file-exists-p (orgzly-data-book-file "job")))
   (orgzly-data-delete-book "job")
   (should-not (file-exists-p (orgzly-data-book-file "job")))))

(ert-deftest orgzly-data-book-preface ()
  (orgzly-data-test--with-books
   (should (equal (orgzly-data-book-preface "inbox") "Inbox preface text."))
   (should-not (orgzly-data-book-preface "gtd"))))

;; ─── Entry extraction ────────────────────────────────────────────────────────

(ert-deftest orgzly-data-entry-fields ()
  (orgzly-data-test--with-books
   (let ((e (orgzly-data-test--entry "Call mom")))
     (should e)
     (should (equal (alist-get 'book e) "gtd"))
     (should (equal (alist-get 'state e) "TODO"))
     (should (equal (alist-get 'priority e) "A"))
     (should (equal (alist-get 'tags e) '("family" "phone")))
     (should (equal (alist-get 'content e) "about the trip"))
     (should (= (alist-get 'content-lines e) 1))
     (should (alist-get 'time (alist-get 'scheduled e)))
     (should (alist-get 'has-time (alist-get 'scheduled e)))
     (should (alist-get 'time (alist-get 'created e))))))

(ert-deftest orgzly-data-entry-inherited-tags ()
  (orgzly-data-test--with-books
   (let ((e (orgzly-data-test--entry "Ship report")))
     (should (equal (alist-get 'itags e) '("project")))
     (should-not (alist-get 'tags e))
     (should (alist-get 'time (alist-get 'closed e)))
     (should (alist-get 'time (alist-get 'deadline e))))))

(ert-deftest orgzly-data-entry-repeater-and-children ()
  (orgzly-data-test--with-books
   (let ((water (orgzly-data-test--entry "Water plants"))
         (parent (orgzly-data-test--entry "Projects")))
     (should (equal (alist-get 'repeater (alist-get 'scheduled water)) "+1w"))
     (should (alist-get 'has-children parent))
     (should-not (alist-get 'has-children water)))))

(ert-deftest orgzly-data-entry-events ()
  (orgzly-data-test--with-books
   (let ((e (orgzly-data-test--entry "Meeting")))
     (should (= (length (alist-get 'events e)) 1))
     (should (equal (alist-get 'content e) "Bring the slides")))))

;; ─── Query over real entries ─────────────────────────────────────────────────

(ert-deftest orgzly-data-query-integration ()
  (orgzly-data-test--with-books
   (let* ((ctx (orgzly-query-context
                :now (encode-time 0 30 14 9 7 2026)
                :todo-keywords '("TODO" "NEXT") :done-keywords '("DONE" "CANCELLED")
                :default-priority "B"))
          (hits (orgzly-query-select
                 (orgzly-data-entries)
                 (orgzly-query-parse "i.todo s.today")
                 ctx)))
     ;; Default sort for an s. query is scheduled time: the date-only
     ;; timestamp (midnight) precedes the 10:00 one, as in Orgzly.
     (should (equal (mapcar (lambda (e) (alist-get 'title e)) hits)
                    '("Water plants" "Call mom"))))))

;; ─── Mutations ───────────────────────────────────────────────────────────────

(ert-deftest orgzly-data-set-state-priority-tags ()
  (orgzly-data-test--with-books
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Loose thought"))))
     (orgzly-data-set-state ref "NEXT")
     (should (equal (alist-get 'state (orgzly-data-test--entry "Loose thought"))
                    "NEXT"))
     (orgzly-data-set-priority ref "C")
     (should (equal (alist-get 'priority (orgzly-data-test--entry "Loose thought"))
                    "C"))
     (orgzly-data-set-tags ref '("someday"))
     (should (equal (alist-get 'tags (orgzly-data-test--entry "Loose thought"))
                    '("someday")))
     (orgzly-data-set-state ref nil)
     (should-not (alist-get 'state (orgzly-data-test--entry "Loose thought"))))))

(ert-deftest orgzly-data-set-planning-and-content ()
  (orgzly-data-test--with-books
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Loose thought"))))
     (orgzly-data-set-planning ref 'deadline "2026-08-01")
     (let ((e (orgzly-data-test--entry "Loose thought")))
       (should (alist-get 'time (alist-get 'deadline e)))
       (should-not (alist-get 'has-time (alist-get 'deadline e))))
     (orgzly-data-set-planning ref 'deadline nil)
     (should-not (alist-get 'deadline (orgzly-data-test--entry "Loose thought")))
     (orgzly-data-set-content ref "New body\nSecond line")
     (let ((e (orgzly-data-test--entry "Loose thought")))
       (should (equal (alist-get 'content e) "New body\nSecond line"))
       (should (= (alist-get 'content-lines e) 2))))))

(ert-deftest orgzly-data-new-note-defaults ()
  (orgzly-data-test--with-books
   (let ((orgzly-new-note-state "TODO"))
     (orgzly-data-new-note "inbox" "Fresh idea" :tags '("new")
                           :scheduled "2026-07-20" :content "details"))
   (let ((e (orgzly-data-test--entry "Fresh idea")))
     (should e)
     (should (equal (alist-get 'state e) "TODO"))
     (should (equal (alist-get 'tags e) '("new")))
     (should (alist-get 'scheduled e))
     (should (alist-get 'created e))     ; CREATED stamped by default
     (should (equal (alist-get 'content e) "details")))))

(ert-deftest orgzly-data-new-note-under-parent ()
  (orgzly-data-test--with-books
   (let ((parent (orgzly-data-entry-ref (orgzly-data-test--entry "Projects"))))
     (orgzly-data-new-note "gtd" "Subproject" :under parent)
     (let ((e (orgzly-data-test--entry "Subproject")))
       (should (= (alist-get 'level e) 2))
       (should (equal (alist-get 'itags e) '("project")))))))

(ert-deftest orgzly-data-delete-and-refile ()
  (orgzly-data-test--with-books
   (orgzly-data-delete-note
    (orgzly-data-entry-ref (orgzly-data-test--entry "Meeting")))
   (should-not (orgzly-data-test--entry "Meeting"))
   ;; Refile a level-2 note into another book: it lands at level 1.
   (orgzly-data-refile
    (orgzly-data-entry-ref (orgzly-data-test--entry "Water plants"))
    "inbox")
   (let ((e (orgzly-data-test--entry "Water plants")))
     (should (equal (alist-get 'book e) "inbox"))
     (should (= (alist-get 'level e) 1)))))

(ert-deftest orgzly-data-cut-copy-paste ()
  (orgzly-data-test--with-books
   (orgzly-data-copy-note
    (orgzly-data-entry-ref (orgzly-data-test--entry "Loose thought")))
   (orgzly-data-paste-note "gtd")
   (should (orgzly-data-test--entry "Loose thought" "gtd"))
   (should (orgzly-data-test--entry "Loose thought" "inbox"))
   (orgzly-data-cut-note
    (orgzly-data-entry-ref (orgzly-data-test--entry "Loose thought" "inbox")))
   (should-not (orgzly-data-test--entry "Loose thought" "inbox"))))

(ert-deftest orgzly-data-structure-ops ()
  (orgzly-data-test--with-books
   (orgzly-data-promote
    (orgzly-data-entry-ref (orgzly-data-test--entry "Ship report")))
   (should (= (alist-get 'level (orgzly-data-test--entry "Ship report")) 1))
   (orgzly-data-demote
    (orgzly-data-entry-ref (orgzly-data-test--entry "Ship report")))
   (should (= (alist-get 'level (orgzly-data-test--entry "Ship report")) 2))))

(ert-deftest orgzly-data-toggle-done-repeater-aware ()
  (orgzly-data-test--with-books
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Water plants")))
         (org-log-done nil)
         (org-log-repeat nil)
         (org-todo-log-states nil))
     ;; Repeating task: toggling done reschedules +1w and stays TODO.
     (orgzly-data-toggle-done ref)
     (let ((e (orgzly-data-test--entry "Water plants")))
       (should (equal (alist-get 'state e) "TODO"))
       (should (equal (alist-get 'raw (alist-get 'scheduled e))
                      "<2026-07-16 Thu +1w>")))
     ;; Non-repeating: toggles to DONE and back.
     (let ((ref2 (orgzly-data-entry-ref (orgzly-data-test--entry "Call mom"))))
       (orgzly-data-toggle-done ref2)
       (should (equal (alist-get 'state (orgzly-data-test--entry "Call mom"))
                      "DONE"))
       (orgzly-data-toggle-done ref2)
       (should (equal (alist-get 'state (orgzly-data-test--entry "Call mom"))
                      "TODO"))))))

(ert-deftest orgzly-data-todo-keywords-config ()
  (let ((org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE" "CANCELLED"))))
    (should (equal (orgzly-data-todo-keywords)
                   '(("TODO" "NEXT") . ("DONE" "CANCELLED")))))
  (let ((org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)"))))
    (should (equal (orgzly-data-todo-keywords) '(("TODO") . ("DONE")))))
  ;; No | separator: last keyword is the done state.
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))))
    (should (equal (orgzly-data-todo-keywords) '(("TODO") . ("DONE"))))))

(provide 'orgzly-data-test)
;;; orgzly-data-test.el ends here
