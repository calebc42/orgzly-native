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

(ert-deftest orgzly-data-getting-started-import ()
  (orgzly-data-test--with-books
   (should (equal (orgzly-data-import-getting-started)
                  orgzly-data-getting-started-book))
   ;; A real, listed book whose outline parses.
   (should (member orgzly-data-getting-started-book
                   (mapcar (lambda (b) (alist-get 'name b))
                           (orgzly-data-books))))
   (let ((entries (orgzly-data-entries orgzly-data-getting-started-book)))
     (should (> (length entries) 20))
     (should (cl-find "Notes" entries
                      :key (lambda (e) (alist-get 'title e)) :test #'equal)))
   (should (equal (orgzly-data-book-preface orgzly-data-getting-started-book)
                  "Welcome to Orgzly!"))
   ;; Re-import resets an existing copy, like Orgzly's.
   (with-temp-file (orgzly-data-book-file orgzly-data-getting-started-book)
     (insert "* Scribbled over\n"))
   (orgzly-data-invalidate)
   (orgzly-data-import-getting-started)
   (should (equal (orgzly-data-book-preface orgzly-data-getting-started-book)
                  "Welcome to Orgzly!"))))

(ert-deftest orgzly-data-book-front-matter ()
  (orgzly-data-test--with-books
   (with-temp-file (expand-file-name "health.org" orgzly-directory)
     (insert "#+TITLE: Health & Fitness\n"
             "#+CREATED: [2026-07-07 Tue 14:43]\n"
             "#+STARTUP: overview\n"
             "#+TODO: TODO IN-PROGRESS | DONE CANCELLED\n"
             "#+FILETAGS: :fitness:log:\n"
             "\nPlain prose.\n\n* First\n"))
   (orgzly-data-invalidate)
   (let ((fm (orgzly-data-book-front-matter "health")))
     (should (equal (alist-get 'title fm) "Health & Fitness"))
     (should (equal (alist-get 'raw (alist-get 'created fm))
                    "[2026-07-07 Tue 14:43]"))
     (should (equal (alist-get 'filetags fm) '("fitness" "log")))
     (should (equal (alist-get 'settings fm)
                    '(("STARTUP" . "overview")
                      ("TODO" . "TODO IN-PROGRESS | DONE CANCELLED"))))
     (should (equal (alist-get 'body fm) "Plain prose.")))
   ;; Prose-only preface: just the body; no preface at all: nil.
   (let ((fm (orgzly-data-book-front-matter "inbox")))
     (should (equal (alist-get 'body fm) "Inbox preface text."))
     (should-not (alist-get 'title fm))
     (should-not (alist-get 'settings fm)))
   (should-not (orgzly-data-book-front-matter "gtd"))))

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
     ;; Default sort for an s. query is scheduled time: on the same
     ;; day, timed entries precede date-only ones (upstream's
     ;; `hour IS NULL' tiebreak).
     (should (equal (mapcar (lambda (e) (alist-get 'title e)) hits)
                    '("Call mom" "Water plants"))))))

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
     (orgzly-data-set-planning ref 'deadline '((date . "2026-08-01")))
     (let ((e (orgzly-data-test--entry "Loose thought")))
       (should (alist-get 'time (alist-get 'deadline e)))
       (should-not (alist-get 'has-time (alist-get 'deadline e))))
     (orgzly-data-set-planning ref 'deadline nil)
     (should-not (alist-get 'deadline (orgzly-data-test--entry "Loose thought")))
     (orgzly-data-set-content ref "New body\nSecond line")
     (let ((e (orgzly-data-test--entry "Loose thought")))
       (should (equal (alist-get 'content e) "New body\nSecond line"))
       (should (= (alist-get 'content-lines e) 2))))))

(ert-deftest orgzly-data-parse-ts-cookies ()
  "End times and warning/delay cookies are captured, not just start+repeater."
  (let ((ts (orgzly-data--parse-ts "<2026-07-20 Mon 14:00-15:30 +1w -2d>")))
    (should (alist-get 'has-time ts))
    (should (equal (alist-get 'repeater ts) "+1w"))
    (should (equal (alist-get 'warning ts) "-2d"))
    (should (equal (format-time-string
                    "%H:%M" (seconds-to-time (alist-get 'end-time ts)))
                   "15:30")))
  ;; First-only delay; no false cookie hits from dates or time ranges.
  (should (equal (alist-get 'warning
                            (orgzly-data--parse-ts "<2026-07-20 Mon --3d>"))
                 "--3d"))
  (let ((plain (orgzly-data--parse-ts "<2026-07-20 Mon 10:00-12:00>")))
    (should-not (alist-get 'warning plain))
    (should-not (alist-get 'repeater plain))))

(ert-deftest orgzly-data-set-planning-round-trip ()
  "Every cookie survives a planning rewrite (org-schedule drops them)."
  (orgzly-data-test--with-books
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Loose thought"))))
     (orgzly-data-set-planning ref 'scheduled
                               '((date . "2026-07-20") (time . "14:00")
                                 (end . "15:30") (repeater . "+1w")
                                 (warning . "-2d")))
     (let ((ts (alist-get 'scheduled (orgzly-data-test--entry "Loose thought"))))
       (should (alist-get 'has-time ts))
       (should (equal (alist-get 'repeater ts) "+1w"))
       (should (equal (alist-get 'warning ts) "-2d"))
       (should (equal (format-time-string
                       "%H:%M" (seconds-to-time (alist-get 'end-time ts)))
                      "15:30")))
     ;; Re-setting just the date keeps nothing implicitly — the caller
     ;; passes the full parts; here without cookies they are gone.
     (orgzly-data-set-planning ref 'scheduled '((date . "2026-07-21")))
     (let ((ts (alist-get 'scheduled (orgzly-data-test--entry "Loose thought"))))
       (should-not (alist-get 'repeater ts))
       (should-not (alist-get 'warning ts))))))

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

(ert-deftest orgzly-data-new-sibling ()
  (orgzly-data-test--with-books
   ;; Below: same level, directly after the sibling's subtree.
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Ship report"))))
     (orgzly-data-new-note "gtd" "Between" :sibling ref)
     (should (= (alist-get 'level (orgzly-data-test--entry "Between")) 2))
     (let ((titles (mapcar (lambda (x) (alist-get 'title x))
                           (orgzly-data-entries "gtd"))))
       (should (equal (seq-take (member "Ship report" titles) 3)
                      '("Ship report" "Between" "Water plants")))))
   ;; Above: same level, directly before the sibling.
   (let ((ref (orgzly-data-entry-ref (orgzly-data-test--entry "Projects"))))
     (orgzly-data-new-note "gtd" "Preface note" :sibling ref :before t)
     (should (= (alist-get 'level (orgzly-data-test--entry "Preface note")) 1))
     (let ((titles (mapcar (lambda (x) (alist-get 'title x))
                           (orgzly-data-entries "gtd"))))
       (should (equal (seq-take (member "Preface note" titles) 2)
                      '("Preface note" "Projects")))))))

(ert-deftest orgzly-data-batch-cut-copy-paste ()
  (orgzly-data-test--with-books
   ;; Batch-cut a level-2 and a level-1 note across books; one paste
   ;; restores both, re-rooted at the target level.
   (let ((refs (list (orgzly-data-entry-ref
                      (orgzly-data-test--entry "Ship report"))
                     (orgzly-data-entry-ref
                      (orgzly-data-test--entry "Loose thought")))))
     (orgzly-data-cut-notes refs)
     (should-not (orgzly-data-test--entry "Ship report"))
     (should-not (orgzly-data-test--entry "Loose thought"))
     (orgzly-data-paste-note "inbox")
     (should (= (alist-get 'level
                           (orgzly-data-test--entry "Ship report" "inbox"))
                1))
     (should (orgzly-data-test--entry "Loose thought" "inbox")))
   ;; Batch copy leaves the originals in place.
   (orgzly-data-copy-notes
    (list (orgzly-data-entry-ref (orgzly-data-test--entry "Water plants"))))
   (should (orgzly-data-test--entry "Water plants" "gtd"))
   (orgzly-data-paste-note "inbox")
   (should (orgzly-data-test--entry "Water plants" "inbox"))))

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
