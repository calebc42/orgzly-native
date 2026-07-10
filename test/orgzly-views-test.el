;;; orgzly-views-test.el --- Build & lint every view spec -*- lexical-binding: t; -*-

;; Loads the whole app against the vendored jetpacs core, points it at a
;; fixture notebook directory, builds every registered shell view in
;; every drill-in state, and lints the produced specs with jetpacs-lint —
;; the same validator the live push runs.  This is the wire-shape safety
;; net for the UI layer.

;;; Code:

(require 'ert)
(require 'orgzly)
(require 'jetpacs-lint)

(defconst orgzly-views-test--gtd "\
* TODO [#A] Call mom :family:phone:
SCHEDULED: <2026-07-09 Thu 10:00 +1w>
:PROPERTIES:
:CREATED: [2026-07-01 Wed 09:00]
:END:
about the trip
* Projects :project:
** DONE Ship report
CLOSED: [2026-07-08 Wed 18:00] DEADLINE: <2026-07-08 Wed>
** TODO Water plants
SCHEDULED: <2026-07-09 Thu>
* Meeting <2026-07-15 Wed 09:00>
Bring the slides
")

(defmacro orgzly-views-test--with-app (&rest body)
  `(let* ((dir (make-temp-file "orgzly-views" t))
          (orgzly-directory dir)
          (orgzly-default-book "gtd")
          (org-todo-keywords '((sequence "TODO" "|" "DONE")))
          (org-inhibit-startup t))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "gtd.org" dir)
             (insert orgzly-views-test--gtd))
           (with-temp-file (expand-file-name "inbox.org" dir)
             (insert "Preface here.\n\n* Loose thought\n"))
           (orgzly-data-invalidate)
           ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p dir (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

(defun orgzly-views-test--build-and-lint (name)
  "Build view NAME through the shell's builder; fail on lint problems."
  (let* ((entry (assoc name jetpacs-shell-views))
         (spec (funcall (plist-get (cdr entry) :builder) nil)))
    (should entry)
    (should spec)
    (let ((problems (jetpacs-lint-spec spec)))
      (should (equal (mapcar #'cdr problems) nil)))
    spec))

(ert-deftest orgzly-views-lint-tabs ()
  (orgzly-views-test--with-app
   (dolist (view '("books" "agenda" "search" "settings"))
     (orgzly-views-test--build-and-lint view))))

(ert-deftest orgzly-views-lint-book-view ()
  (orgzly-views-test--with-app
   (setq orgzly-ui--current-book "gtd")
   (orgzly-views-test--build-and-lint "book")
   ;; And in multi-select mode with a selection.
   (setq orgzly-ui--select-mode t
         orgzly-ui--selection
         (list (orgzly-data-entry-ref (car (orgzly-data-entries "gtd")))))
   (orgzly-views-test--build-and-lint "book")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-note-view ()
  (orgzly-views-test--with-app
   (dolist (entry (orgzly-data-entries))
     (setq orgzly-ui--note-ref (orgzly-data-entry-ref entry))
     (orgzly-views-test--build-and-lint "note"))
   ;; A dangling ref renders the not-found state, never an error.
   (setq orgzly-ui--note-ref '((file . "/nope.org") (pos . 1) (title . "x")))
   (orgzly-views-test--build-and-lint "note")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-preface-and-searches ()
  (orgzly-views-test--with-app
   (setq orgzly-ui--preface-book "inbox")
   (orgzly-views-test--build-and-lint "preface")
   (setq orgzly-search--manage t)
   (orgzly-views-test--build-and-lint "searches")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-search-results ()
  (orgzly-views-test--with-app
   ;; Flat query, agenda query, and a query error all render.
   (dolist (q '("i.todo" ".it.done ad.7" "call"))
     (setq orgzly-search--query q)
     (orgzly-views-test--build-and-lint "search"))
   (setq orgzly-search--query "")))

(ert-deftest orgzly-views-widget-and-reminders-specs ()
  (orgzly-views-test--with-app
   (let ((views (orgzly-widget--views)))
     (should (= (length views) (length orgzly-saved-searches)))
     (dolist (v views)
       (should (stringp (alist-get 'title (cdr v))))
       (should (vectorp (alist-get 'items (cdr v))))))
   (let ((specs (orgzly-reminders-upcoming
                 (encode-time 0 30 8 9 7 2026))))
     (dolist (s specs)
       (should (stringp (alist-get 'id s)))
       (should (integerp (alist-get 'at_ms s)))
       (should (stringp (alist-get 'title s)))))))

(ert-deftest orgzly-views-app-registered ()
  (orgzly-views-test--with-app
   (should (assoc "orgzly" (bound-and-true-p jetpacs-apps--registry)))))

(provide 'orgzly-views-test)
;;; orgzly-views-test.el ends here
