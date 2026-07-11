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
   ;; "settings" is the stock core view — Orgzly's sections render
   ;; through it rather than an app-defined screen.
   (dolist (view '("orgzly.books" "orgzly.agenda" "orgzly.search" "settings"))
     (orgzly-views-test--build-and-lint view))))

(ert-deftest orgzly-views-lint-book-view ()
  (orgzly-views-test--with-app
   (setq orgzly-ui--current-book "gtd")
   (orgzly-views-test--build-and-lint "orgzly.book")
   ;; And in multi-select mode with a selection.
   (setq orgzly-ui--select-mode t
         orgzly-ui--selection
         (list (orgzly-data-entry-ref (car (orgzly-data-entries "gtd")))))
   (orgzly-views-test--build-and-lint "orgzly.book")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-note-view ()
  (orgzly-views-test--with-app
   (dolist (entry (orgzly-data-entries))
     (setq orgzly-ui--note-ref (orgzly-data-entry-ref entry))
     (orgzly-views-test--build-and-lint "orgzly.note"))
   ;; A dangling ref renders the not-found state, never an error.
   (setq orgzly-ui--note-ref '((file . "/nope.org") (pos . 1) (title . "x")))
   (orgzly-views-test--build-and-lint "orgzly.note")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-preface-and-searches ()
  (orgzly-views-test--with-app
   (setq orgzly-ui--preface-book "inbox")
   (orgzly-views-test--build-and-lint "orgzly.preface")
   (setq orgzly-search--manage t)
   (orgzly-views-test--build-and-lint "orgzly.searches")
   (orgzly-ui--reset-drill-in)))

(ert-deftest orgzly-views-lint-search-results ()
  (orgzly-views-test--with-app
   ;; Flat query, agenda query, and a query error all render.
   (dolist (q '("i.todo" ".it.done ad.7" "call"))
     (setq orgzly-search--query q)
     (orgzly-views-test--build-and-lint "orgzly.search"))
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

;; ─── Orgzly row semantics ────────────────────────────────────────────────────

(defun orgzly-views-test--spans (node)
  "The span alists of a rich-text NODE, as a list."
  (append (alist-get 'spans node) nil))

(ert-deftest orgzly-views-title-follows-orgzly ()
  "STATE  #A  Title  tags — red open state, green done, no colons, no strike."
  (orgzly-views-test--with-app
   (let* ((entries (orgzly-data-entries "gtd"))
          (call-mom (car entries))
          (shipped (cl-find "Ship report" entries
                            :key (lambda (e) (alist-get 'title e))
                            :test #'equal)))
     (let ((spans (orgzly-views-test--spans
                   (orgzly-ui--title-node call-mom))))
       ;; State: bold, Orgzly red for open keywords.
       (should (equal (alist-get 'text (nth 0 spans)) "TODO  "))
       (should (equal (alist-get 'color (nth 0 spans)) orgzly-ui--todo-color))
       (should (alist-get 'bold (nth 0 spans)))
       ;; Priority as "#A", not "[#A]", and never red.
       (should (equal (alist-get 'text (nth 1 spans)) "#A  "))
       ;; Tags space-separated, no colons, muted.
       (let ((tag-span (car (last spans))))
         (should (equal (alist-get 'text tag-span) "  family phone"))
         (should (equal (alist-get 'color tag-span) orgzly-ui--muted-color)))
       ;; No strikethrough anywhere (Orgzly fades, never strikes).
       (should-not (cl-some (lambda (s) (alist-get 'strike s)) spans)))
     (let ((spans (orgzly-views-test--spans
                   (orgzly-ui--title-node shipped))))
       ;; Done state: Orgzly green; the title fades to the muted color.
       (should (equal (alist-get 'color (nth 0 spans)) orgzly-ui--done-color))
       (should (equal (alist-get 'color (nth 1 spans))
                      orgzly-ui--muted-color))))))

(ert-deftest orgzly-views-title-line-count ()
  "The content line count shows only when content is hidden and the pref is on."
  (orgzly-views-test--with-app
   (let ((entry (car (orgzly-data-entries "gtd")))
         (orgzly-display-content-line-count t))
     (let ((spans (orgzly-views-test--spans
                   (orgzly-ui--title-node entry :hide-content t))))
       (should (equal (alist-get 'text (car (last spans))) "  1")))
     (let ((spans (orgzly-views-test--spans
                   (orgzly-ui--title-node entry))))
       (should-not (equal (alist-get 'text (car (last spans))) "  1"))))))

;; ─── Dialog specs ────────────────────────────────────────────────────────────

(ert-deftest orgzly-views-lint-dialogs ()
  "The quick popup, timestamp and tags dialogs produce wire-valid specs."
  (orgzly-views-test--with-app
   (let ((ref (orgzly-data-entry-ref (car (orgzly-data-entries "gtd")))))
     (dolist (spec (list (orgzly-ui--quick-dialog ref)
                         (orgzly-ui--plan-dialog ref "scheduled")
                         (orgzly-ui--plan-dialog ref "deadline")
                         (orgzly-ui--tags-dialog ref '("family"))))
       (should (equal (mapcar #'cdr (jetpacs-lint-spec spec)) nil))))))

;; ─── Drawer ──────────────────────────────────────────────────────────────────

(ert-deftest orgzly-views-drawer-lists-searches-and-books ()
  "The drawer band mirrors saved searches and notebooks, Orgzly-style."
  (orgzly-views-test--with-app
   (setq orgzly--drawer-memo nil)
   (orgzly--drawer-sync)
   (let* ((band (cl-remove-if-not
                 (lambda (e) (and (>= (car e) 30) (< (car e) 50)))
                 jetpacs-shell-drawer-items))
          (items (mapcar (lambda (e) (funcall (cadr e))) band))
          (labels (mapcar (lambda (i) (alist-get 'label i)) items)))
     (should (member "Searches" labels))
     (should (member "Notebooks" labels))
     (should (member "gtd" labels))
     (should (member "inbox" labels))
     (dolist (ss orgzly-saved-searches)
       (should (member (car ss) labels))))))

(provide 'orgzly-views-test)
;;; orgzly-views-test.el ends here
