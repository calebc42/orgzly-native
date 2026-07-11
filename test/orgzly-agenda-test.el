;;; orgzly-agenda-test.el --- ERT tests for agenda expansion & reminders -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'orgzly-query)
(require 'orgzly-agenda)
(require 'orgzly-reminders)

(defconst orgzly-agenda-test--now (encode-time 0 30 14 9 7 2026)
  "2026-07-09 Thu 14:30 local.")

(defun orgzly-agenda-test--ts (y m d &optional hh mm repeater warning)
  `((time . ,(float-time (encode-time 0 (or mm 0) (or hh 0) d m y)))
    (has-time . ,(and hh t))
    (raw . "<ts>")
    (repeater . ,repeater)
    (warning . ,warning)))

(cl-defun orgzly-agenda-test--entry (&key (book "gtd") (pos 1) (title "N")
                                          state scheduled deadline events)
  `((book . ,book) (file . "/n/gtd.org") (pos . ,pos) (level . 1)
    (title . ,title) (state . ,state) (tags) (itags)
    (scheduled . ,scheduled) (deadline . ,deadline) (events . ,events)
    (content . "")))

(defun orgzly-agenda-test--ctx ()
  (orgzly-query-context :now orgzly-agenda-test--now
                        :todo-keywords '("TODO") :done-keywords '("DONE")))

;; ─── Occurrence expansion ────────────────────────────────────────────────────

(ert-deftest orgzly-agenda-occurrence-plain ()
  (let* ((ws (orgzly-agenda--day-start 0 orgzly-agenda-test--now))
         (we (orgzly-agenda--day-start 7 orgzly-agenda-test--now))
         (in (orgzly-agenda-test--ts 2026 7 12))
         (out (orgzly-agenda-test--ts 2026 7 30))
         (past (orgzly-agenda-test--ts 2026 7 1)))
    (should (= (length (orgzly-agenda--occurrences in ws we)) 1))
    (should (= (length (orgzly-agenda--occurrences out ws we)) 0))
    ;; Overdue base survives as one occurrence unless suppressed.
    (should (= (length (orgzly-agenda--occurrences past ws we)) 1))
    (should (= (length (orgzly-agenda--occurrences past ws we t)) 0))))

(ert-deftest orgzly-agenda-occurrence-repeater ()
  (let* ((ws (orgzly-agenda--day-start 0 orgzly-agenda-test--now))
         (we (orgzly-agenda--day-start 14 orgzly-agenda-test--now))
         ;; Weekly from Jul 2: catches up to Jul 9, then 16, 23, 30…
         (rep (orgzly-agenda-test--ts 2026 7 2 nil nil "+1w"))
         (occs (orgzly-agenda--occurrences rep ws we t)))
    (should (equal (mapcar (lambda (o)
                             (format-time-string "%m-%d" (seconds-to-time o)))
                           occs)
                   '("07-09" "07-16")))))

(ert-deftest orgzly-agenda-occurrence-monthly-repeater ()
  (let* ((ws (orgzly-agenda--day-start 0 orgzly-agenda-test--now))
         (we (orgzly-agenda--day-start 60 orgzly-agenda-test--now))
         (rep (orgzly-agenda-test--ts 2026 1 15 nil nil "+1m"))
         (occs (orgzly-agenda--occurrences rep ws we t)))
    (should (equal (mapcar (lambda (o)
                             (format-time-string "%m-%d" (seconds-to-time o)))
                           occs)
                   '("07-15" "08-15")))))

;; ─── Day grouping ────────────────────────────────────────────────────────────

(ert-deftest orgzly-agenda-items-overdue-into-today ()
  (let* ((entries (list (orgzly-agenda-test--entry
                         :title "late" :state "TODO"
                         :scheduled (orgzly-agenda-test--ts 2026 7 1))
                        (orgzly-agenda-test--entry
                         :title "today" :pos 2 :state "TODO"
                         :scheduled (orgzly-agenda-test--ts 2026 7 9 9 0))
                        (orgzly-agenda-test--entry
                         :title "friday" :pos 3 :state "TODO"
                         :deadline (orgzly-agenda-test--ts 2026 7 10))))
         (query (orgzly-query-parse ".it.done ad.7"))
         (groups (orgzly-agenda-day-groups entries query
                                           (orgzly-agenda-test--ctx)
                                           orgzly-agenda-test--now)))
    (should (= (length groups) 7))
    ;; Today: the overdue item first (midnight base), then the 9:00 one.
    (let ((today-titles (mapcar (lambda (it)
                                  (alist-get 'title (alist-get 'entry it)))
                                (cdr (nth 0 groups)))))
      (should (equal today-titles '("late" "today"))))
    (should (equal (mapcar (lambda (it)
                             (alist-get 'title (alist-get 'entry it)))
                           (cdr (nth 1 groups)))
                   '("friday")))
    ;; The overdue item is qualified with how many days late it is.
    (should (= (alist-get 'overdue-days (car (cdr (nth 0 groups)))) 8))))

(ert-deftest orgzly-agenda-sections-overdue-first ()
  "Overdue occurrences sit in a leading Overdue section, Orgzly-style —
unless grouping overdue scheduled notes with today is on."
  (let* ((entries (list (orgzly-agenda-test--entry
                         :title "late" :state "TODO"
                         :scheduled (orgzly-agenda-test--ts 2026 7 1))
                        (orgzly-agenda-test--entry
                         :title "today" :pos 2 :state "TODO"
                         :scheduled (orgzly-agenda-test--ts 2026 7 9 9 0))))
         (query (orgzly-query-parse ".it.done ad.7"))
         (titles (lambda (items)
                   (mapcar (lambda (it)
                             (alist-get 'title (alist-get 'entry it)))
                           items))))
    (let* ((orgzly-agenda-group-scheduled-with-today nil)
           (sections (orgzly-agenda-sections entries query
                                             (orgzly-agenda-test--ctx)
                                             orgzly-agenda-test--now)))
      (should (eq (car (nth 0 sections)) 'overdue))
      (should (equal (funcall titles (cdr (nth 0 sections))) '("late")))
      ;; 7 day buckets follow; today keeps only its own item.
      (should (= (length sections) 8))
      (should (equal (funcall titles (cdr (nth 1 sections))) '("today"))))
    (let* ((orgzly-agenda-group-scheduled-with-today t)
           (sections (orgzly-agenda-sections entries query
                                             (orgzly-agenda-test--ctx)
                                             orgzly-agenda-test--now)))
      ;; No Overdue section: the late scheduled note groups under today.
      (should (= (length sections) 7))
      (should (equal (funcall titles (cdr (nth 0 sections)))
                     '("late" "today"))))))

(ert-deftest orgzly-agenda-done-drops-overdue ()
  "A DONE note's overdue scheduled time must not haunt today."
  (let* ((entries (list (orgzly-agenda-test--entry
                         :title "done late" :state "DONE"
                         :scheduled (orgzly-agenda-test--ts 2026 7 1))))
         (query (orgzly-query-parse "ad.7"))   ; no state filter
         (items (orgzly-agenda-items entries query
                                     (orgzly-agenda-test--ctx)
                                     orgzly-agenda-test--now)))
    (should (null items))))

(ert-deftest orgzly-agenda-event-on-its-day ()
  (let* ((entries (list (orgzly-agenda-test--entry
                         :title "meet"
                         :events (list (orgzly-agenda-test--ts 2026 7 11 10 0)))))
         (query (orgzly-query-parse "ad.7"))
         (groups (orgzly-agenda-day-groups entries query
                                           (orgzly-agenda-test--ctx)
                                           orgzly-agenda-test--now)))
    (should (null (cdr (nth 0 groups))))
    (should (= (length (cdr (nth 2 groups))) 1))
    (should (eq (alist-get 'kind (car (cdr (nth 2 groups)))) 'event))))

;; ─── Reminders ───────────────────────────────────────────────────────────────

(ert-deftest orgzly-reminders-timed-and-date-only ()
  (let* ((orgzly-reminders-scheduled t)
         (orgzly-reminders-deadline t)
         (orgzly-reminders-event nil)
         (orgzly-reminders-daily-time "09:00")
         (timed (orgzly-agenda-test--entry
                 :title "timed" :state "TODO"
                 :scheduled (orgzly-agenda-test--ts 2026 7 9 16 0)))
         (date-only (orgzly-agenda-test--entry
                     :title "tomorrow" :pos 2 :state "TODO"
                     :deadline (orgzly-agenda-test--ts 2026 7 10)))
         (past-daily (orgzly-agenda-test--entry
                      :title "today-date-only" :pos 3 :state "TODO"
                      :scheduled (orgzly-agenda-test--ts 2026 7 9)))
         (done (orgzly-agenda-test--entry
                :title "done" :pos 4 :state "DONE"
                :scheduled (orgzly-agenda-test--ts 2026 7 9 18 0)))
         (far (orgzly-agenda-test--entry
               :title "far" :pos 5 :state "TODO"
               :scheduled (orgzly-agenda-test--ts 2026 8 20 10 0)))
         (now (float-time orgzly-agenda-test--now))
         (specs (apply #'append
                       (mapcar (lambda (e)
                                 (orgzly-reminders--entry-specs
                                  e now '("DONE")))
                               (list timed date-only past-daily done far))))
         (titles (mapcar (lambda (s) (alist-get 'title s)) specs)))
    ;; timed → 16:00 today; date-only tomorrow → 09:00 tomorrow;
    ;; date-only today → 09:00 already past, skipped; done → skipped;
    ;; and — Orgzly's model — a time weeks out is armed now, unbounded.
    (should (member "timed" titles))
    (should (member "tomorrow" titles))
    (should-not (member "today-date-only" titles))
    (should-not (member "done" titles))
    (should (member "far" titles))
    (let ((tom (cl-find "tomorrow" specs
                        :key (lambda (s) (alist-get 'title s)) :test #'equal)))
      (should (equal (format-time-string
                      "%m-%d %H:%M"
                      (seconds-to-time (/ (alist-get 'at_ms tom) 1000)))
                     "07-10 09:00")))))

(ert-deftest orgzly-reminders-warning-lead ()
  "Deadline/event reminders fire early by the warning cookie; scheduled don't."
  (let* ((orgzly-reminders-daily-time "09:00")
         (now (float-time orgzly-agenda-test--now))
         (daily (* 9 3600))
         ;; Deadline Jul 20 with -5d warning → reminds Jul 15 09:00.
         (at (orgzly-reminders--next-at
              (orgzly-agenda-test--ts 2026 7 20 nil nil nil "-5d")
              now daily t)))
    (should (equal (format-time-string "%m-%d %H:%M" (seconds-to-time at))
                   "07-15 09:00"))
    ;; Unwarned classes ignore the cookie.
    (should (equal (format-time-string
                    "%m-%d" (seconds-to-time
                             (orgzly-reminders--next-at
                              (orgzly-agenda-test--ts 2026 7 20 nil nil nil "-5d")
                              now daily nil)))
                   "07-20"))
    ;; A repeater steps past occurrences whose led time already passed.
    (should (equal (format-time-string
                    "%m-%d" (seconds-to-time
                             (orgzly-reminders--next-at
                              (orgzly-agenda-test--ts 2026 7 1 nil nil "+1w" "-2d")
                              now daily t)))
                   "07-13"))))

(ert-deftest orgzly-agenda-multi-day-range ()
  "A <a>--<b> range fills every day of the range inside the window."
  (let* ((ws (orgzly-agenda--day-start 0 orgzly-agenda-test--now))
         (we (orgzly-agenda--day-start 7 orgzly-agenda-test--now))
         ;; Jul 10 through Jul 12: three daily occurrences.
         (ranged (append (orgzly-agenda-test--ts 2026 7 10)
                         `((range-end
                            . ,(float-time (encode-time 0 0 0 12 7 2026))))))
         (occs (orgzly-agenda--occurrences ranged ws we t)))
    (should (equal (mapcar (lambda (o)
                             (format-time-string "%m-%d" (seconds-to-time o)))
                           occs)
                   '("07-10" "07-11" "07-12")))))

(ert-deftest orgzly-agenda-deadline-warning-period ()
  "A warned deadline enters the agenda early, at max(today, due - warning)."
  (let* ((ctx (orgzly-agenda-test--ctx))
         (query (orgzly-query-parse "ad.7"))
         ;; Due Jul 20 (outside ad.7), -5d warning → surfaces Jul 15.
         (warned (orgzly-agenda-test--entry
                  :title "warned" :state "TODO"
                  :deadline (orgzly-agenda-test--ts 2026 7 20 nil nil nil "-5d")))
         ;; Due Jul 10, -5d warning → warning start already past: today.
         (looming (orgzly-agenda-test--entry
                   :title "looming" :pos 2 :state "TODO"
                   :deadline (orgzly-agenda-test--ts 2026 7 10 nil nil nil "-5d")))
         (items (orgzly-agenda-items (list warned looming) query ctx
                                     orgzly-agenda-test--now)))
    (should (equal (mapcar (lambda (it)
                             (cons (alist-get 'title (alist-get 'entry it))
                                   (format-time-string
                                    "%m-%d" (seconds-to-time
                                             (alist-get 'day it)))))
                           items)
                   '(("looming" . "07-09") ("warned" . "07-15"))))
    ;; Neither is overdue — the due dates are ahead.
    (should-not (cl-some (lambda (it) (alist-get 'overdue-days it)) items))))

(provide 'orgzly-agenda-test)
;;; orgzly-agenda-test.el ends here
