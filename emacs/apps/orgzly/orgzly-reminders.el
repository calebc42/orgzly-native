;;; orgzly-reminders.el --- Scheduled/deadline/event reminders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;; Orgzly's reminder classes on the jetpacs `reminders.set' frame: exact
;; alarms for scheduled times, deadline times, and events, each behind its
;; own toggle; date-only items remind at `orgzly-reminders-daily-time',
;; and deadline/event reminders lead by the warning cookie.  Armed like
;; Orgzly: each note's next future occurrence, however far out.  The set
;; is replace-set semantics and persisted across reboots by the
;; companion; each shell push re-derives it.
;;
;; Companion-protocol limit (see docs/PARITY.md): the reminder
;; notification carries no Done/Snooze buttons — tapping it opens the
;; app, where the note is one tap away.

;;; Commentary:

;; Recovered Orgzly domain logic; current presentation lives in orgzly-ui.el.

;;; Code:

(require 'cl-lib)
(require 'orgzly-data)
(require 'orgzly-agenda)

(defcustom orgzly-reminders-scheduled t
  "Remind at scheduled times (Orgzly: reminders for scheduled)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-deadline t
  "Remind at deadline times (Orgzly: reminders for deadlines)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-event t
  "Remind at event times — plain active timestamps (Orgzly: events)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-daily-time "09:00"
  "Clock time (HH:MM) at which date-only items remind."
  :type 'string :group 'orgzly)

(defun orgzly-reminders--daily-offset ()
  "Seconds past midnight of `orgzly-reminders-daily-time'."
  (if (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                    orgzly-reminders-daily-time)
      (+ (* 3600 (string-to-number (match-string 1 orgzly-reminders-daily-time)))
         (* 60 (string-to-number (match-string 2 orgzly-reminders-daily-time))))
      (* 9 3600)))

(defun orgzly-reminders--kinds ()
  "Return the enabled planning reminder classes."
  (append (when orgzly-reminders-scheduled '(scheduled))
          (when orgzly-reminders-deadline '(deadline))
          (when orgzly-reminders-event '(event))))

(defun orgzly-reminders--next-at (ts now daily warned)
  "Reminder timestamp for TS's next occurrence after NOW, or nil.
The reminder time of an occurrence is the occurrence itself, led by the
warning/delay cookie when WARNED (Orgzly: deadlines and events), and
date-only stamps remind at DAILY seconds past midnight.  A repeater
steps until that reminder time is ahead of NOW — Orgzly arms the next
future occurrence with no horizon."
  (when-let* ((base (alist-get 'time ts)))
    (let* ((rep (orgzly-agenda--parse-repeater (alist-get 'repeater ts)))
           (warn (and warned (orgzly-agenda--parse-warning
                              (alist-get 'warning ts))))
           (at-of (lambda (occ)
                    (let ((at (if warn
                                  (orgzly-agenda--step
                                   occ (car warn) (- (cdr warn)))
                                occ)))
                      (if (alist-get 'has-time ts)
                          at
                        (+ (orgzly-agenda--day-start 0 (seconds-to-time at))
                           daily)))))
           (occ base)
           (at (funcall at-of base))
           (guard 0))
      (when rep
        (while (and (<= at now) (< guard 1000))
          (setq occ (orgzly-agenda--step occ (car rep) (cdr rep))
                at (funcall at-of occ)
                guard (1+ guard))))
      (and (> at now) at))))

(defun orgzly-reminders--entry-specs (entry now done-keywords)
  "Return ENTRY reminder alists after NOW, excluding DONE-KEYWORDS."
  (unless (member (alist-get 'state entry) done-keywords)
    (let ((daily (orgzly-reminders--daily-offset))
          specs)
      (dolist (kind (orgzly-reminders--kinds))
        (dolist (ts (if (eq kind 'event)
                        (alist-get 'events entry)
                      (when-let* ((one (alist-get kind entry))) (list one))))
          (when-let* ((at (orgzly-reminders--next-at
                          ts now daily (memq kind '(deadline event)))))
            (push
             `((id . ,(format "orgzly:%s:%s:%s:%d"
                              (alist-get 'file entry)
                              (alist-get 'pos entry) kind (truncate at)))
               (at_ms . ,(truncate (* at 1000)))
               (title . ,(alist-get 'title entry))
               (body . ,(format "%s · %s · %s"
                                (capitalize (symbol-name kind))
                                (format-time-string
                                 "%H:%M" (seconds-to-time at))
                                (alist-get 'book entry))))
             specs))))
      specs)))

(defun orgzly-reminders-upcoming (&optional now)
  "Compute reminder candidates across all books at NOW.
This pure planning API does not arm or send platform reminders.
One per note and class: its next future occurrence, however far out
Orgzly's model; each push re-derives the set."
  (let* ((now (float-time (or now (current-time))))
         (done (cdr (orgzly-data-todo-keywords)))
         specs)
    (dolist (entry (orgzly-data-entries))
      (setq specs (nconc specs
                         (orgzly-reminders--entry-specs entry now done))))
    (sort specs (lambda (a b) (< (alist-get 'at_ms a) (alist-get 'at_ms b))))))

(provide 'orgzly-reminders)
;;; orgzly-reminders.el ends here
