;;; orgzly-reminders.el --- Scheduled/deadline/event reminders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's reminder classes on the jetpacs `reminders.set' frame: exact
;; alarms for scheduled times, deadline times, and events, each behind its
;; own toggle; date-only items remind at `orgzly-reminders-daily-time'.
;; The set is replace-set semantics and persisted across reboots by the
;; companion, so each shell push simply re-derives the upcoming window.
;;
;; Companion-protocol limit (see docs/PARITY.md): the reminder
;; notification carries no Done/Snooze buttons — tapping it opens the
;; app, where the note is one tap away.

;;; Code:

(require 'cl-lib)
(require 'jetpacs)
(require 'jetpacs-shell)
(require 'orgzly-data)
(require 'orgzly-agenda)

(defcustom orgzly-reminders-scheduled t
  "Remind at scheduled times (Orgzly: reminders for scheduled)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-deadline t
  "Remind at deadline times (Orgzly: reminders for deadlines)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-event nil
  "Remind at event times — plain active timestamps (Orgzly: events)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-daily-time "09:00"
  "Clock time (HH:MM) at which date-only items remind."
  :type 'string :group 'orgzly)

(defcustom orgzly-reminders-horizon-hours 24
  "How far ahead reminders are armed; each push re-derives the window."
  :type 'integer :group 'orgzly)

(defun orgzly-reminders--daily-offset ()
  "Seconds past midnight of `orgzly-reminders-daily-time'."
  (if (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                    orgzly-reminders-daily-time)
      (+ (* 3600 (string-to-number (match-string 1 orgzly-reminders-daily-time)))
         (* 60 (string-to-number (match-string 2 orgzly-reminders-daily-time))))
      (* 9 3600)))

(defun orgzly-reminders--kinds ()
  (append (when orgzly-reminders-scheduled '(scheduled))
          (when orgzly-reminders-deadline '(deadline))
          (when orgzly-reminders-event '(event))))

(defun orgzly-reminders--entry-specs (entry now horizon done-keywords)
  "Reminder spec alists for ENTRY within [NOW, NOW+HORIZON)."
  (unless (member (alist-get 'state entry) done-keywords)
    (let ((end (+ now horizon))
          (daily (orgzly-reminders--daily-offset))
          specs)
      (dolist (kind (orgzly-reminders--kinds))
        (dolist (ts (if (eq kind 'event)
                        (alist-get 'events entry)
                      (when-let ((one (alist-get kind entry))) (list one))))
          ;; Expand from slightly in the past so a repeater's occurrence
          ;; earlier today still steps forward into the window.
          (dolist (occ (orgzly-agenda--occurrences ts now end t))
            (let ((at (if (alist-get 'has-time ts)
                          occ
                        ;; Date-only: remind at the daily reminder time.
                        (+ (orgzly-agenda--day-start
                            0 (seconds-to-time occ))
                           daily))))
              (when (and (> at now) (< at end))
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
                 specs))))))
      specs)))

(defun orgzly-reminders-upcoming (&optional now)
  "Every reminder spec within the horizon, across all books."
  (let* ((now (float-time (or now (current-time))))
         (horizon (* 3600 orgzly-reminders-horizon-hours))
         (done (cdr (orgzly-data-todo-keywords)))
         specs)
    (dolist (entry (orgzly-data-entries))
      (setq specs (nconc specs
                         (orgzly-reminders--entry-specs entry now horizon done))))
    (sort specs (lambda (a b) (< (alist-get 'at_ms a) (alist-get 'at_ms b))))))

(defvar orgzly-reminders--last 'unset
  "Previous reminder set, to suppress identical sends.")

(defun orgzly-reminders-sync ()
  "Push the upcoming reminders as a replace-set (memo-guarded)."
  (let ((rems (condition-case nil (orgzly-reminders-upcoming) (error nil))))
    (unless (equal rems orgzly-reminders--last)
      (setq orgzly-reminders--last rems)
      (jetpacs-send "reminders.set" `((reminders . ,(vconcat rems)))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly-reminders-sync)
(add-hook 'jetpacs-shell-refresh-hook
          (lambda () (setq orgzly-reminders--last 'unset)))

(provide 'orgzly-reminders)
;;; orgzly-reminders.el ends here
