;;; orgzly-reminders.el --- Scheduled/deadline/event reminders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Orgzly's reminder classes on the jetpacs `reminders.set' frame: exact
;; alarms for scheduled times, deadline times, and events, each behind its
;; own toggle; date-only items remind at `orgzly-reminders-daily-time',
;; and deadline/event reminders lead by the warning cookie.  Armed like
;; Orgzly: each note's next future occurrence, however far out, though
;; only the soonest `orgzly-reminders-max-count' go to the phone, whose
;; reminder allowance is shared by every app.
;;
;; Delivery is a `jetpacs-reminders-define-projection': the set is
;; re-derived after every surface push, replaced whole, and persisted
;; across reboots by the Companion.  Turning `orgzly-reminders-enabled'
;; off clears it.  Ids are opaque hashes, so no path reaches the phone.
;;
;; A tap runs `orgzly.home', queued or waking Emacs when offline, and
;; opens the app where the note is one tap away.  There are no Done or
;; Snooze buttons (see docs/PARITY.md).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs-surfaces)              ; jetpacs-granted-p
(require 'jetpacs-widgets)               ; jetpacs-opaque-id, jetpacs-action
(require 'jetpacs-reminders)
(require 'orgzly-data)
(require 'orgzly-agenda)

(defconst orgzly-reminders-owner "orgzly"
  "Owner of Orgzly's device reminder set.")

(defconst orgzly-reminders--surface "app:orgzly"
  "The app surface a reminder tap opens on the phone.")

(defconst orgzly-reminders--tap-ttl-seconds 86400
  "How long a reminder tap made offline stays queued.")

(defcustom orgzly-reminders-enabled t
  "Deliver Orgzly reminders to the phone.
Turning this off clears the reminders Orgzly placed there."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-max-count 64
  "Most reminders Orgzly places on the phone, the soonest first.
The phone's allowance (256 on the Jetpacs Companion) is shared by every
app, and a set that would exceed it is refused whole."
  :type '(integer 1 256) :group 'orgzly)

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

(defun orgzly-reminders--on-tap ()
  "The action a reminder tap runs: open Orgzly's notebooks.
Without it the phone gives the notification no tap at all.  Offline it
waits in the queue, or wakes Emacs when the phone grants that; the app
surface opens only where the phone grants rich reminder actions."
  (jetpacs-action "orgzly.home"
                  :when-offline (if (jetpacs-granted-p "offline.wake")
                                    'wake
                                  'queue)
                  :ttl-s orgzly-reminders--tap-ttl-seconds
                  :open-surface (and (jetpacs-granted-p "reminders.actions")
                                     orgzly-reminders--surface)))

(defun orgzly-reminders--reminder (spec on-tap)
  "SPEC from `orgzly-reminders-upcoming' as a SPEC 18.6 reminder plist.
ON-TAP is the action the notification's tap runs."
  (let ((title (alist-get 'title spec)))
    (list :id (jetpacs-opaque-id "orgzly-reminder-" (alist-get 'id spec))
          :at_ms (alist-get 'at_ms spec)
          :title (if (and (stringp title) (not (string-blank-p title)))
                     title
                   "Orgzly reminder")
          :body (alist-get 'body spec)
          :on_tap on-tap)))

(defun orgzly-reminders-projection (&optional now)
  "The reminders Orgzly wants on the phone at NOW, soonest first.
At most `orgzly-reminders-max-count', each id once.  Signals when the
notebooks cannot be read, so a failed scan never clears the phone."
  (let ((seen (make-hash-table :test #'equal))
        (on-tap (orgzly-reminders--on-tap))
        reminders)
    (dolist (spec (orgzly-reminders-upcoming now))
      (let ((reminder (orgzly-reminders--reminder spec on-tap)))
        (when (and (< (length reminders) orgzly-reminders-max-count)
                   (not (gethash (plist-get reminder :id) seen)))
          (puthash (plist-get reminder :id) t seen)
          (push reminder reminders))))
    (nreverse reminders)))

(defun orgzly-reminders-register ()
  "Keep Orgzly's reminders on the phone in step with the notebooks."
  (jetpacs-reminders-define-projection
   orgzly-reminders-owner #'orgzly-reminders-projection
   :enabled-p (lambda () orgzly-reminders-enabled)))

(defun orgzly-reminders-unregister ()
  "Stop syncing Orgzly's reminders; the phone keeps the set it holds."
  (jetpacs-reminders-remove-projection orgzly-reminders-owner))

(provide 'orgzly-reminders)
;;; orgzly-reminders.el ends here
