;;; orgzly-agenda.el --- Query-driven day agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's agenda: a saved search whose ad.N option expands matching
;; notes into day buckets over the next N days.  Scheduled and deadline
;; times generate occurrences (repeaters expanded); events (plain active
;; timestamps) land on their day; overdue scheduled/deadline items group
;; under today, qualified with how late they are.
;;
;; The expansion lives here so the search view and the reminders module
;; reuse the same occurrence math.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-query)

(declare-function orgzly-ui--note-row "orgzly-ui")
(declare-function orgzly-ui--icon-line "orgzly-ui")

(defconst orgzly-agenda--meta-color "#8a8a8a")

;; ─── Occurrence expansion ────────────────────────────────────────────────────

(defun orgzly-agenda--day-start (&optional offset now)
  "Float time of midnight OFFSET days from NOW's day."
  (let ((d (decode-time (or now (current-time)))))
    (float-time (encode-time 0 0 0 (+ (nth 3 d) (or offset 0))
                             (nth 4 d) (nth 5 d)))))

(defun orgzly-agenda--parse-repeater (repeater)
  "Parse REPEATER (\"+1w\", \"++2d\", \".+1m\") into (UNIT . VALUE), or nil."
  (when (and (stringp repeater)
             (string-match "\\`[.+]?\\+\\([0-9]+\\)\\([hdwmy]\\)\\'" repeater))
    (cons (pcase (match-string 2 repeater)
            ("h" 'hour) ("d" 'day) ("w" 'week) ("m" 'month) ("y" 'year))
          (string-to-number (match-string 1 repeater)))))

(defun orgzly-agenda--step (time unit value)
  "TIME advanced by VALUE UNITs, calendar-aware for months and years."
  (pcase unit
    ('hour (+ time (* value 3600)))
    ('day (+ time (* value 86400)))
    ('week (+ time (* value 7 86400)))
    (_ (let ((d (decode-time (seconds-to-time time))))
         (float-time
          (encode-time (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)
                       (+ (nth 4 d) (if (eq unit 'month) value 0))
                       (+ (nth 5 d) (if (eq unit 'year) value 0))))))))

(defun orgzly-agenda--occurrences (ts window-start window-end &optional no-overdue)
  "Occurrence float-times of TS within [WINDOW-START, WINDOW-END).
A repeating timestamp contributes every repetition inside the window;
a base time before WINDOW-START contributes one overdue occurrence
(the base itself) unless NO-OVERDUE."
  (when-let ((time (alist-get 'time ts)))
    (let ((rep (orgzly-agenda--parse-repeater (alist-get 'repeater ts))))
      (cond
       ((null rep)
        (cond ((and (< time window-start) (not no-overdue)) (list time))
              ((and (>= time window-start) (< time window-end)) (list time))))
       (t
        (let ((occ time) (out nil) (guard 0))
          ;; Catch up to the window, keeping one overdue base if applicable.
          (when (and (< occ window-start) (not no-overdue))
            (push occ out))
          (while (and (< occ window-start) (< guard 1000))
            (setq occ (orgzly-agenda--step occ (car rep) (cdr rep))
                  guard (1+ guard)))
          ;; Collect repetitions inside the window.
          (while (and (< occ window-end) (< guard 1000))
            (when (>= occ window-start) (push occ out))
            (setq occ (orgzly-agenda--step occ (car rep) (cdr rep))
                  guard (1+ guard)))
          (nreverse out)))))))

(defun orgzly-agenda-items (entries query ctx &optional now)
  "Expand QUERY's hits from ENTRIES into agenda items.
Returns a list of item alists sorted by (day, time):
  ((day . FLOAT midnight) (time . FLOAT) (kind . scheduled|deadline|event)
   (overdue-days . INT|nil) (entry . ENTRY))
Days span QUERY's ad.N (default 1) from today; overdue occurrences
collapse into today."
  (let* ((now (or now (current-time)))
         (days (or (plist-get query :agenda-days) 1))
         (window-start (orgzly-agenda--day-start 0 now))
         (window-end (orgzly-agenda--day-start days now))
         (hits (orgzly-query-select entries query ctx))
         (items nil))
    (dolist (entry hits)
      (let ((done (member (alist-get 'state entry) (alist-get 'done ctx))))
        (dolist (kind '(scheduled deadline))
          (when-let ((ts (alist-get kind entry)))
            (dolist (occ (orgzly-agenda--occurrences
                          ts window-start window-end done))
              (let* ((overdue (< occ window-start))
                     (day (if overdue window-start
                            (orgzly-agenda--day-start
                             0 (seconds-to-time occ)))))
                (push `((day . ,day) (time . ,occ) (kind . ,kind)
                        (overdue-days
                         . ,(when overdue
                              (max 1 (floor (/ (- window-start occ) 86400)))))
                        (entry . ,entry))
                      items)))))
        (dolist (ts (alist-get 'events entry))
          (dolist (occ (orgzly-agenda--occurrences ts window-start window-end t))
            (push `((day . ,(orgzly-agenda--day-start 0 (seconds-to-time occ)))
                    (time . ,occ) (kind . event) (overdue-days . nil)
                    (entry . ,entry))
                  items)))))
    (sort (nreverse items)
          (lambda (a b)
            (or (< (alist-get 'day a) (alist-get 'day b))
                (and (= (alist-get 'day a) (alist-get 'day b))
                     (< (alist-get 'time a) (alist-get 'time b))))))))

(defun orgzly-agenda-day-groups (entries query ctx &optional now)
  "Agenda items grouped per day: ((DAY . ITEMS) ...), days without items kept.
Overdue occurrences land in today's group (their `overdue-days' marks
them); `orgzly-agenda-sections' splits them out Orgzly-style."
  (let* ((now (or now (current-time)))
         (days (or (plist-get query :agenda-days) 1))
         (items (orgzly-agenda-items entries query ctx now)))
    (cl-loop for i from 0 below days
             for day = (orgzly-agenda--day-start i now)
             collect (cons day
                           (cl-remove-if-not
                            (lambda (it) (= (alist-get 'day it) day))
                            items)))))

(defcustom orgzly-agenda-group-scheduled-with-today nil
  "When non-nil, overdue scheduled notes group under Today.
Otherwise every overdue occurrence sits in the leading Overdue section,
as Orgzly does."
  :type 'boolean :group 'orgzly)

(defun orgzly-agenda-sections (entries query ctx &optional now)
  "Agenda sections: (`overdue' . ITEMS) first, then (DAY . ITEMS) per day.
The Overdue section collects occurrences whose base time is before
today — except overdue scheduled ones when
`orgzly-agenda-group-scheduled-with-today', which stay under Today."
  (let* ((now (or now (current-time)))
         (groups (orgzly-agenda-day-groups entries query ctx now))
         (overdue (cl-remove-if-not
                   (lambda (it)
                     (and (alist-get 'overdue-days it)
                          (not (and orgzly-agenda-group-scheduled-with-today
                                    (eq (alist-get 'kind it) 'scheduled)))))
                   (cdr (car groups)))))
    (when overdue
      (setcdr (car groups)
              (cl-remove-if (lambda (it) (memq it overdue)) (cdr (car groups)))))
    (if overdue (cons (cons 'overdue overdue) groups) groups)))

;; ─── Rendering ───────────────────────────────────────────────────────────────

(defun orgzly-agenda--day-header (day now)
  (let* ((today (orgzly-agenda--day-start 0 now))
         (label (format-time-string "%A, %b %-d" (seconds-to-time day)))
         (qualifier (cond ((= day today) "Today")
                          ((= day (orgzly-agenda--day-start 1 now)) "Tomorrow"))))
    (jetpacs-section-header
     (if qualifier (format "%s · %s" qualifier label) label))))

(defun orgzly-agenda--item-row (item)
  "One agenda item as the standard Orgzly note row.
Only the planning time responsible for the item's presence shows, and
an overdue item carries how late it is in red."
  (let* ((entry (alist-get 'entry item))
         (kind (alist-get 'kind item))
         (overdue (alist-get 'overdue-days item))
         (row (orgzly-ui--note-row entry :show-book t :only-kind kind
                                   :hide-content t)))
    (if (null overdue)
        row
      (jetpacs-column
       row
       (jetpacs-row
        (jetpacs-spacer :width 4)
        (orgzly-ui--icon-line "history"
                              (format "%d day%s overdue" overdue
                                      (if (= overdue 1) "" "s"))
                              "#e53935"))))))

(defun orgzly-agenda--empty-day-node ()
  (jetpacs-rich-text
   (list (jetpacs-span "No notes" :color orgzly-agenda--meta-color))
   :style 'caption :padding 8))

(defun orgzly-agenda-day-nodes (entries query ctx &optional now hide-empty)
  "Widget nodes: Overdue then day sections, shared with the search view."
  (let ((now (or now (current-time))))
    (cl-loop for (day . items) in (orgzly-agenda-sections entries query ctx now)
             when (or items (and (not (eq day 'overdue)) (not hide-empty)))
             append (cons (if (eq day 'overdue)
                              (jetpacs-section-header "Overdue")
                            (orgzly-agenda--day-header day now))
                          (or (mapcar #'orgzly-agenda--item-row items)
                              (list (orgzly-agenda--empty-day-node)))))))

;; ─── The agenda tab ──────────────────────────────────────────────────────────

(defcustom orgzly-agenda-hide-empty-days nil
  "When non-nil, days with no notes are omitted (Orgzly: hide empty days)."
  :type 'boolean :group 'orgzly)

(defvar orgzly-agenda--current "Agenda"
  "Name of the saved search the agenda tab is showing.")

(declare-function orgzly-search-saved-searches "orgzly-search")

(defun orgzly-agenda--searches ()
  "The agenda-shaped saved searches (those carrying ad.N)."
  (cl-remove-if-not
   (lambda (ss) (plist-get (orgzly-query-parse (cdr ss)) :agenda-days))
   (orgzly-search-saved-searches)))

(defun orgzly-agenda--body ()
  (let* ((searches (orgzly-agenda--searches))
         (current (or (assoc orgzly-agenda--current searches)
                      (car searches)))
         (query-str (or (cdr current) ".it.done ad.7"))
         (query (orgzly-query-parse query-str))
         (ctx (orgzly-data-query-context)))
    (apply #'jetpacs-lazy-column
           (append
            (when (> (length searches) 1)
              (list (apply #'jetpacs-scroll-row
                           (mapcar (lambda (ss)
                                     (jetpacs-chip (car ss)
                                                :selected (equal (car ss)
                                                                 (car current))
                                                :on-tap (jetpacs-action
                                                         "orgzly.agenda.switch"
                                                         :args `((name . ,(car ss)))
                                                         :when-offline "drop")))
                                   searches))))
            (or (orgzly-agenda-day-nodes (orgzly-data-entries) query ctx nil
                                         orgzly-agenda-hide-empty-days)
                (list (jetpacs-empty-state :icon "event_available"
                                        :title "Nothing scheduled")))))))

(jetpacs-shell-define-view "agenda"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "agenda" (orgzly-agenda--body)
                                  :snackbar snackbar))
  :tab '(:icon "event" :label "Agenda")
  :order 11)

(jetpacs-defaction "orgzly.agenda.switch"
  (lambda (args _)
    (setq orgzly-agenda--current (alist-get 'name args))
    (jetpacs-shell-push)))

(provide 'orgzly-agenda)
;;; orgzly-agenda.el ends here
