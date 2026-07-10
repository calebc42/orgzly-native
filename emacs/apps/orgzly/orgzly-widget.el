;;; orgzly-widget.el --- Home-screen list widget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's saved-search list widget on the jetpacs `widget:agenda'
;; surface: one widget view per saved search, switched companion-side
;; from the widget header (offline-capable, served from cache).  Rows
;; carry the todo-cycle checkmark button, exactly like Orgzly's widget.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-agenda)
(require 'orgzly-search)

(defconst orgzly-widget--cap 20
  "Rows per widget view; a sanity bound on spec size, the list scrolls.")

(defun orgzly-widget--ts-meta (entry)
  "The row's metadata line: earliest planning time, then the book."
  (let* ((bits (delq nil
                     (list
                      (when-let ((s (alist-get 'scheduled entry)))
                        (format-time-string
                         (if (alist-get 'has-time s) "%b %-d %H:%M" "%b %-d")
                         (seconds-to-time (alist-get 'time s))))
                      (alist-get 'book entry)))))
    (string-join bits " · ")))

(defun orgzly-widget--icon (entry)
  (cond ((alist-get 'deadline entry) "deadline")
        ((alist-get 'scheduled entry) "scheduled")
        ((alist-get 'events entry) "event")
        (t "folder")))

(defun orgzly-widget--row (entry)
  (let ((ref (orgzly-data-entry-ref entry))
        (todo (alist-get 'state entry))
        (done (orgzly-ui--done-p entry)))
    (jetpacs-widget-item
     (alist-get 'title entry)
     :todo todo :done done
     :meta (orgzly-widget--ts-meta entry)
     :icon (orgzly-widget--icon entry)
     :on-tap (jetpacs-action "orgzly.note.open" :args ref) :in-app t
     :button (and todo (if done "todo_done" "todo_open"))
     :on-button (and todo (jetpacs-action "orgzly.note.toggle-done" :args ref)))))

(defun orgzly-widget--agenda-rows (query ctx)
  "Day-grouped widget rows for an ad.N QUERY, with divider rows."
  (let ((now (current-time))
        (rows nil) (count 0))
    (cl-loop
     for (day . items) in (orgzly-agenda-day-groups
                           (orgzly-data-entries) query ctx now)
     while (< count orgzly-widget--cap)
     when items
     do (let* ((today (orgzly-agenda--day-start 0 now))
               (label (cond ((= day today) "Today")
                            ((= day (orgzly-agenda--day-start 1 now)) "Tomorrow")
                            (t (format-time-string "%a, %b %-d"
                                                   (seconds-to-time day))))))
          (push (jetpacs-widget-divider label) rows)
          (dolist (it items)
            (when (< count orgzly-widget--cap)
              (push (orgzly-widget--row (alist-get 'entry it)) rows)
              (cl-incf count)))))
    (nreverse rows)))

(defun orgzly-widget--search-rows (query ctx)
  (mapcar #'orgzly-widget--row
          (seq-take (orgzly-query-select (orgzly-data-entries) query ctx)
                    orgzly-widget--cap)))

(defun orgzly-widget--views ()
  "One widget view per saved search, day-grouped when the query is ad.N."
  (let ((ctx (orgzly-data-query-context)))
    (mapcar
     (lambda (ss)
       (let ((query (orgzly-query-parse (cdr ss))))
         (cons (intern (car ss))
               `((title . ,(car ss))
                 (items . ,(vconcat
                            (condition-case nil
                                (if (plist-get query :agenda-days)
                                    (orgzly-widget--agenda-rows query ctx)
                                  (orgzly-widget--search-rows query ctx))
                              (error nil))))))))
     (orgzly-search-saved-searches))))

(defvar orgzly-widget--last 'unset
  "Previous widget spec, to suppress identical pushes.")

(defun orgzly-widget-push ()
  "Push the `widget:agenda' surface (memo-guarded)."
  (let ((views (condition-case nil (orgzly-widget--views) (error nil))))
    (when views
      (unless (equal views orgzly-widget--last)
        (setq orgzly-widget--last views)
        (jetpacs-surface-push
         "widget:agenda"
         `((views . ,views)
           (initial_view . ,(symbol-name (car (car views))))))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly-widget-push)
(add-hook 'jetpacs-shell-refresh-hook
          (lambda () (setq orgzly-widget--last 'unset)))

(provide 'orgzly-widget)
;;; orgzly-widget.el ends here
