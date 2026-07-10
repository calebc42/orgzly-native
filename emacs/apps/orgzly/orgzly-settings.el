;;; orgzly-settings.el --- Settings screen & registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's preference screens, mapped onto the jetpacs schema-driven
;; settings machinery: each section below mirrors one of Orgzly's pref
;; screens; the registry doubles as the wire allowlist (only listed
;; symbols are settable from the phone), values validate against their
;; `custom-type', and persistence goes through Customize.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-settings)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-reminders)
(require 'orgzly-agenda)

;; Every mutation from the settings screen may change what the views and
;; scans derive, so drop the memo (the standing cache contract).
(add-hook 'jetpacs-settings-after-set-hook
          (lambda (&rest _) (orgzly-data-invalidate)))

(jetpacs-settings-register-section
 "Notebooks"
 '((orgzly-directory . (:label "Notebooks directory"))
   (orgzly-default-book . (:label "Default notebook"))))

(jetpacs-settings-register-section
 "New note"
 '((orgzly-new-note-state . (:label "Initial state"))
   (orgzly-new-note-prepend . (:label "Add to top of notebook"))
   (orgzly-new-note-created-property . (:label "Stamp created time"))
   (orgzly-created-property . (:label "Created property name"))))

(jetpacs-settings-register-section
 "Agenda"
 '((orgzly-agenda-hide-empty-days . (:label "Hide empty days"))))

(jetpacs-settings-register-section
 "Reminders"
 '((orgzly-reminders-scheduled . (:label "For scheduled times"))
   (orgzly-reminders-deadline . (:label "For deadlines"))
   (orgzly-reminders-event . (:label "For events"))
   (orgzly-reminders-daily-time . (:label "Daily time for date-only notes"))
   (orgzly-reminders-horizon-hours . (:label "Arm window (hours)"))))

(jetpacs-settings-register-section
 "Org file format"
 '((org-archive-location . (:label "Archive location"))
   (org-log-done . (:label "Log state changes"))
   (org-priority-default . (:label "Default priority (char code)"))
   (org-priority-highest . (:label "Highest priority (char code)"))
   (org-priority-lowest . (:label "Lowest priority (char code)"))))

;; ─── The settings view (reached from the drawer) ─────────────────────────────

(defun orgzly-settings--body ()
  (apply #'jetpacs-lazy-column (jetpacs-settings-sections)))

(jetpacs-shell-define-view "settings"
  :builder (lambda (snackbar)
             (jetpacs-shell-nav-view "Settings" (orgzly-settings--body)
                                  :snackbar snackbar))
  :order 90)

(jetpacs-shell-add-drawer-item
 60 (lambda ()
      (jetpacs-drawer-item "settings" "Settings"
                        (jetpacs-shell-switch-view "settings"))))

(provide 'orgzly-settings)
;;; orgzly-settings.el ends here
