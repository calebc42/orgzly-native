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
(require 'orgzly-ui)
(require 'orgzly-reminders)
(require 'orgzly-agenda)

;; Every mutation from the settings screen may change what the views and
;; scans derive, so drop the memo (the standing cache contract).
(add-hook 'jetpacs-settings-after-set-hook
          (lambda (&rest _) (orgzly-data-invalidate)))

;; Registered under the app's owner id, so coexisting apps' settings
;; never interleave: these sections render only while Orgzly is current.
(with-jetpacs-owner "orgzly"
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
   "Display"
   '((orgzly-display-content . (:label "Note content in lists"))
     (orgzly-display-content-line-count . (:label "Content line count"))
     (orgzly-display-planning . (:label "Planning times"))
     (orgzly-display-book-name-in-search . (:label "Notebook name in results"))
     (orgzly-content-preview-lines . (:label "Content preview lines"))))

  (jetpacs-settings-register-section
   "Agenda"
   '((orgzly-agenda-hide-empty-days . (:label "Hide empty days"))
     (orgzly-agenda-group-scheduled-with-today
      . (:label "Group overdue scheduled with today"))))

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
     (org-priority-lowest . (:label "Lowest priority (char code)")))))

;; No settings view and no drawer entry of our own: the stock core
;; "settings" screen renders the sections above (scoped to this app),
;; and the stock drawer entry already targets it.  An app only defines
;; "<appid>.settings" when it needs controls the schema can't express.

(provide 'orgzly-settings)
;;; orgzly-settings.el ends here
