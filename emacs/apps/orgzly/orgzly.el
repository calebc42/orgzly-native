;;; orgzly.el --- Orgzly Revived as a pure-elisp Jetpacs Tier 1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1"))

;; Orgzly Revived, rebuilt on the Jetpacs foundation with zero Kotlin:
;; the companion is the stock app-agnostic renderer; every screen,
;; query, reminder, and mutation below is elisp, and org files on disk
;; are the database.
;;
;;   (add-to-list 'load-path "…/jetpacs/emacs/core")  ; or jetpacs-core.el
;;   (add-to-list 'load-path "…/emacs/apps/orgzly")
;;   (require 'orgzly)
;;
;; Feature coverage against orgzly-android-revived is tracked in
;; docs/PARITY.md.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-apps)

(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-ui)
(require 'orgzly-agenda)
(require 'orgzly-search)
(require 'orgzly-reminders)
(require 'orgzly-widget)
(require 'orgzly-capture)
(require 'orgzly-settings)

(setq jetpacs-shell-drawer-header "Orgzly")

(jetpacs-defapp "orgzly"
  :label "Orgzly" :icon "book"
  :views '("books" "agenda" "search"
           "book" "note" "preface" "searches" "settings")
  :order 10)

(provide 'orgzly)
;;; orgzly.el ends here
