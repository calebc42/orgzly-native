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

;; ─── The drawer: searches and notebooks, like Orgzly's navigation ────────────
;;
;; Orgzly's drawer lists every saved search and every notebook for
;; one-tap jumps.  Drawer builders run per push (so selection highlights
;; stay current), but the *set* of items only changes when a search or
;; book is added, renamed, or removed — the sync below runs after each
;; push and is memo-guarded on the name lists, so a stable set costs
;; nothing and never re-pushes.

(defvar orgzly--drawer-memo nil
  "The (SEARCH-NAMES BOOK-NAMES) the drawer was last built for.")

(defun orgzly--drawer-sync ()
  "Mirror saved searches and notebooks into the drawer."
  (let ((key (list (mapcar #'car (orgzly-search-saved-searches))
                   (mapcar #'orgzly-data-book-name
                           (orgzly-data-book-files)))))
    (unless (equal key orgzly--drawer-memo)
      (setq orgzly--drawer-memo key)
      ;; Orders 30–49 are the drawer band owned by this sync.
      (setq jetpacs-shell-drawer-items
            (cl-remove-if (lambda (e) (and (>= (car e) 30) (< (car e) 50)))
                          jetpacs-shell-drawer-items))
      ;; Registered under the app's owner id — this sync runs from a
      ;; push hook, outside any load-time owner scope, and the entries
      ;; must ride only Orgzly's drawer once a second app exists.
      (with-jetpacs-owner "orgzly"
       (let ((order 30.0))
        (jetpacs-shell-add-drawer-item
         order (lambda ()
                 (jetpacs-drawer-item "manage_search" "Searches"
                                   (jetpacs-action "orgzly.search.manage"
                                                :when-offline "drop"))))
        (dolist (name (nth 0 key))
          (setq order (+ order 0.01))
          (jetpacs-shell-add-drawer-item
           order
           (lambda ()
             (jetpacs-drawer-item
              "search" name
              (jetpacs-action "orgzly.search.saved"
                           :args `((name . ,name)) :when-offline "drop")
              :selected (equal (cdr (assoc name (orgzly-search-saved-searches)))
                               orgzly-search--query)))))
        (setq order 40.0)
        (jetpacs-shell-add-drawer-item
         order (lambda ()
                 (jetpacs-drawer-item "library_books" "Notebooks"
                                   (jetpacs-shell-switch-view "orgzly.books"))))
        (dolist (name (nth 1 key))
          (setq order (+ order 0.01))
          (jetpacs-shell-add-drawer-item
           order
           (lambda ()
             (jetpacs-drawer-item
              "description" name
              (jetpacs-action "orgzly.book.open"
                           :args `((book . ,name)) :when-offline "drop")
              :selected (equal name orgzly-ui--current-book))))))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly--drawer-sync)
(orgzly--drawer-sync)

;; Every view name carries the "orgzly." namespace so a coexisting app
;; (glasspane's "glasspane.agenda", say) can never replace one of these
;; in the registry.  Settings is absent by design: Orgzly's sections
;; ride the stock core settings screen.
(jetpacs-defapp "orgzly"
  :label "Orgzly" :icon "book"
  :views '("orgzly.books" "orgzly.agenda" "orgzly.search" "orgzly.book"
           "orgzly.note" "orgzly.preface" "orgzly.searches")
  :order 10)

(provide 'orgzly)
;;; orgzly.el ends here
