;;; build-bundle.el --- Regenerate the orgzly-native single-file bundle -*- lexical-binding: t; -*-

;; Concatenate the orgzly app sources into one loadable bundle at the repo
;; root. Run after editing any source file:
;;
;;   emacs --batch -l emacs/build-bundle.el
;;
;; Output:
;;   orgzly.el  — Orgzly Revived as a Jetpacs Tier-1 app (emacs/apps/orgzly/):
;;                the dotted query language, the file-backed data layer, the
;;                books/notes/editor views, agenda, search, reminders, the
;;                home-screen widget, capture, and settings.
;;
;; This does NOT inline the Jetpacs core. The bundle opens with
;; `(require 'jetpacs-core)`, so the jetpacs foundation bundle
;; (jetpacs-core.el, from the separate jetpacs repo / the `jetpacs'
;; submodule) must be on `load-path' first. That is the whole point of the
;; two-bundle model: one installed jetpacs-core.el serves every app, and
;; orgzly-native is pure elisp that `(require 's the core, never a copy of it.
;;
;; The files are emitted in dependency order. Because every source ends with a
;; `(provide 'FEATURE)', the inter-file `(require ...)' forms for app features
;; become no-ops once the providing chunk has loaded earlier in the bundle;
;; the core `(require 'jetpacs-...)' forms resolve against the already-loaded
;; jetpacs-core. External requires (org, cl-lib, ...) resolve normally.

;;; Code:

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       ;; Dependency order. Do not reorder without re-checking the require
       ;; graph.
       (app-files '("apps/orgzly/orgzly-query.el"
                    "apps/orgzly/orgzly-data.el"
                    "apps/orgzly/orgzly-ui.el"
                    "apps/orgzly/orgzly-agenda.el"
                    "apps/orgzly/orgzly-search.el"
                    "apps/orgzly/orgzly-reminders.el"
                    "apps/orgzly/orgzly-widget.el"
                    "apps/orgzly/orgzly-capture.el"
                    "apps/orgzly/orgzly-settings.el"
                    "apps/orgzly/orgzly.el"))
       (out (expand-file-name "../orgzly.el" here)))
  (with-temp-file out
    (insert ";;; orgzly.el --- Orgzly Revived in elisp (Jetpacs Tier-1 app), single-file bundle -*- lexical-binding: t; -*-\n"
            ";;\n"
            ";; GENERATED FILE -- do not edit by hand.\n"
            ";; Produced by emacs/build-bundle.el from the emacs/apps/orgzly/ sources.\n"
            ";; Concatenated in dependency order; each part keeps its own `provide',\n"
            ";; so the inter-file `require' forms resolve within this file.\n"
            ";;\n"
            ";; Requires the Jetpacs core (jetpacs-core.el) on `load-path' first.\n"
            ";;\n"
            ";;; Code:\n\n"
            "(require 'jetpacs-core)\n\n")
    (dolist (f app-files)
      (insert ";;; ==================================================================\n"
              (format ";;; BEGIN %s\n" f)
              ";;; ==================================================================\n\n")
      (insert-file-contents (expand-file-name f here))
      (goto-char (point-max))
      (insert "\n"))
    (insert "(provide 'orgzly)\n"
            ";;; orgzly.el ends here\n"))
  (message "Wrote %s" out))

;;; build-bundle.el ends here
