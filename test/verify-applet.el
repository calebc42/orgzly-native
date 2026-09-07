;;; -*- lexical-binding: t; -*-
(setq load-prefer-newer t)
(require 'jetpacs-applet-tooling)
(setq jetpacs-applet-tooling-repositories-root
      (file-name-directory (directory-file-name (getenv "JETPACS_ROOT"))))
(let* ((root (getenv "ORGZLY_SOURCE_ROOT"))
       (platform (expand-file-name "emacs" (getenv "JETPACS_ROOT")))
       (workspace (expand-file-name "static-sources" (getenv "ORGZLY_VERIFY_OUTPUT")))
       (applet (expand-file-name "emacs/apps/orgzly" workspace))
       (providers (expand-file-name "platform" workspace)))
  ;; Static reconciliation reads byte-identical copies of the actual platform
  ;; providers, never an applet relabeled as a platform or an expanded allowlist.
  (make-directory applet t) (make-directory providers t)
  (dolist (pair (list (cons (expand-file-name "emacs/apps/orgzly" root) applet)
                      (cons platform providers)))
    (dolist (file (directory-files (car pair) t (rx ".el" eos)))
      (copy-file file (expand-file-name (file-name-nondirectory file) (cdr pair)) t)))
  (setq jetpacs-applet-tooling-workspace-root workspace
        jetpacs-applet-tooling-platform-source-directory "platform")
  (dolist (operation '(jetpacs-applet-tooling-inspect-applet
                       jetpacs-applet-tooling-applet-manifest
                       jetpacs-applet-tooling-validate-applet))
    (let ((report (funcall operation "emacs/apps/orgzly")))
      (with-temp-file (concat (getenv "ORGZLY_VERIFY_OUTPUT") "/" (symbol-name operation) ".txt")
        (insert report))))
  (setq jetpacs-applet-tooling-workspace-root root
        jetpacs-applet-tooling-platform-source-directory nil))
;; Explicit trusted test execution follows static inspection, using temp Org files.
(require 'orgzly-current-test)
(require 'jetpacs-devtools)
(orgzly-test-with-snapshot
 (with-temp-file (expand-file-name "render-determinism.txt" (getenv "ORGZLY_VERIFY_OUTPUT"))
   (insert (jetpacs-applet-tooling-check-render-determinism "orgzly")))
 (let ((output (or (getenv "ORGZLY_FIXTURE_OUTPUT") "/tmp/orgzly-screen-fixtures")))
   (make-directory output t)
   (dolist (dark '(nil t))
     (let ((orgzly-state-dark dark))
       (dolist (pair (list (cons "notebooks" '(:kind books)) (cons "outline" '(:kind book :book "gtd"))
                          (cons "note" (list :kind 'note :id (alist-get 'id (car (orgzly-state-entries "gtd")))))))
         (with-temp-file (expand-file-name (concat (car pair) (if dark "-dark" "-light") ".json") output)
           (set-buffer-multibyte nil)
           (insert (jetpacs-node->canonical-json
                    (orgzly-ui-screen (cdr pair) (jetpacs-view-switch "books"))))))))))
