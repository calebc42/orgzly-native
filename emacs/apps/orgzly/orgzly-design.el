;;; orgzly-design.el --- Orgzly Foundation style profiles -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; App-owned palettes and density; all rendering uses reusable Foundation nodes.
;;; Code:
(require 'jetpacs-components)
(require 'jetpacs-design-profiles)

(defconst orgzly-design-light
  '(("primary" . "#546E7A") ("on_primary" . "#FFFFFF")
    ("secondary" . "#607D8B") ("on_secondary" . "#FFFFFF")
    ("background" . "#FAFAFA") ("on_background" . "#212121")
    ("surface" . "#FFFFFF") ("on_surface" . "#212121")
    ("outline" . "#E0E0E0") ("error" . "#B71C1C")
    ("on_error" . "#FFFFFF") ("success" . "#1B5E20") ("warning" . "#8D5900"))
  "Orgzly light palette, derived from the upstream Android resources.")
(defconst orgzly-design-dark
  '(("primary" . "#B0BEC5") ("on_primary" . "#212121")
    ("secondary" . "#90A4AE") ("on_secondary" . "#212121")
    ("background" . "#212121") ("on_background" . "#FFFFFF")
    ("surface" . "#242424") ("on_surface" . "#FFFFFF")
    ("outline" . "#424242") ("error" . "#EF9A9A")
    ("on_error" . "#212121") ("success" . "#A5D6A7") ("warning" . "#FFCC80"))
  "Orgzly dark palette with upstream light TODO and DONE colors.")

(defun orgzly-design-profile (dark)
  "Build Orgzly's immutable Foundation profile, using DARK when non-nil."
  (let* ((palette (if dark orgzly-design-dark orgzly-design-light))
         (roles (mapcar (lambda (p) (cons (car p) (jetpacs-design-color (cdr p)))) palette)))
    (jetpacs-design-make-profile
     (if dark "orgzly.dark" "orgzly.light") (if dark "Orgzly Dark" "Orgzly Light")
     :tokens roles :theme-roles roles
     :typography
     '(("body" . (:family "system" :size 16 :weight 400 :line-height 22 :letter-spacing 0))
       ("title" . (:family "system" :size 20 :weight 500 :line-height 28 :letter-spacing 0))
       ("caption" . (:family "system" :size 13 :weight 400 :line-height 18 :letter-spacing 0))
       ("label" . (:family "system" :size 14 :weight 500 :line-height 20 :letter-spacing 0)))
     :styles
     `(("orgzly.screen" . ,(jetpacs-design-style
                            `(("background_color" . ,(jetpacs-design-token "surface"))
                              ("content_color" . ,(jetpacs-design-token "on_surface"))
                              ("fill_width" . ,(jetpacs-design-boolean t)))))
       ("orgzly.toolbar" . ,(jetpacs-design-style
                             `(("background_color" . ,(jetpacs-design-color (if dark "#292929" "#546E7A")))
                               ("content_color" . ,(jetpacs-design-color "#FFFFFF"))
                               ("padding_horizontal" . ,(jetpacs-design-dimension 8))
                               ("min_height" . ,(jetpacs-design-dimension 56))
                               ("fill_width" . ,(jetpacs-design-boolean t)))))
       ("orgzly.row" . ,(jetpacs-design-style
                         `(("corner_radius" . ,(jetpacs-design-dimension 0))
                           ("padding_horizontal" . ,(jetpacs-design-dimension 12))
                           ("padding_vertical" . ,(jetpacs-design-dimension 8))
                           ("min_height" . ,(jetpacs-design-dimension 48))
                           ("fill_width" . ,(jetpacs-design-boolean t)))
                         :rules (list (jetpacs-design-rule "pressed"
                                `(("background_color" . ,(jetpacs-design-token "outline")))))))
       ("orgzly.done" . ,(jetpacs-design-style `(("alpha" . ,(jetpacs-design-number 0.55)))))
       ("orgzly.form" . ,(jetpacs-design-style
                          `(("padding" . ,(jetpacs-design-dimension 16))
                            ("fill_width" . ,(jetpacs-design-boolean t))))))
     :component-styles
     '(("text.body" . ("typography.body")) ("text.title" . ("typography.title"))
       ("text.caption" . ("typography.caption")) ("text.label" . ("typography.label"))
       ("list-item.container" . ("orgzly.row"))
       ("text-field.label" . ("typography.caption"))
       ("text-field.text" . ("typography.body"))
       ("section-header.title" . ("typography.label"))
       ("list-item.title" . ("typography.body"))
       ("list-item.subtitle" . ("typography.caption"))))))

(defun orgzly-design-register ()
  "Register both Orgzly style presets idempotently."
  (dolist (dark '(nil t)) (jetpacs-design-register-profile (orgzly-design-profile dark))))

(defun orgzly-design-wrap (screen dark)
  "Wrap SCREEN in the selected profile, defaulting to DARK's Orgzly preset."
  (let* ((active (jetpacs-design-active-profile))
         (profile (if (assoc "orgzly.row" (plist-get active :styles)) active
                    (jetpacs-design-profile-get (if dark "orgzly.dark" "orgzly.light")))))
    (jetpacs-design-profile-scope profile (list screen))))

(provide 'orgzly-design)
;;; orgzly-design.el ends here
