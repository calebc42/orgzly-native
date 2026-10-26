;;; orgzly-design.el --- Orgzly Foundation style profiles -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; App-owned palettes and metrics; all rendering uses reusable Foundation nodes.
;; The look is Orgzly Revived 1.23: Material 3 dynamic colors, elevated
;; notebook cards, bulleted outline rows with indent guides, and a FAB.
;; Metrics are the upstream layout dimensions (res/values/dimens.xml and the
;; theme's font sizes).  Text styles reproduce TextView line metrics, so
;; wrapped and single lines land where the upstream TextViews put them.
;;; Code:
(require 'cl-lib)
(require 'jetpacs-components)
(require 'jetpacs-design-profiles)
(require 'jetpacs-surfaces)              ; jetpacs-colors (SPEC 20.1.3)

(defconst orgzly-design-dark
  '(("primary" . "#B4C5FF") ("on_primary" . "#1A2D60")
    ("secondary" . "#C1C5DD") ("on_secondary" . "#2B3042")
    ("background" . "#121318") ("on_background" . "#E3E2E9")
    ("surface" . "#121318") ("on_surface" . "#C9CAD4")
    ("outline" . "#2F3034") ("error" . "#EF9A9A")
    ("on_error" . "#212121") ("success" . "#A5D6A7") ("warning" . "#FFCC80"))
  "Orgzly dark roles: Revived's dynamic dark scheme as drawn on the device.")

(defconst orgzly-design-light
  '(("primary" . "#396286") ("on_primary" . "#FFFFFF")
    ("secondary" . "#585E71") ("on_secondary" . "#FFFFFF")
    ("background" . "#F8F9FE") ("on_background" . "#2D333A")
    ("surface" . "#F8F9FE") ("on_surface" . "#5A6067")
    ("outline" . "#DADBDF") ("error" . "#B71C1C")
    ("on_error" . "#FFFFFF") ("success" . "#1B5E20") ("warning" . "#8D5900"))
  "Orgzly light roles: Revived's dynamic light scheme as drawn on the Pixel Tablet.")

(defconst orgzly-design-dark-colors
  '((container . "#1A1B20") (strong . "#FCFAFF") (text . "#C9CAD4")
    (guide . "#2F3034") (bullet . "#000000") (fab . "#7A90C8") (on-fab . "#000000")
    (pill . "#8A90A5") (on-pill . "#000000") (dim . "#64656C")
    (todo . "#EF9A9A") (done . "#A5D6A7") (link . "#B7CAFF") (hint . "#8F9096")
    (dialog . "#282A2F") (header . "#C4CAE1") (menu . "#1E1F25") (dialog-hint . "#98999F")
    (track-on . "#484A55") (track-off . "#5A5A5E") (thumb-on . "#C0C6DD") (thumb-off . "#B9B9B9")
    (pill-off . "#414247") (on-pill-off . "#89898E")
    (surface . "#0C0E12") (divider . "#8F9096"))
  "Revived dark colors with no EBP role: containers, accents and states.")

(defconst orgzly-design-light-colors
  '((container . "#F1F4FA") (strong . "#2D333A") (text . "#5A6067")
    (guide . "#DADBDF") (bullet . "#455362") (fab . "#A4CDF6") (on-fab . "#174567")
    (pill . "#D5E4F7") (on-pill . "#455362") (dim . "#B1B5BA")
    (todo . "#B71C1C") (done . "#1B5E20") (link . "#396286") (hint . "#ABADB3")
    (dialog . "#E4E8F0") (header . "#396286") (menu . "#EBEEF5") (dialog-hint . "#76777F")
    (track-on . "#B4C5FF") (track-off . "#C6C6D0") (thumb-on . "#396286") (thumb-off . "#ECECEC")
    (pill-off . "#CDD2DA") (on-pill-off . "#90959D")
    (surface . "#F8F9FE") (divider . "#70767D"))
  "Revived light colors with no EBP role.")

;;; The device's scheme (SPEC 20.1.3, amendment #205)
;;
;; Revived draws with the device's Material You scheme, so every color
;; above is one of its roles — or a fixed resource, or a role composited
;; at some alpha over the surface behind it — as measured on two devices.
;; When the Companion reports the device's scheme, the palettes derive
;; from it the way Revived's theme does, and a new wallpaper recolors
;; Orgzly Native as it recolors Revived; without a report (an old
;; Companion, a snapshot before the welcome) the measured values stand.

(defun orgzly-design--hex->rgb (hex)
  "The (R G B) of #RRGGBB HEX, 0-255 each."
  (list (string-to-number (substring hex 1 3) 16)
        (string-to-number (substring hex 3 5) 16)
        (string-to-number (substring hex 5 7) 16)))

(defun orgzly-design-blend (fg alpha bg)
  "FG at ALPHA (0.0-1.0) composited over BG, both #RRGGBB, as #RRGGBB.
How Android draws a text or divider color with alpha over its parent."
  (apply #'format "#%02X%02X%02X"
         (cl-mapcar (lambda (f b) (round (+ (* alpha f) (* (- 1 alpha) b))))
                    (orgzly-design--hex->rgb fg) (orgzly-design--hex->rgb bg))))

(defun orgzly-design--device-colors (dark scheme)
  "Orgzly's app colors for DARK derived from the device's SCHEME plist.
SCHEME maps role keywords to #rrggbb (`jetpacs-colors'); each entry is
the Material role Revived's theme draws the element with, a fixed
resource, or a role at the alpha Android composites it at."
  (let* ((fixed (if dark orgzly-design-dark-colors orgzly-design-light-colors))
         ;; A role the device reports, else the measured color FALLBACK:
         ;; a Companion without Material's vocabulary reports the
         ;; thirteen standard roles alone.
         (role (lambda (name fallback)
                 (let ((value (jetpacs-plist-get-named scheme name)))
                   (if value (upcase value) (alist-get fallback fixed)))))
         (surface (upcase (jetpacs-plist-get-named scheme "surface")))
         (on-surface (upcase (jetpacs-plist-get-named scheme "on_surface")))
         (text (funcall role "on_surface_variant" 'text))
         (dialog (funcall role "surface_container_high" 'dialog))
         ;; The framework accent of Settings' switches and category
         ;; titles: primary in light, measured as secondary in dark.
         (accent (upcase (jetpacs-plist-get-named scheme (if dark "secondary" "primary"))))
         (pill-off (orgzly-design-blend on-surface 0.12 dialog)))
    `((container . ,(funcall role "surface_container_low" 'container))
      ;; textColorPrimary: on_surface (the Pixel Tablet draws exactly
      ;; that in both polarities; the measured phone palette's brighter
      ;; dark value is Samsung's own text color, kept as the fallback).
      (strong . ,on-surface)
      (text . ,text)
      ;; listDivider: 12% of the foreground over the surface.
      (guide . ,(orgzly-design-blend (if dark "#FFFFFF" "#000000") 0.12 surface))
      (bullet . ,(funcall role "on_secondary_container" 'bullet))
      (fab . ,(funcall role "primary_container" 'fab))
      (on-fab . ,(funcall role "on_primary_container" 'on-fab))
      (pill . ,(funcall role "secondary_container" 'pill))
      (on-pill . ,(funcall role "on_secondary_container" 'on-pill))
      ;; Done rows: the text at 45%.
      (dim . ,(orgzly-design-blend text 0.45 surface))
      (todo . ,(alist-get 'todo fixed))
      (done . ,(alist-get 'done fixed))
      (link . ,(upcase (jetpacs-plist-get-named scheme "primary")))
      ;; textColorHint: on_surface at 60% in dark, 38% in light.
      (hint . ,(orgzly-design-blend on-surface (if dark 0.6 0.38) surface))
      (dialog . ,dialog)
      (header . ,accent)
      (menu . ,(funcall role "surface_container" 'menu))
      (dialog-hint . ,(orgzly-design-blend on-surface 0.6 dialog))
      ;; The framework switch: the accent at 30% for the track on, the
      ;; foreground at 30% off; the thumbs are AppCompat's fixed greys.
      (track-on . ,(orgzly-design-blend accent 0.3 surface))
      (track-off . ,(if dark (orgzly-design-blend "#FFFFFF" 0.3 surface)
                      (alist-get 'track-off fixed)))
      (thumb-on . ,accent)
      (thumb-off . ,(alist-get 'thumb-off fixed))
      ;; A disabled tonal button: on_surface at 12%, its label at 38%.
      (pill-off . ,pill-off)
      (on-pill-off . ,(orgzly-design-blend on-surface 0.38 pill-off))
      ;; The period pickers' AlertDialog window: the surface in light,
      ;; the lowest container in dark.
      (surface . ,(if dark (funcall role "surface_container_lowest" 'surface) surface))
      (divider . ,(upcase (jetpacs-plist-get-named scheme "outline"))))))

(defun orgzly-design--device-roles (dark scheme colors)
  "Orgzly's EBP theme roles for DARK from the device's SCHEME and app COLORS.
The roles the profile's tokens and chrome carry: Revived's window is the
surface, its body text on_surface_variant, its outline the divider
composite, and its error and success the fixed TODO and DONE resources."
  (let ((role (lambda (name) (upcase (jetpacs-plist-get-named scheme name))))
        (fixed (if dark orgzly-design-dark orgzly-design-light)))
    `(("primary" . ,(funcall role "primary"))
      ("on_primary" . ,(funcall role "on_primary"))
      ("secondary" . ,(funcall role "secondary"))
      ("on_secondary" . ,(funcall role "on_secondary"))
      ("background" . ,(funcall role "surface"))
      ("on_background" . ,(funcall role "on_surface"))
      ("surface" . ,(funcall role "surface"))
      ("on_surface" . ,(alist-get 'text colors))
      ("outline" . ,(alist-get 'guide colors))
      ("error" . ,(alist-get 'todo colors))
      ("on_error" . ,(cdr (assoc "on_error" fixed)))
      ("success" . ,(alist-get 'done colors))
      ("warning" . ,(cdr (assoc "warning" fixed))))))

(defvar orgzly-design--palettes nil
  "The derived palettes: ((DARK . SCHEME) ROLES . COLORS) entries.
SCHEME is the device's scheme plist the entry was derived from, compared
by identity: the client keeps one object per report.")

(defvar orgzly-design--measured nil
  "Non-nil while the palettes are the measured ones whatever the device reports.
Bound while the presets register: a preset is immutable, so it is the
measured palette, and the device's colors reach the screen through
`orgzly-design-current-profile' instead.")

(defun orgzly-design--palette (dark)
  "The (ROLES . COLORS) pair for DARK: the device's, else the measured one."
  (let* ((scheme (and (not orgzly-design--measured)
                      (plist-get (jetpacs-colors) (if dark :dark :light))))
         (key (cons dark scheme)))
    (or (cdr (assoc key orgzly-design--palettes))
        (let ((pair (if scheme
                        (let ((colors (orgzly-design--device-colors dark scheme)))
                          (cons (orgzly-design--device-roles dark scheme colors) colors))
                      (cons (if dark orgzly-design-dark orgzly-design-light)
                            (if dark orgzly-design-dark-colors orgzly-design-light-colors)))))
          (push (cons key pair) orgzly-design--palettes)
          ;; Keep the current report's four entries and little else.
          (when (> (length orgzly-design--palettes) 8)
            (setcdr (nthcdr 7 orgzly-design--palettes) nil))
          pair))))

(defvar orgzly-design--profiles nil
  "The built profiles, ((DARK . SCHEME) . PROFILE) entries.
Keyed like `orgzly-design--palettes'.")

(defun orgzly-design-current-profile (dark)
  "Orgzly's profile for DARK in the device's colors, else the measured ones.
A fresh deep copy each call, as `jetpacs-design-profile-get' returns one;
the registered presets are the measured palettes and never change."
  (let* ((key (cons dark (plist-get (jetpacs-colors) (if dark :dark :light))))
         (entry (assoc key orgzly-design--profiles)))
    (unless entry
      (setq entry (cons key (orgzly-design-profile dark)))
      (push entry orgzly-design--profiles)
      (when (> (length orgzly-design--profiles) 8)
        (setcdr (nthcdr 7 orgzly-design--profiles) nil)))
    (copy-tree (cdr entry))))

(defun orgzly-design-roles (dark)
  "Orgzly's EBP theme roles for DARK: an alist of role name to #RRGGBB."
  (car (orgzly-design--palette dark)))

(defun orgzly-design-color (name &optional dark)
  "Return Orgzly color NAME for DARK or the light palette."
  (alist-get name (cdr (orgzly-design--palette dark))))

(defconst orgzly-design-density 2.8125
  "Pixels per dp of the reference device the TextView metrics were measured on.")

(defconst orgzly-design-text-metrics
  '((24 91 80 9) (22 83 73 8) (16 61 53 6) (14 53 46 6) (12 46 40 6))
  "TextView metrics in reference pixels: (SP SINGLE-LINE STEP TOP).
SINGLE-LINE is a one-line TextView's height, STEP the distance between
lines and TOP the font padding above the first line.  Measured from
Revived's layout bounds and glyph positions; TextView rounds its font
metrics to whole pixels, so these are not a fixed multiple of the size.")

(defun orgzly-design--text (size color &rest more)
  "A text style of SIZE sp in COLOR with TextView line metrics, plus MORE pairs.
The line height is the TextView line step; the extra height of a single
line is TextView's font padding, measured above and the rest below."
  (pcase-let ((`(,_ ,single ,step ,top)
               (or (assq size orgzly-design-text-metrics)
                   (let ((px (* size orgzly-design-density)))
                     (list size (* px 1.3553) (* px 1.1719) (* px 0.1503))))))
    (apply #'orgzly-design--text-metrics size single step top color more)))

(defun orgzly-design--text-metrics (size single step top color &rest more)
  "A SIZE sp text style in COLOR from TextView metrics in reference pixels.
SINGLE is a one-line height, STEP the line step and TOP the padding above
the first line; MORE pairs follow."
  (let* ((extra (- single step))
         ;; Compose rounds a text block up to whole pixels, so a step
         ;; below the font's natural height costs one pixel per block.
         (overflow (max 0 (ceiling (- (* size orgzly-design-density 1.1719) step))))
         ;; Truncate, so a dimension never lands a hair above its pixel
         ;; and Compose's ceiling adds a whole one.
         (dp (lambda (px) (/ (floor (* 1000 (/ px orgzly-design-density))) 1000.0))))
    (jetpacs-design-style
     `(("font_size" . ,(jetpacs-design-dimension size))
       ("line_height" . ,(jetpacs-design-dimension (funcall dp step)))
       ("padding_top" . ,(jetpacs-design-dimension (funcall dp (- (round top) overflow))))
       ("padding_bottom" . ,(jetpacs-design-dimension (funcall dp (max 0 (- extra (round top))))))
       ("content_color" . ,(jetpacs-design-color color))
       ("paragraph_breaking" . ,(jetpacs-design-boolean t))
       ,@more))))

(defun orgzly-design--placeholder (size color &rest more)
  "A SIZE sp placeholder style in COLOR on the TextView line step, plus MORE.
It has no padding: the field's text style pads the whole field already,
so a padded placeholder would make an empty field that much taller."
  (let ((step (or (nth 2 (assq size orgzly-design-text-metrics))
                  (* size orgzly-design-density 1.1719))))
    (jetpacs-design-style
     `(("font_size" . ,(jetpacs-design-dimension size))
       ("line_height" . ,(jetpacs-design-dimension
                          (/ (floor (* 1000 (/ step orgzly-design-density))) 1000.0)))
       ("content_color" . ,(jetpacs-design-color color))
       ,@more))))

(defun orgzly-design--text-spaced (size single step top color)
  "A SIZE sp text style in COLOR spaced like a TextView's line spacing.
SINGLE is the one-line height, STEP the line step and TOP the padding
above the first line, in reference pixels; each line's extra height goes
below it and the last line drops it."
  (let ((natural (ceiling (* size orgzly-design-density 1.1719)))
        (dp (lambda (px) (/ (floor (* 1000 (/ px orgzly-design-density))) 1000.0))))
    (jetpacs-design-style
     `(("font_size" . ,(jetpacs-design-dimension size))
       ("line_height" . ,(jetpacs-design-dimension (funcall dp step)))
       ("line_spacing_below" . ,(jetpacs-design-boolean t))
       ("padding_top" . ,(jetpacs-design-dimension (funcall dp top)))
       ("padding_bottom" . ,(jetpacs-design-dimension (funcall dp (- single top natural))))
       ("content_color" . ,(jetpacs-design-color color))
       ("paragraph_breaking" . ,(jetpacs-design-boolean t))))))

(defun orgzly-design--chromeless (&rest pairs)
  "A text-field container style without border or fill, plus PAIRS.
The border is zero wide, so it insets nothing, and transparent, because a
zero width still draws a hairline; it stays so while focused."
  (jetpacs-design-style
   (orgzly-design--pairs (append '(:border_width 0 :border_color "#00000000" :corner_radius 0
                                   :background_color "#00000000")
                                 pairs))
   :rules (list (jetpacs-design-rule "focused"
                 `(("border_width" . ,(jetpacs-design-dimension 0))
                   ("border_color" . ,(jetpacs-design-color "#00000000")))))))

(defun orgzly-design--pairs (pairs)
  "Style properties from keyword PAIRS (:property value).
A string is a color, t a boolean, and a number a dimension."
  (cl-loop for (key value) on pairs by #'cddr
           collect (cons (substring (symbol-name key) 1)
                         (cond ((stringp value) (jetpacs-design-color value))
                               ((eq value t) (jetpacs-design-boolean t))
                               (t (jetpacs-design-dimension value))))))

(defun orgzly-design--box (&rest pairs)
  "A style from keyword PAIRS (:property value) with dimension or color values."
  (jetpacs-design-style (orgzly-design--pairs pairs)))

(defun orgzly-design--menu-style (c edge-inset)
  "Revived's popup menu style in colors C, EDGE-INSET dp from the window edge.
It covers its button, on the menu surface with the popup window's shadow."
  (jetpacs-design-style
   (append (orgzly-design--pairs
            (list :background_color (funcall c 'menu) :border_width 0
                  :border_color "#00000000" :corner_radius 4
                  :padding_vertical 8 :min_width 196
                  :popup_edge_inset edge-inset :popup_offset_y 0.36
                  :elevation 16))
           `(("popup_overlap" . ,(jetpacs-design-boolean t))))))

(defun orgzly-design-overflow-scope (dark window child)
  "CHILD, a toolbar overflow menu, under WINDOW's popup placement in DARK.
Revived's popup ends at its button, which sits 4dp from the window edge,
8dp further in on a 600dp window (values-sw600dp toolbar padding)."
  (let ((c (lambda (name) (orgzly-design-color name dark))))
    (jetpacs-design-scope
     nil
     `(("orgzly.overflow.menu"
        . ,(orgzly-design--menu-style c (if (orgzly-design-sw600-p window) 12 4))))
     (list child)
     :component-styles
     (list (jetpacs-design-component-style "menu.container" '("orgzly.overflow.menu"))))))

(defun orgzly-design-profile (dark)
  "Build Orgzly's immutable Foundation profile, using DARK when non-nil."
  (let* ((roles (mapcar (lambda (p) (cons (car p) (jetpacs-design-color (cdr p))))
                        (orgzly-design-roles dark)))
         (c (lambda (name) (orgzly-design-color name dark))))
    (jetpacs-design-make-profile
     (if dark "orgzly.dark" "orgzly.light") (if dark "Orgzly Dark" "Orgzly Light")
     :tokens roles :theme-roles roles
     :typography
     '(("body" . (:family "system" :size 16 :weight 400 :line-height 18.75 :letter-spacing 0))
       ("title" . (:family "system" :size 22 :weight 400 :line-height 28 :letter-spacing 0))
       ("caption" . (:family "system" :size 14 :weight 400 :line-height 16.41 :letter-spacing 0))
       ("label" . (:family "system" :size 14 :weight 500 :line-height 20 :letter-spacing 0)))
     :styles
     `(;; Toolbar: M3 small top app bar title, 22sp regular.
       ;; The 4.27dp start and 2.13dp top padding move the M3 title slot's
       ;; text onto the AppCompat toolbar title's position; the end padding
       ;; is the gap Toolbar leaves before its first action, and a long title
       ;; is cut at a word as Toolbar cuts it.
       ("orgzly.t.toolbar" . ,(jetpacs-design-style
                               `(("padding_start" . ,(jetpacs-design-dimension 4.27))
                                 ("padding_end" . ,(jetpacs-design-dimension 3.91))
                                 ("padding_top" . ,(jetpacs-design-dimension 2.13))
                                 ("font_size" . ,(jetpacs-design-dimension 22))
                                 ("letter_spacing" . ,(jetpacs-design-dimension 0.12))
                                 ("word_ellipsis" . ,(jetpacs-design-boolean t))
                                 ("paragraph_breaking" . ,(jetpacs-design-boolean t))
                                 ("line_height" . ,(jetpacs-design-dimension 28))
                                 ("content_color" . ,(jetpacs-design-color (funcall c 'strong))))))
       ;; The overflow trigger is as narrow as Revived's ActionButton.Overflow
       ;; leaves its three dots from the screen edge.
       ("orgzly.overflow" . ,(orgzly-design--box :width 28.8))
       ;; Toolbar's overflow dots are the strong text color, the other
       ;; actions the secondary one.
       ("orgzly.overflow.icon" . ,(orgzly-design--box :content_color (funcall c 'strong)
                                                      :ripple_color (orgzly-design-ripple (funcall c 'strong)) :ripple_radius 20))
       ;; Toolbar actions: AppCompat's borderless action ripple, 20dp.
       ("orgzly.toolbar.action" . ,(orgzly-design--box :ripple_color (orgzly-design-ripple (funcall c 'strong)) :ripple_radius 20))
       ;; The overflow popup: it covers its button, 4dp from the edge, on the
       ;; menu surface; 48dp items with 16sp labels 16dp in.
       ("orgzly.menu" . ,(orgzly-design--menu-style c 4))
       ;; AppCompat's rows: 24dp icons, the check box or radio button at the
       ;; end in the accent color, and no highlight for a checked row.
       ("orgzly.menu.item"
        . ,(jetpacs-design-style
            (orgzly-design--pairs
             (list :padding_horizontal 16 :min_height 48 :icon_size 24
                   ;; An icon row starts its icon 8dp in, its label 8dp after.
                   :icon_start 8 :icon_gap 8
                   :content_color (funcall c 'strong) :check_at_end t
                   :check_color (funcall c 'link)))
            :rules (list (jetpacs-design-rule
                          "selected" `(("background_color" . ,(jetpacs-design-color (funcall c 'menu))))))))
       ("orgzly.t.menu" . ,(orgzly-design--text 16 (funcall c 'strong)
                                                `("letter_spacing" . ,(jetpacs-design-dimension 0.5))))
       ;; Notebook card: item_book.xml, an elevated M3 card 12dp below the
       ;; previous one, 16dp from the screen edges, 16dp padding.
       ("orgzly.card.margin" . ,(orgzly-design--box :padding_horizontal 16 :padding_top 12
                                                    :fill_width t))
       ("orgzly.card" . ,(orgzly-design--box :background_color (funcall c 'container)
                                             :corner_radius 12 :padding 16 :fill_width t))
       ;; Letter spacing reproduces the measured advance of the upstream
       ;; TextAppearances (labelLarge carries 0.1sp).
       ("orgzly.t.book" . ,(orgzly-design--text 24 (funcall c 'strong)
                                                `("letter_spacing" . ,(jetpacs-design-dimension 0.11))))
       ("orgzly.t.detail" . ,(orgzly-design--text 12 (funcall c 'text)
                                                  `("letter_spacing" . ,(jetpacs-design-dimension 0.065))))
       ;; BookItemInfoIcon: 14dp, 2dp start and 8dp end margins.
       ("orgzly.detail.icon" . ,(orgzly-design--box :padding_start 2 :padding_end 8))
       ("orgzly.details" . ,(orgzly-design--box :padding_top 8))
       ;; Outline: item_head.xml at the comfortable density.
       ("orgzly.t.preface" . ,(orgzly-design--text 16 (funcall c 'text)))
       ("orgzly.preface" . ,(orgzly-design--box :padding_horizontal 16 :padding_vertical 8
                                                :fill_width t))
       ("orgzly.t.title" . ,(orgzly-design--text 16 (funcall c 'text)))
       ("orgzly.t.content" . ,(orgzly-design--text 14 (funcall c 'text)))
       ("orgzly.t.fold" . ,(orgzly-design--text 16 (funcall c 'text)
                                                `("text_align" . ,(jetpacs-design-text-align "center"))))
       ;; Fixed sizes inside the row are universal attributes, not style
       ;; widths: Foundation Styles sizes do not report intrinsic width.
       ("orgzly.row.box" . ,(orgzly-design--box :fill_width t :intrinsic_height t))
       ("orgzly.result.box" . ,(orgzly-design--box :padding_horizontal 16 :fill_width t
                                                   :intrinsic_height t))
       ;; Rows are as dense as Revived's (31.6dp for one line): an authored
       ;; minimum height lifts the pressable's 48dp default.
       ("orgzly.head" . ,(jetpacs-design-style
                          `(("padding_vertical" . ,(jetpacs-design-dimension 5))
                            ("min_height" . ,(jetpacs-design-dimension 0)))
                          :rules (list (jetpacs-design-rule "pressed"
                                 `(("background_color" . ,(jetpacs-design-color (funcall c 'container))))))))
       ("orgzly.faded" . ,(jetpacs-design-style `(("alpha" . ,(jetpacs-design-number 0.45)))))
       ;; Android truncates the 2dp offset to 5 reference pixels.
       ("orgzly.below-title" . ,(orgzly-design--box :padding_top 1.78))
       ;; indent.xml: a 20dp slot with a 1dp VerticalDivider centred in it.
       ("orgzly.guide.line" . ,(orgzly-design--box :background_color (funcall c 'guide)))
       ;; The bullet's 20dp slot is the first line's height, its 8dp dot centred.
       ("orgzly.bullet.dot" . ,(orgzly-design--box :corner_radius 4
                                                   :background_color (funcall c 'bullet)))
       ;; bullet_folded.xml: the dot on a secondary-container disc.
       ("orgzly.bullet.ring" . ,(orgzly-design--box :corner_radius 8
                                                    :background_color (funcall c 'pill)))
       ("orgzly.fold" . ,(orgzly-design--box :min_width 0 :min_height 0 :padding_top 5))
       ;; FAB: 56dp, 16dp corners, 16dp from the edges.
       ("orgzly.fab" . ,(orgzly-design--box :width 56 :height 56 :corner_radius 16 :elevation 6
                                            :ripple_color (orgzly-design-ripple (funcall c 'strong))
                                            :background_color (funcall c 'fab)
                                            :content_color (funcall c 'on-fab)))
       ;; Drawer: a 320dp NavigationView; its menu starts 8dp below the
       ;; status bar with 48dp rows, the active one a pill inset 12dp.
       ("orgzly.drawer.sheet" . ,(orgzly-design--box :width 320
                                                     :background_color (funcall c 'container)))
       ;; DrawerLayout's default scrim, 60% black.
       ("orgzly.scrim" . ,(orgzly-design--box :background_color "#99000000"))
       ("orgzly.drawer" . ,(orgzly-design--box :padding_top 8 :fill_width t))
       ("orgzly.drawer.margin" . ,(orgzly-design--box :padding_horizontal 11.29 :fill_width t))
       ("orgzly.drawer.item" . ,(orgzly-design--box :min_height 0 :padding_horizontal 16.58
                                                    :fill_width t))
       ("orgzly.drawer.active" . ,(orgzly-design--box :min_height 0 :corner_radius 24
                                                      :background_color (funcall c 'pill)
                                                      :content_color (funcall c 'on-pill)
                                                      :padding_horizontal 16.58 :fill_width t))
       ("orgzly.drawer.plain" . ,(orgzly-design--box :min_height 0))
       ;; The Sync footer: a 24dp icon 16dp from the edge, 12dp above the bottom.
       ("orgzly.drawer.footer" . ,(orgzly-design--box :padding_start 16 :padding_bottom 12
                                                      :fill_width t))
       ("orgzly.t.drawer" . ,(orgzly-design--text 14 (funcall c 'text)
                                                  `("font_weight" . ,(jetpacs-design-font-weight 500))
                                                  `("letter_spacing" . ,(jetpacs-design-dimension 0.1))))
       ("orgzly.form" . ,(orgzly-design--box :padding 16 :fill_width t))
       ;; Note screen: fragment_note.xml.
       ("orgzly.t.value" . ,(orgzly-design--text 16 (funcall c 'text)))
       ;; Breadcrumbs: 16sp at lineSpacingMultiplier 1.25, 61px for one
       ;; line and 66px more for each further one.
       ("orgzly.t.bread" . ,(orgzly-design--text-spaced 16 61 66 6 (funcall c 'text)))
       ("orgzly.t.hint" . ,(orgzly-design--text 16 (funcall c 'hint)))
       ("orgzly.note.bread" . ,(orgzly-design--box :min_height 48 :padding_horizontal 16
                                                   :padding_vertical 12))
       ("orgzly.divider" . ,(orgzly-design--box :fill_width t :background_color (funcall c 'guide)))
       ("orgzly.note.header" . ,(orgzly-design--box :min_height 0 :fill_width t))
       ("orgzly.note.value" . ,(orgzly-design--box :min_height 0 :fill_width t))
       ("orgzly.note.title-view" . ,(orgzly-design--box :min_height 0 :fill_width t
                                                        :padding_horizontal 16 :padding_vertical 6))
       ("orgzly.note.content-view" . ,(orgzly-design--box :min_height 0 :fill_width t
                                                          :padding_horizontal 16 :padding_vertical 6))
       ;; Chromeless fields shaped like the upstream TextViews.
       ("orgzly.field.title.box" . ,(orgzly-design--chromeless
                                                        :padding_horizontal 16 :padding_vertical 6))
       ;; font_large 22sp; the measured advance needs 0.15sp of letter spacing.
       ("orgzly.field.title.text" . ,(orgzly-design--text 22 (funcall c 'text)
                                                          `("min_height" . ,(jetpacs-design-dimension 0))
                                                          `("letter_spacing" . ,(jetpacs-design-dimension 0.15))))
       ("orgzly.field.title.hint" . ,(orgzly-design--placeholder 22 (funcall c 'hint)
                                                          `("letter_spacing" . ,(jetpacs-design-dimension 0.15))))
       ("orgzly.field.value.box" . ,(orgzly-design--chromeless :padding 0))
       ;; A centred field sits lower than the EditText, so its text pads 5px.
       ("orgzly.field.value.text" . ,(orgzly-design--text-metrics 16 61 53 5 (funcall c 'text)
                                                                  `("min_height" . ,(jetpacs-design-dimension 0))))
       ("orgzly.field.value.hint" . ,(orgzly-design--text-metrics 16 61 53 5 (funcall c 'hint)))
       ("orgzly.field.search.box" . ,(orgzly-design--chromeless :padding 0))
       ("orgzly.field.search.text" . ,(orgzly-design--text-metrics 22 83 73 8 (funcall c 'strong)
                                                                   `("min_height" . ,(jetpacs-design-dimension 0))))
       ("orgzly.field.search.hint" . ,(orgzly-design--text-metrics 22 83 73 8 (funcall c 'hint)))
       ;; Searches: a saved query's 18sp name over its 14sp query.
       ("orgzly.search.row" . ,(orgzly-design--box :padding_horizontal 16 :padding_vertical 6
                                                   :fill_width t))
       ("orgzly.t.search-name" . ,(orgzly-design--text-metrics 18 69 60 7 (funcall c 'text)))
       ;; Agenda day dividers: bold 14sp on the container band.
       ("orgzly.band" . ,(orgzly-design--box :background_color (funcall c 'container)
                                             :padding_horizontal 16 :fill_width t))
       ("orgzly.t.band" . ,(orgzly-design--text 14 (funcall c 'text)
                                                `("font_weight" . ,(jetpacs-design-font-weight 700))))
       ;; Settings: PreferenceFragment rows, 72dp text start, 16dp padding.
       ("orgzly.pref" . ,(orgzly-design--box :min_height 0 :fill_width t :padding_vertical 16
                                             :padding_start 72 :padding_end 16))
       ;; The 24dp icon would otherwise stretch a 16dp-padded row.
       ("orgzly.pref.category" . ,(orgzly-design--box :min_height 0 :fill_width t
                                                      :padding_vertical 14.58 :padding_end 16))
       ;; titleMedium and bodyMedium: 0.15sp and 0.25sp of letter spacing,
       ;; a wrapped summary 20sp a line.
       ("orgzly.t.pref-title" . ,(orgzly-design--text 16 (funcall c 'strong)
                                                      `("font_weight" . ,(jetpacs-design-font-weight 500))
                                                      `("letter_spacing" . ,(jetpacs-design-dimension 0.15))))
       ("orgzly.t.summary" . ,(orgzly-design--text-metrics 14 53 56 6 (funcall c 'text)
                                                           `("letter_spacing" . ,(jetpacs-design-dimension 0.25))))
       ("orgzly.pref.header" . ,(orgzly-design--box :padding_start 72 :padding_top 16 :fill_width t))
       ("orgzly.t.pref-header" . ,(orgzly-design--text 14 (funcall c 'header)
                                                       `("font_weight" . ,(jetpacs-design-font-weight 500))))
       ;; The AppCompat switch's track and thumb, on and off.
       ("orgzly.switch.track-on" . ,(orgzly-design--box :corner_radius 6.75
                                                        :background_color (funcall c 'track-on)))
       ("orgzly.switch.track-off" . ,(orgzly-design--box :corner_radius 6.75
                                                         :background_color (funcall c 'track-off)))
       ("orgzly.switch.thumb-on" . ,(orgzly-design--box :corner_radius 9.75
                                                        :background_color (funcall c 'thumb-on)))
       ("orgzly.switch.thumb-off" . ,(orgzly-design--box :corner_radius 9.75
                                                         :background_color (funcall c 'thumb-off)))
       ;; The dialog's EditText: 16dp in, hint at 4dp, a 2dp focused underline.
       ("orgzly.dialog.input" . ,(orgzly-design--box :padding_horizontal 16 :fill_width t))
       ("orgzly.dialog.underline" . ,(orgzly-design--box :fill_width t
                                                         :background_color (funcall c 'header)))
       ("orgzly.field.dialog.box" . ,(orgzly-design--chromeless :padding_start 4.27 :padding_top 7.46
                                                                :padding_bottom 1.42))
       ("orgzly.field.dialog.text" . ,(orgzly-design--text-metrics 18 68 60 7 (funcall c 'strong)
                                                                   `("min_height" . ,(jetpacs-design-dimension 0))))
       ("orgzly.field.dialog.hint" . ,(orgzly-design--text-metrics 18 68 60 7 (funcall c 'dialog-hint)))
       ("orgzly.field.content.box" . ,(orgzly-design--chromeless
                                                          :padding_horizontal 16 :padding_vertical 6))
       ("orgzly.field.content.text" . ,(orgzly-design--text 16 (funcall c 'text)
                                                            `("min_height" . ,(jetpacs-design-dimension 0))))
       ("orgzly.field.content.hint" . ,(orgzly-design--placeholder 16 (funcall c 'hint)))
       ;; Dialogs: the 316dp AlertDialog, drawn by the host through chrome.dialog.
       ;; alpha is the window's dim behind it: the AlertDialog's 0.32.
       ("orgzly.dialog.sheet" . ,(orgzly-design--dialog-sheet (funcall c 'dialog) 316.44))
       ("orgzly.dialog" . ,(orgzly-design--box :fill_width t))
       ("orgzly.dialog.title" . ,(orgzly-design--box :padding_horizontal 24 :fill_width t))
       ("orgzly.t.dialog-title" . ,(orgzly-design--text 24 (funcall c 'strong)
                                                        `("letter_spacing" . ,(jetpacs-design-dimension 0.15))))
       ("orgzly.t.dialog-message" . ,(orgzly-design--text 14 (funcall c 'text)))
       ("orgzly.dialog.item" . ,(orgzly-design--box :min_height 0 :fill_width t :ripple_color (orgzly-design-ripple (funcall c 'strong))))
       ;; Period pickers: an AppCompat AlertDialog under Material 3's dialog
       ;; overlay, its two-line centred descriptions around three wheels.
       ("orgzly.t.period-title" . ,(orgzly-design--text 20 (funcall c 'strong)))
       ("orgzly.period.hint" . ,(orgzly-design--box :padding_horizontal 16 :height 35.5 :fill_width t))
       ("orgzly.t.period-hint" . ,(orgzly-design--text 14 (funcall c 'text)
                                                       `("text_align" . ,(jetpacs-design-text-align "center"))))
       ;; Text buttons: 48dp tall, at least 64dp wide, the label centred.
       ("orgzly.dialog.button" . ,(orgzly-design--box :min_height 0 :min_width 0 :corner_radius 100
                                                      :ripple_color (orgzly-design-ripple (funcall c 'strong))))
       ("orgzly.t.dialog-button" . ,(orgzly-design--text 14 (funcall c 'link)
                                                         `("font_weight" . ,(jetpacs-design-font-weight 500))
                                                         `("text_align" . ,(jetpacs-design-text-align "center"))))
       ;; Timestamp dialog: dialog_timestamp.xml under its 16dp edge, a
       ;; 36dp bold heading, 12sp caps labels and 40dp tonal buttons.
       ("orgzly.ts.head" . ,(orgzly-design--box :padding 16 :fill_width t))
       ("orgzly.t.ts-title" . ,(orgzly-design--text 18 (funcall c 'text)
                                                    `("font_weight" . ,(jetpacs-design-font-weight 700))
                                                    `("text_align" . ,(jetpacs-design-text-align "center"))))
       ("orgzly.ts.body" . ,(orgzly-design--box :padding 16 :fill_width t))
       ("orgzly.t.ts-label" . ,(orgzly-design--text 12 (funcall c 'text)
                                                    `("text_align" . ,(jetpacs-design-text-align "center"))))
       ;; 40dp: the 53px label block centred by its padding.
       ("orgzly.ts.pill" . ,(orgzly-design--box :min_height 0 :corner_radius 100 :fill_width t :ripple_color (orgzly-design-ripple (funcall c 'strong))
                                                :padding_horizontal 24 :padding_vertical 10.58
                                                :background_color (funcall c 'pill)))
       ("orgzly.t.ts-pill" . ,(orgzly-design--text 14 (funcall c 'on-pill)
                                                   `("font_weight" . ,(jetpacs-design-font-weight 500))
                                                   `("letter_spacing" . ,(jetpacs-design-dimension 0.1))
                                                   `("text_align" . ,(jetpacs-design-text-align "center"))))
       ;; The date and time buttons, drawn by the Companion's picker face;
       ;; a time too wide for its half wraps, 46px a line.
       ("orgzly.ts.picker" . ,(jetpacs-design-style
                               (orgzly-design--pairs
                                (list :min_height 40 :corner_radius 100 :padding_horizontal 24
                                      :padding_vertical 7.1 :background_color (funcall c 'pill)
                                      :ripple_color (orgzly-design-ripple (funcall c 'strong))
                                      ;; Revived opens the platform's date and time dialogs.
                                      :platform_dialog t))
                               :rules (list (jetpacs-design-rule
                                             "disabled"
                                             ;; Revived greys the button by color, not opacity.
                                             `(("background_color"
                                                . ,(jetpacs-design-color (funcall c 'pill-off)))
                                               ("alpha" . ,(jetpacs-design-number 1)))))))
       ("orgzly.t.ts-picker" . ,(jetpacs-design-style
                                 `(("font_size" . ,(jetpacs-design-dimension 14))
                                   ("line_height" . ,(jetpacs-design-dimension 16.355))
                                   ("font_weight" . ,(jetpacs-design-font-weight 500))
                                   ("letter_spacing" . ,(jetpacs-design-dimension 0.1))
                                   ("text_align" . ,(jetpacs-design-text-align "center"))
                                   ("max_lines" . ,(jetpacs-design-number 3))
                                   ("content_color" . ,(jetpacs-design-color (funcall c 'on-pill))))
                                 :rules (list (jetpacs-design-rule
                                               "disabled"
                                               `(("content_color"
                                                  . ,(jetpacs-design-color (funcall c 'on-pill-off)))))))))
     :component-styles
     '(("chrome.drawer" . ("orgzly.drawer.sheet"))
       ("picker.container" . ("orgzly.ts.picker"))
       ("picker.label" . ("orgzly.t.ts-picker"))
       ("menu.trigger" . ("orgzly.overflow.icon"))
       ("icon-button.container" . ("orgzly.toolbar.action"))
       ("menu.container" . ("orgzly.menu"))
       ("menu.item" . ("orgzly.menu.item"))
       ("menu.item-label" . ("orgzly.t.menu"))
       ("chrome.scrim" . ("orgzly.scrim"))
       ("chrome.dialog" . ("orgzly.dialog.sheet"))
       ("text.body" . ("typography.body")) ("text.title" . ("typography.title"))
       ("text.caption" . ("typography.caption")) ("text.label" . ("typography.label"))
       ("text-field.label" . ("typography.caption"))
       ("text-field.text" . ("typography.body"))
       ("section-header.title" . ("typography.label"))
       ("list-item.title" . ("typography.body"))
       ("list-item.subtitle" . ("typography.caption"))))))

(defun orgzly-design-ripple (color)
  "The touch ripple of Revived's Material 3 theme over COLOR: 14% of it."
  (concat color "24"))

(defun orgzly-design-register ()
  "Register both Orgzly style presets idempotently.
The presets are the measured palettes; the device's colors are not a
preset but the profile `orgzly-design-current-profile' wraps with."
  (let ((orgzly-design--measured t))
    (dolist (dark '(nil t)) (jetpacs-design-register-profile (orgzly-design-profile dark)))))

(defun orgzly-design--member (plist name)
  "The value of the key named NAME in PLIST, interned or not."
  (cl-loop for (key value) on plist by #'cddr
           when (equal (symbol-name key) name) return value))

(defun orgzly-design-dialog-width (window)
  "The AlertDialog width in dp for WINDOW, a `jetpacs-window' plist.
AppCompat sizes a dialog to its minimum width, 95% of a phone window or 80%
from a 600dp smallest width, less the 24dp background inset on each side;
without a report it is the reference phone's."
  ;; By name: EBP decodes a member Emacs never interned as a fresh
  ;; symbol, so `plist-get' misses it when the report predates this file.
  (let ((w (orgzly-design--member window ":width_dp"))
        (h (orgzly-design--member window ":height_dp")))
    (if (not (and (numberp w) (numberp h))) 316.44
      (- (* w (if (>= (min w h) 600) 0.8 0.95)) 48))))

(defun orgzly-design-sw600-p (window)
  "Whether WINDOW, a `jetpacs-window' plist, is 600dp at its smallest.
Android then takes Revived's values-sw600dp resources."
  (let ((w (orgzly-design--member window ":width_dp"))
        (h (orgzly-design--member window ":height_dp")))
    (and (numberp w) (numberp h) (>= (min w h) 600))))

(defun orgzly-design-fold-width (window)
  "The outline's fold button width in dp for WINDOW, a `jetpacs-window' plist.
Revived's fold_button_width is 48dp, and 60dp from a 600dp smallest width."
  (if (orgzly-design-sw600-p window) 60 48))

(defun orgzly-design-tags-scope (dark window clear child)
  "CHILD, the tags menu, under the scope of Revived's tags dropdown.
The popup spans the Tags field of WINDOW, a `jetpacs-window' plist: the
field starts 64dp in and ends 52dp from the edge at the menu button, 48dp
more with the CLEAR button.  It opens 7.5dp over the field's bottom on
DARK's popup surface, with 48dp rows whose 16sp labels sit 8dp in."
  (let* ((w (or (orgzly-design--member window ":width_dp") 384))
         (width (- w 64.36 52 (if clear 48 0)))
         (c (lambda (name) (orgzly-design-color name dark))))
    (jetpacs-design-scope
     nil
     `(("orgzly.tags.menu"
        . ,(jetpacs-design-style
            (append (orgzly-design--pairs
                     (list :background_color (funcall c 'menu) :border_width 0
                           :border_color "#00000000" :corner_radius 4
                           :padding_vertical 8 :min_width width :popup_offset_y 40.5
                           :elevation 16))
                    ;; From the 48dp button's top: 7.5dp over the field's bottom.
                    `(("popup_overlap" . ,(jetpacs-design-boolean t))
                      ("popup_before_anchor" . ,(jetpacs-design-boolean t))))))
       ("orgzly.tags.item" . ,(orgzly-design--box :padding_horizontal 8 :min_height 48))
       ;; Revived's borderless 48dp ImageButton: a bounded ripple.
       ("orgzly.tags.trigger" . ,(orgzly-design--box :content_color (funcall c 'text)
                                                     :ripple_color (orgzly-design-ripple (funcall c 'strong)))))
     (list child)
     :component-styles
     (list (jetpacs-design-component-style "menu.container" '("orgzly.tags.menu"))
           (jetpacs-design-component-style "menu.item" '("orgzly.tags.item"))
           (jetpacs-design-component-style "menu.item-label" '("orgzly.t.menu"))
           (jetpacs-design-component-style "menu.trigger" '("orgzly.tags.trigger"))))))

(defun orgzly-design-wrap-dialog (dialog dark &optional sheet)
  "Wrap DIALOG like `orgzly-design-wrap' with DARK, sized for the window.
SHEET, when given, is the dialog sheet style instead."
  (let* ((active (jetpacs-design-active-profile))
         (profile (if (assoc "orgzly.card" (plist-get active :styles)) (copy-tree active)
                    (orgzly-design-current-profile dark)))
         (sheet-style sheet)
         (sheet (assoc "orgzly.dialog.sheet" (plist-get profile :styles))))
    (when sheet
      (setcdr sheet (or sheet-style
                        (orgzly-design--dialog-sheet (orgzly-design-color 'dialog dark)
                                                     (orgzly-design-dialog-width (jetpacs-window))))))
    (jetpacs-design-profile-scope profile (list dialog))))

(defun orgzly-design-period-sheet (dark)
  "The sheet of Revived's period pickers for DARK: 240dp, 4dp corners, surface."
  (orgzly-design--dialog-sheet (orgzly-design-color 'surface dark) 240 4))

(defun orgzly-design--dialog-sheet (color width &optional radius)
  "The chrome.dialog style: a WIDTH dp AlertDialog in COLOR over a 0.32 dim.
RADIUS is its corner radius, 28dp by default."
  (jetpacs-design-style
   (append (orgzly-design--pairs (list :width width :corner_radius (or radius 28) :padding 0
                                       :elevation 6
                                       :background_color color))
           `(("alpha" . ,(jetpacs-design-number 0.32))))))

(defun orgzly-design-wrap (screen dark)
  "Wrap SCREEN in the selected profile, defaulting to DARK's Orgzly preset."
  (let* ((active (jetpacs-design-active-profile))
         (profile (if (assoc "orgzly.card" (plist-get active :styles)) active
                    (orgzly-design-current-profile dark))))
    (jetpacs-design-profile-scope profile (list screen))))

(provide 'orgzly-design)
;;; orgzly-design.el ends here
