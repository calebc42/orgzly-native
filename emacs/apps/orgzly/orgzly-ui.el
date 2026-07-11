;;; orgzly-ui.el --- Books, note list & note editor views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; The Orgzly screens, as jetpacs shell views (namespaced "orgzly.*" per
;; the multi-app contract in jetpacs-apps.el):
;;   "orgzly.books"   tab      — the notebooks list (create/rename/delete/preface)
;;   "orgzly.book"    overlay  — one book as a flat foldable outline in Orgzly's
;;                               item_head idiom: colored state keywords, icon-led
;;                               planning lines, inline content, swipe/long-press
;;                               quick-action popup, batch selection
;;   "orgzly.note"    overlay  — the note editor: breadcrumbs, inline title,
;;                               icon-led metadata rows with clear buttons, the
;;                               timestamp dialog, tags, properties, content
;;   "orgzly.preface" overlay  — the book preface editor
;;
;; All mutations funnel through orgzly-data.el (which invalidates the scan
;; memo); every handler ends in a `jetpacs-shell-push'.  Prompts inside
;; handlers (`read-string', `completing-read', `y-or-n-p') surface as
;; native dialogs via the jetpacs minibuffer bridge.
;;
;; Rendering constraint worth knowing: the companion honours `color' only
;; on rich-text spans (hex) and icons (hex or theme token) — `jetpacs-text'
;; color is ignored.  Every piece of colored text below is therefore a
;; span, and icons use the "outline" theme token so they adapt to the
;; device light/dark theme.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'jetpacs)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'jetpacs-surfaces)
(require 'orgzly-data)
(require 'orgzly-query)

;; ─── Display preferences (Orgzly's Display settings) ─────────────────────────

(defcustom orgzly-display-content t
  "When non-nil, note content is shown in note lists (Orgzly: display content)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-content-line-count nil
  "When non-nil, titles carry the content line count when content is hidden."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-planning t
  "When non-nil, planning times show under note titles."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-display-book-name-in-search 'under
  "Where search and agenda rows show the note's notebook.
Orgzly's three placements: nil hides it, `before' puts it above the
title, `under' (or t) among the metadata lines below."
  :type '(choice (const :tag "Hide" nil)
                 (const :tag "Before note" before)
                 (const :tag "Under note" under))
  :group 'orgzly)

(defcustom orgzly-content-preview-lines 0
  "Maximum content lines shown under a title in note lists; 0 = all.
Orgzly shows the full content, so the cap defaults off."
  :type 'integer :group 'orgzly)

;; ─── Drill-in state ──────────────────────────────────────────────────────────

(defvar orgzly-ui--current-book nil
  "Name of the open book, or nil when on the notebooks list.")

(defvar orgzly-ui--note-ref nil
  "Ref alist of the note open in the editor view, or nil.")

(defvar orgzly-ui--preface-book nil
  "Book whose preface is being edited, or nil.")

(defvar orgzly-ui--select-mode nil
  "Non-nil while the book view is in multi-select mode.")

(defvar orgzly-ui--selection nil
  "Refs of the notes selected in multi-select mode.")

(defvar orgzly-ui--focus-ref nil
  "Ref of a note the book view should scroll into view (upstream's focus).")

(defvar orgzly-ui--narrow-ref nil
  "Ref whose subtree the book view is narrowed to, or nil (upstream's narrow).")

(defvar orgzly-ui--fold-generation 0
  "Bumped by the visibility cycle: fold state lives on-device keyed by
collapsible id, so new ids are the only way to reset it.")

(defvar orgzly-ui--fold-all nil
  "Non-nil while the visibility cycle has everything folded.")

(defun orgzly-ui--reset-drill-in (&optional _view)
  "Clear all drill-in state (used when the user lands on a tab)."
  (setq orgzly-ui--current-book nil
        orgzly-ui--note-ref nil
        orgzly-ui--preface-book nil
        orgzly-ui--select-mode nil
        orgzly-ui--selection nil
        orgzly-ui--focus-ref nil
        orgzly-ui--narrow-ref nil))

(add-hook 'jetpacs-shell-view-switched-hook #'orgzly-ui--reset-drill-in)
(add-hook 'jetpacs-shell-refresh-hook #'orgzly-data-invalidate)

;; ─── Refs on the wire ────────────────────────────────────────────────────────

(defun orgzly-ui--args-ref (args)
  "The note ref carried in action ARGS."
  `((file . ,(alist-get 'file args))
    (pos . ,(alist-get 'pos args))
    (title . ,(alist-get 'title args))))

(defun orgzly-ui--entry-for-ref (ref)
  "The current entry alist for REF, matched by position then title."
  (let* ((file (alist-get 'file ref))
         (book (orgzly-data-book-name file))
         (entries (orgzly-data-entries book)))
    (or (cl-find-if (lambda (e) (and (equal (alist-get 'file e) file)
                                     (eql (alist-get 'pos e) (alist-get 'pos ref))))
                    entries)
        (cl-find-if (lambda (e) (equal (alist-get 'title e)
                                       (alist-get 'title ref)))
                    entries))))

(defun orgzly-ui--note-action (name entry-or-ref &rest extra)
  "An `jetpacs-action' NAME carrying ENTRY-OR-REF's ref plus EXTRA args."
  (let ((ref (if (alist-get 'book entry-or-ref)
                 (orgzly-data-entry-ref entry-or-ref)
               entry-or-ref)))
    (jetpacs-action name :args (append ref extra))))

;; ─── Shared rendering ────────────────────────────────────────────────────────

;; Orgzly's item_head palette: open keywords red, done keywords green
;; (light theme red_900/green_900, dark red_200/green_200).  Span colors
;; are raw hex on the wire, so use the Material 600s — readable on both
;; companion themes.
(defconst orgzly-ui--todo-color "#e53935" "Open keyword color (Orgzly red).")
(defconst orgzly-ui--done-color "#43a047" "Done keyword color (Orgzly green).")
(defconst orgzly-ui--muted-color "#8a8a8a" "Secondary text (post-title, times).")
(defconst orgzly-ui--todo-faded-color "#c98a88"
  "Open keyword on a faded (done/archived) row — Orgzly's 45% alpha.")
(defconst orgzly-ui--done-faded-color "#8fbf91"
  "Done keyword on a faded row.")

(defun orgzly-ui--done-p (entry)
  (let ((kws (orgzly-data-todo-keywords)))
    (and (alist-get 'state entry)
         (member (alist-get 'state entry) (cdr kws)) t)))

(defun orgzly-ui--faded-p (entry)
  "Non-nil when ENTRY renders faded — done or archived (Orgzly's 45% alpha)."
  (or (orgzly-ui--done-p entry)
      (member "ARCHIVE" (alist-get 'tags entry))
      (member "ARCHIVE" (alist-get 'itags entry))
      nil))

(cl-defun orgzly-ui--title-node (entry &key hide-content)
  "The headline laid out as Orgzly's TitleGenerator does:
STATE  #A  Title  tags  count — state colored (red open / green done),
state and priority bold, post-title text muted and space-separated.
Done and archived rows fade entirely (alpha emulated with the muted
color).  The content line count appears only when HIDE-CONTENT and the
line-count preference are on."
  (let* ((state (alist-get 'state entry))
         (done (orgzly-ui--done-p entry))
         (fade (and (orgzly-ui--faded-p entry) orgzly-ui--muted-color))
         (tags (alist-get 'tags entry))
         (lines (or (alist-get 'content-lines entry) 0)))
    (jetpacs-rich-text
     (append
      (when state
        (list (jetpacs-span (concat state "  ") :bold t
                         ;; A faded row fades its keyword too (upstream
                         ;; alphas the whole row) — desaturated hues.
                         :color (cond ((and done fade) orgzly-ui--done-faded-color)
                                      (done orgzly-ui--done-color)
                                      (fade orgzly-ui--todo-faded-color)
                                      (t orgzly-ui--todo-color)))))
      (when (alist-get 'priority entry)
        (list (jetpacs-span (format "#%s  " (alist-get 'priority entry))
                         :bold t :color fade)))
      (list (jetpacs-span (alist-get 'title entry) :color fade))
      (when tags
        (list (jetpacs-span
               (concat "  " (mapconcat #'substring-no-properties tags " "))
               :color orgzly-ui--muted-color)))
      (when (and hide-content orgzly-display-content-line-count (> lines 0))
        (list (jetpacs-span (format "  %d" lines)
                         :color orgzly-ui--muted-color))))
     :style 'body)))

(defun orgzly-ui--icon-line (icon label &optional color)
  "One icon-led metadata line under a title (Orgzly's item_head rows)."
  (jetpacs-row
   (jetpacs-icon icon :size 14 :color "outline")
   (jetpacs-rich-text
    (list (jetpacs-span label :color (or color orgzly-ui--muted-color)))
    :style 'caption)
   :spacing 6 :align "center"))

(defun orgzly-ui--ts-label (ts)
  "TS as Orgzly's user-facing time: \"Wed, Jul 9 10:00 +1w\"."
  (let* ((time (seconds-to-time (alist-get 'time ts)))
         (this-year (equal (format-time-string "%Y")
                           (format-time-string "%Y" time)))
         (base (format-time-string
                (concat "%a, %b %-d" (unless this-year ", %Y")
                        (when (alist-get 'has-time ts) " %H:%M"))
                time)))
    (concat base
            (when (alist-get 'repeater ts)
              (concat " " (alist-get 'repeater ts))))))

(cl-defun orgzly-ui--meta-lines (entry &key show-book only-kind)
  "The icon-led lines under ENTRY's title, per the display preferences.
ONLY-KIND, when non-nil, keeps just that planning line — the agenda
shows only the time responsible for an item's presence."
  (let ((keep (lambda (kind) (or (null only-kind) (eq kind only-kind)))))
    (delq nil
          (list
           (when (and show-book
                      (memq orgzly-display-book-name-in-search '(t under)))
             (orgzly-ui--icon-line "folder_open" (alist-get 'book entry)))
           (when (and orgzly-display-planning (funcall keep 'scheduled))
             (when-let ((ts (alist-get 'scheduled entry)))
               (orgzly-ui--icon-line "today" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (funcall keep 'deadline))
             (when-let ((ts (alist-get 'deadline entry)))
               (orgzly-ui--icon-line "alarm" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (funcall keep 'event))
             (when-let ((ts (car (alist-get 'events entry))))
               (orgzly-ui--icon-line "access_time" (orgzly-ui--ts-label ts))))
           (when (and orgzly-display-planning (null only-kind))
             (when-let ((ts (alist-get 'closed entry)))
               (orgzly-ui--icon-line "task_alt" (orgzly-ui--ts-label ts))))))))

(defun orgzly-ui--content-node (entry)
  "ENTRY's content shown under the title (org-highlighted), or nil.
`orgzly-content-preview-lines' caps it when non-zero (Orgzly shows it
whole).  On a faded row the content renders as a muted span instead of
highlighted markup — text-node colors don't reach the wire, so the
whole-row fade costs the preview its highlighting."
  (when orgzly-display-content
    (let ((content (alist-get 'content entry))
          (cap orgzly-content-preview-lines))
      (when (and (stringp content) (not (string-empty-p content)))
        (let* ((lines (split-string content "\n"))
               (shown (if (and (> cap 0) (> (length lines) cap))
                          (concat (string-join (seq-take lines cap) "\n")
                                  "\n…")
                        content)))
          (if (orgzly-ui--faded-p entry)
              (jetpacs-rich-text
               (list (jetpacs-span shown :color orgzly-ui--muted-color))
               :style 'caption)
            (jetpacs-markup shown :syntax "org" :style 'caption)))))))

(defun orgzly-ui--selected-p (entry)
  (let ((file (alist-get 'file entry)) (pos (alist-get 'pos entry)))
    (cl-find-if (lambda (r) (and (equal (alist-get 'file r) file)
                                 (eql (alist-get 'pos r) pos)))
                orgzly-ui--selection)))

(cl-defun orgzly-ui--note-row (entry &key show-book only-kind hide-content)
  "One note as a tappable flat row; the building block of every list.
Title plus icon-led metadata lines — tap opens the note (or toggles
selection in multi-select mode)."
  (let ((ref (orgzly-data-entry-ref entry)))
    (apply #'jetpacs-row
           (append
            (when orgzly-ui--select-mode
              (list (jetpacs-checkbox
                     (format "orgzly-sel:%s:%s" (alist-get 'file entry)
                             (alist-get 'pos entry))
                     :checked (and (orgzly-ui--selected-p entry) t)
                     :on-change (orgzly-ui--note-action "orgzly.note.select" ref))))
            (list
             (jetpacs-box
              (list (apply #'jetpacs-column
                           (append
                            ;; "Before note" placement of the notebook
                            ;; name (Orgzly's third option).
                            (when (and show-book
                                       (eq orgzly-display-book-name-in-search
                                           'before))
                              (list (orgzly-ui--icon-line
                                     "folder_open" (alist-get 'book entry))))
                            (cons (orgzly-ui--title-node
                                   entry :hide-content hide-content)
                                  (orgzly-ui--meta-lines
                                   entry :show-book show-book
                                   :only-kind only-kind)))))
              :weight 1.0 :padding 4
              :on-tap (if orgzly-ui--select-mode
                          (orgzly-ui--note-action "orgzly.note.select-toggle" ref)
                        (orgzly-ui--note-action "orgzly.note.open" ref))))
            (list :align "center")))))

;; ─── Quick-action popup (Orgzly's note popup) ────────────────────────────────

(defconst orgzly-ui--quick-ops
  '(("state"       "State"       "flag"                "orgzly.note.pick-state")
    ("toggle-done" "Done"        "check_circle"        "orgzly.note.toggle-done")
    ("new-above"   "New above"   "vertical_align_top"  "orgzly.note.new-above")
    ("new-under"   "New under"   "playlist_add"        "orgzly.note.new-under")
    ("new-below"   "New below"   "vertical_align_bottom" "orgzly.note.new-below")
    ("refile"      "Refile"      "move_to_inbox"       "orgzly.note.refile")
    ("archive"     "Archive"     "archive"             "orgzly.note.archive")
    ("focus"       "Focus"       "center_focus_strong" "orgzly.note.focus")
    ("narrow"      "Narrow"      "crop_free"           "orgzly.note.narrow")
    ("clock-in"    "Clock in"    "play_circle"         "orgzly.note.clock-in")
    ("clock-out"   "Clock out"   "stop_circle"         "orgzly.note.clock-out")
    ("clock-cancel" "Cancel clock" "cancel"            "orgzly.note.clock-cancel")
    ("cut"         "Cut"         "content_cut"         "orgzly.note.cut")
    ("copy"        "Copy"        "content_copy"        "orgzly.note.copy")
    ("paste"       "Paste below" "content_paste"       "orgzly.note.paste")
    ("delete"      "Delete"      "delete"              "orgzly.note.delete"))
  "(OP LABEL ICON ACTION) rows of the quick popup, in Orgzly's popup order.")

(defun orgzly-ui--quick-dialog (ref)
  "The quick-action popup spec for REF — Orgzly's swipe note popup."
  (let ((btn (lambda (op)
               (pcase-let ((`(,name ,label ,icon ,_) (assoc op orgzly-ui--quick-ops)))
                 (jetpacs-button label
                              (jetpacs-action "orgzly.note.quick-op"
                                           :args (append ref `((op . ,name)))
                                           :when-offline "drop")
                              :icon icon :variant "text")))))
    (jetpacs-lazy-column
     (jetpacs-row
      (jetpacs-box (list (orgzly-ui--quick-title ref)) :weight 1.0)
      (jetpacs-icon-button "close" (jetpacs-action "dialog.dismiss")
                        :content-description "Close")
      :align "center")
     (jetpacs-flow-row
      (jetpacs-date-button "Schedule"
                        (jetpacs-action "orgzly.note.quick-plan"
                                     :args (append ref '((kind . "scheduled")))
                                     :when-offline "drop"))
      (jetpacs-date-button "Deadline"
                        (jetpacs-action "orgzly.note.quick-plan"
                                     :args (append ref '((kind . "deadline")))
                                     :when-offline "drop"))
      (funcall btn "state")
      (funcall btn "toggle-done"))
     (jetpacs-divider)
     (apply #'jetpacs-flow-row
            (mapcar btn '("new-above" "new-under" "new-below")))
     (apply #'jetpacs-flow-row
            (mapcar btn '("refile" "archive" "focus" "narrow")))
     (jetpacs-divider)
     (apply #'jetpacs-flow-row
            (mapcar btn '("clock-in" "clock-out" "clock-cancel")))
     (jetpacs-divider)
     (apply #'jetpacs-flow-row
            (mapcar btn '("cut" "copy" "paste" "delete"))))))

(defun orgzly-ui--quick-title (ref)
  (jetpacs-rich-text
   (list (jetpacs-span (or (alist-get 'title ref) "") :bold t))
   :style 'body))

(jetpacs-defaction "orgzly.note.quick"
  ;; Swipe or long-press on a note row: show the quick-action popup.
  (lambda (args _)
    (jetpacs-send-dialog (orgzly-ui--quick-dialog (orgzly-ui--args-ref args)))))

(jetpacs-defaction "orgzly.note.quick-op"
  ;; A popup button: dismiss the popup, then run the underlying action.
  (lambda (args payload)
    (jetpacs-dismiss-dialog)
    (when-let* ((op (assoc (alist-get 'op args) orgzly-ui--quick-ops))
                (handler (gethash (nth 3 op) jetpacs-action-handlers)))
      (funcall handler args payload))))

(jetpacs-defaction "orgzly.note.quick-plan"
  ;; The popup's Schedule/Deadline date pick: dismiss, apply the date.
  (lambda (args payload)
    (jetpacs-dismiss-dialog)
    (funcall (gethash "orgzly.note.plan-date" jetpacs-action-handlers)
             args payload)))

;; ─── Books view ──────────────────────────────────────────────────────────────

(defun orgzly-ui--book-menu (name)
  (jetpacs-menu
   (list
    (jetpacs-menu-item "New note" (jetpacs-action "orgzly.note.new"
                                            :args `((book . ,name)))
                    :icon "add")
    (jetpacs-menu-item "Paste note" (jetpacs-action "orgzly.note.paste"
                                              :args `((book . ,name)))
                    :icon "content_paste")
    (jetpacs-menu-item "Edit preface" (jetpacs-action "orgzly.book.preface"
                                                :args `((book . ,name))
                                                :when-offline "drop")
                    :icon "notes")
    (jetpacs-menu-item "File tags…" (jetpacs-action "orgzly.book.edit-filetags"
                                              :args `((book . ,name))
                                              :when-offline "drop")
                    :icon "label")
    (jetpacs-menu-item "Rename…" (jetpacs-action "orgzly.book.rename"
                                           :args `((book . ,name)))
                    :icon "edit")
    (jetpacs-menu-item "Make default" (jetpacs-action "orgzly.book.set-default"
                                                :args `((book . ,name)))
                    :icon "star")
    (jetpacs-menu-item "Delete…" (jetpacs-action "orgzly.book.delete"
                                           :args `((book . ,name)))
                    :icon "delete"))))

(defun orgzly-ui--book-card (b)
  "One notebook as Orgzly's book card: title plus icon-led detail lines.
A book with a #+TITLE shows it as the headline and the filename as a
muted subtitle, like Orgzly's item_book."
  (let* ((name (alist-get 'name b))
         (title (alist-get 'title b)))
    (jetpacs-card
     (list
      (jetpacs-row
       (jetpacs-box
        (list
         (apply
          #'jetpacs-column
          (delq nil
                (list
          (jetpacs-rich-text
           (append
            (list (jetpacs-span (or title name) :bold t))
            (when (equal name orgzly-default-book)
              (list (jetpacs-span "  ★" :color orgzly-ui--muted-color))))
           :style 'title)
          (when title
            (jetpacs-text name 'caption nil orgzly-ui--muted-color))
          (jetpacs-spacer :height 6)
          (orgzly-ui--icon-line
           "access_time"
           (format-time-string "%b %-d %H:%M"
                               (seconds-to-time (alist-get 'mtime b))))
          (orgzly-ui--icon-line
           "format_list_bulleted"
           (format "%d note%s" (alist-get 'count b)
                   (if (= (alist-get 'count b) 1) "" "s")))))))
        :weight 1.0
        :on-tap (jetpacs-action "orgzly.book.open"
                             :args `((book . ,name))
                             :when-offline "drop"))
       (orgzly-ui--book-menu name)
       :align "center")))))

(defun orgzly-ui--books-body ()
  (let ((books (orgzly-data-books)))
    (if (null books)
        (jetpacs-column
         (jetpacs-empty-state :icon "library_books" :title "No notebooks"
                           :caption (format "Create your first notebook in %s"
                                            orgzly-directory)
                           :on-tap (jetpacs-action "orgzly.book.new")
                           :action-label "New notebook")
         (jetpacs-button "Import Getting Started"
                      (jetpacs-action "orgzly.book.import-getting-started"
                                   :when-offline "drop")
                      :variant "text" :icon "school")
         :align "center")
      (apply #'jetpacs-lazy-column (mapcar #'orgzly-ui--book-card books)))))

;; ─── Book (note list) view ───────────────────────────────────────────────────

(defun orgzly-ui--forest-of (entries min-level)
  "Recursive forest builder over document-ordered ENTRIES."
  (let (forest)
    (while entries
      (let* ((e (car entries))
             (level (alist-get 'level e)))
        (if (< level min-level)
            (setq entries nil)
          (let ((rest (cdr entries)) children)
            ;; Children: the run of following entries deeper than E.
            (let ((sub nil))
              (while (and rest (> (alist-get 'level (car rest)) level))
                (push (car rest) sub)
                (setq rest (cdr rest)))
              (setq children (orgzly-ui--forest-of (nreverse sub) (1+ level))))
            (push (cons e children) forest)
            (setq entries rest)))))
    (nreverse forest)))

(defun orgzly-ui--note-node (node)
  "NODE (ENTRY . CHILDREN) as one foldable outline element.
Every note is a collapsible: the chevron is the Orgzly bullet/fold
button (▸ leaf or folded, ▾ unfolded), children and the content preview
fold on-device, and swipe or long-press opens the quick-action popup."
  (let* ((entry (car node))
         (children (cdr node))
         (ref (orgzly-data-entry-ref entry))
         (content (orgzly-ui--content-node entry))
         (quick (jetpacs-action "orgzly.note.quick" :args ref
                             :when-offline "drop")))
    (let ((node (jetpacs-collapsible
                 (format "orgzly-fold:%d:%s:%s" orgzly-ui--fold-generation
                         (alist-get 'file entry) (alist-get 'pos entry))
                 (orgzly-ui--note-row entry
                                      :hide-content (not orgzly-display-content))
                 (delq nil (cons content (mapcar #'orgzly-ui--note-node children)))
                 :collapsed (or orgzly-ui--fold-all
                                (and (null children) (null content)))
                 :on-long-tap quick
                 :on-swipe quick)))
      (if (and orgzly-ui--focus-ref
               (equal (alist-get 'file entry)
                      (alist-get 'file orgzly-ui--focus-ref))
               (eql (alist-get 'pos entry)
                    (alist-get 'pos orgzly-ui--focus-ref)))
          (jetpacs-scroll-here node)
        node))))

(defun orgzly-ui--quick-row (entry header)
  "HEADER wrapped so swipe or long-press opens ENTRY's quick popup.
Search and agenda hits ride the same collapsible furniture as the book
outline (a leaf's static chevron) — that's where the swipe and
long-press gestures live, so query results get Orgzly's popup ops
without new core primitives."
  (let ((quick (jetpacs-action "orgzly.note.quick"
                            :args (orgzly-data-entry-ref entry)
                            :when-offline "drop")))
    (jetpacs-collapsible
     (format "orgzly-hit:%s:%s" (alist-get 'file entry) (alist-get 'pos entry))
     header nil
     :collapsed t
     :on-long-tap quick
     :on-swipe quick)))

(defun orgzly-ui--selection-bar ()
  "Orgzly's multi-select action bar, shown while selecting.
Second row: the structure ops of upstream's \"move\" CAB submode, plus
batch cut/copy."
  (jetpacs-surface
   (list
    (jetpacs-scroll-row
     (jetpacs-button "State" (jetpacs-action "orgzly.bulk.state") :variant "text"
                  :icon "flag")
     (jetpacs-button "Done" (jetpacs-action "orgzly.bulk.toggle-done")
                  :variant "text" :icon "check_circle")
     (jetpacs-date-button "Schedule"
                       (jetpacs-action "orgzly.bulk.plan"
                                    :args '((kind . "scheduled"))))
     (jetpacs-date-button "Deadline"
                       (jetpacs-action "orgzly.bulk.plan"
                                    :args '((kind . "deadline"))))
     (jetpacs-button "Refile" (jetpacs-action "orgzly.bulk.refile") :variant "text"
                  :icon "move_to_inbox")
     (jetpacs-button "Archive" (jetpacs-action "orgzly.bulk.archive")
                  :variant "text" :icon "archive")
     (jetpacs-button "Delete" (jetpacs-action "orgzly.bulk.delete")
                  :variant "text" :icon "delete"))
    (jetpacs-scroll-row
     (jetpacs-button "Promote" (jetpacs-action "orgzly.bulk.promote")
                  :variant "text" :icon "chevron_left")
     (jetpacs-button "Demote" (jetpacs-action "orgzly.bulk.demote")
                  :variant "text" :icon "chevron_right")
     (jetpacs-button "Up" (jetpacs-action "orgzly.bulk.move"
                                    :args '((dir . "up")))
                  :variant "text" :icon "arrow_upward")
     (jetpacs-button "Down" (jetpacs-action "orgzly.bulk.move"
                                      :args '((dir . "down")))
                  :variant "text" :icon "arrow_downward")
     (jetpacs-button "Cut" (jetpacs-action "orgzly.bulk.cut")
                  :variant "text" :icon "content_cut")
     (jetpacs-button "Copy" (jetpacs-action "orgzly.bulk.copy")
                  :variant "text" :icon "content_copy")
     (jetpacs-button "Clock in" (jetpacs-action "orgzly.bulk.clock-in")
                  :variant "text" :icon "play_circle")
     (jetpacs-button "Clock out" (jetpacs-action "orgzly.bulk.clock-out")
                  :variant "text" :icon "stop_circle")
     (jetpacs-button "Cancel clock"
                  (jetpacs-action "orgzly.bulk.clock-cancel")
                  :variant "text" :icon "cancel")))
   :color "surface_container" :shape "rounded" :padding 4 :fill t))

(defun orgzly-ui--preface-node (book)
  "BOOK's preface abstracted the way the note editor abstracts metadata:
the #+TITLE as a title line, CREATED and FILETAGS as icon-led lines,
other in-buffer settings as one muted keyword summary, and only the
non-keyword prose rendered as org text.  Tap opens the preface editor."
  (when-let ((fm (orgzly-data-book-front-matter book)))
    (let ((title (alist-get 'title fm))
          (created (alist-get 'created fm))
          (filetags (alist-get 'filetags fm))
          (settings (alist-get 'settings fm))
          (body (alist-get 'body fm)))
      (jetpacs-box
       (list
        (apply #'jetpacs-column
               (append
                (delq nil
                      (list
                       (when title
                         (jetpacs-rich-text
                          (list (jetpacs-span title :bold t))
                          :style 'title))
                       (when created
                         (orgzly-ui--icon-line
                          "access_time"
                          (concat "Created " (orgzly-ui--ts-label created))))
                       (when filetags
                         (orgzly-ui--icon-line
                          "label" (string-join filetags " ")))
                       (when settings
                         (orgzly-ui--icon-line
                          "tune" (mapconcat #'car settings ", ")))
                       (when body
                         (jetpacs-markup body :syntax "org"
                                      :style 'caption))))
                '(:spacing 4))))
       :padding 8
       :on-tap (jetpacs-action "orgzly.book.preface"
                            :args `((book . ,book))
                            :when-offline "drop")))))

(defun orgzly-ui--narrowed-entries (entries ref)
  "REF's subtree within document-ordered ENTRIES, or nil when gone.
Falls back to matching by title when positions have drifted."
  (when-let ((idx (or (cl-position-if
                       (lambda (e)
                         (and (equal (alist-get 'file e) (alist-get 'file ref))
                              (eql (alist-get 'pos e) (alist-get 'pos ref))))
                       entries)
                      (cl-position-if
                       (lambda (e)
                         (and (equal (alist-get 'file e) (alist-get 'file ref))
                              (equal (alist-get 'title e)
                                     (alist-get 'title ref))))
                       entries))))
    (let* ((root (nth idx entries))
           (level (alist-get 'level root)))
      (cons root
            (cl-loop for e in (nthcdr (1+ idx) entries)
                     while (> (alist-get 'level e) level)
                     collect e)))))

(defun orgzly-ui--book-body ()
  (let* ((book orgzly-ui--current-book)
         (entries (orgzly-data-entries book))
         (narrowed (and orgzly-ui--narrow-ref
                        (orgzly-ui--narrowed-entries
                         entries orgzly-ui--narrow-ref))))
    (apply #'jetpacs-lazy-column
           (delq nil
                 (append
                  (when orgzly-ui--select-mode
                    (list (orgzly-ui--selection-bar)))
                  ;; Narrowed: just the subtree, no preface (upstream's
                  ;; narrow-to-subtree); the top bar carries the widen.
                  (unless narrowed
                    (list (orgzly-ui--preface-node book)))
                  (or (mapcar #'orgzly-ui--note-node
                              (orgzly-ui--forest-of
                               (or narrowed entries)
                               (if narrowed
                                   (alist-get 'level (car narrowed))
                                 1)))
                      (list (jetpacs-empty-state
                             :icon "note_add" :title "No notes"
                             :caption "Tap + to add the first note"
                             :on-tap (jetpacs-action "orgzly.note.new"
                                                  :args `((book . ,book)))
                             :action-label "New note"))))))))

(defun orgzly-ui--book-view (snackbar)
  (jetpacs-shell-nav-view
   (if orgzly-ui--select-mode
       (format "%d selected" (length orgzly-ui--selection))
     (or orgzly-ui--current-book "Book"))
   (orgzly-ui--book-body)
   :back-to "orgzly.books"
   :actions (delq nil
                  (list
                   (when orgzly-ui--narrow-ref
                     (jetpacs-icon-button
                      "zoom_out_map"
                      (jetpacs-action "orgzly.book.widen" :when-offline "drop")
                      :content-description "Widen view"))
                   (jetpacs-icon-button
                    (if orgzly-ui--fold-all "unfold_more" "unfold_less")
                    (jetpacs-action "orgzly.book.cycle-visibility"
                                 :when-offline "drop")
                    :content-description "Cycle visibility")
                   (jetpacs-icon-button "search"
                                     (jetpacs-shell-switch-view "orgzly.search")
                                     :content-description "Search")
                   (jetpacs-icon-button (if orgzly-ui--select-mode
                                            "close" "checklist")
                                     (jetpacs-action "orgzly.book.select-mode"
                                                  :when-offline "drop")
                                     :content-description "Select notes")))
   :fab (unless orgzly-ui--select-mode
          (jetpacs-fab "add" :on-tap (jetpacs-action
                                   "orgzly.note.new"
                                   :args `((book . ,orgzly-ui--current-book)))))
   :snackbar snackbar))

;; ─── Note editor view ────────────────────────────────────────────────────────

(defun orgzly-ui--breadcrumbs (entry)
  "The Orgzly breadcrumbs: book › ancestors, each crumb tappable."
  (let* ((book (alist-get 'book entry))
         (entries (orgzly-data-entries book))
         (idx (cl-position-if
               (lambda (e) (and (equal (alist-get 'file e)
                                       (alist-get 'file entry))
                                (eql (alist-get 'pos e)
                                     (alist-get 'pos entry))))
               entries))
         (level (alist-get 'level entry))
         (crumbs nil))
    (when idx
      (let ((i (1- idx)))
        (while (and (>= i 0) (> level 1))
          (let ((e (nth i entries)))
            (when (< (alist-get 'level e) level)
              (push e crumbs)
              (setq level (alist-get 'level e))))
          (setq i (1- i)))))
    (jetpacs-box
     (list
      (jetpacs-rich-text
       (cons
        (jetpacs-span book :bold t
                   :on-tap (jetpacs-action "orgzly.book.open"
                                        :args `((book . ,book))
                                        :when-offline "drop"))
        (cl-loop for c in crumbs append
                 (list (jetpacs-span "  ›  " :color orgzly-ui--muted-color)
                       (jetpacs-span (alist-get 'title c)
                                  :on-tap (orgzly-ui--note-action
                                           "orgzly.note.open"
                                           (orgzly-data-entry-ref c))))))
       :style 'caption))
     :padding 8)))

(cl-defun orgzly-ui--meta-row (icon value hint &key on-tap on-clear)
  "One editor metadata row in Orgzly's note-fragment idiom:
leading ICON, the VALUE (or the muted HINT when empty), × to clear."
  (apply #'jetpacs-row
         (append
          (list
           (jetpacs-icon icon :size 20 :color "outline")
           (jetpacs-box
            (list (jetpacs-rich-text
                   (list (if value (jetpacs-span value)
                           (jetpacs-span hint :color orgzly-ui--muted-color)))
                   :style 'body))
            :weight 1.0 :padding 8 :on-tap on-tap))
          (when (and value on-clear)
            (list (jetpacs-icon-button
                   "close" on-clear
                   :content-description (concat "Clear " (downcase hint)))))
          (list :spacing 8 :align "center"))))

(defun orgzly-ui--properties-nodes (entry)
  (let* ((ref (orgzly-data-entry-ref entry))
         (props (ignore-errors (orgzly-data-properties ref))))
    (append
     (mapcar (lambda (p)
               (jetpacs-row
                (jetpacs-rich-text
                 (list (jetpacs-span (car p) :color orgzly-ui--muted-color))
                 :style 'label)
                (jetpacs-spacer :width 8)
                (jetpacs-text (cdr p) 'body nil nil t)
                (jetpacs-spacer :weight 1.0)
                (jetpacs-icon-button
                 "close"
                 (orgzly-ui--note-action "orgzly.note.del-property" ref
                                         (cons 'name (car p)))
                 :content-description "Delete property")
                :align "center"))
             props)
     (list (jetpacs-button "Add property"
                        (orgzly-ui--note-action "orgzly.note.add-property" ref)
                        :icon "add" :variant "text")))))

(defun orgzly-ui--note-body ()
  (let ((entry (orgzly-ui--entry-for-ref orgzly-ui--note-ref)))
    (if (null entry)
        (jetpacs-empty-state :icon "error" :title "Note not found"
                          :caption "It may have been moved or deleted")
      (let* ((ref (orgzly-data-entry-ref entry))
             (state (alist-get 'state entry))
             (priority (alist-get 'priority entry))
             (tags (mapcar #'substring-no-properties (alist-get 'tags entry)))
             (scheduled (alist-get 'scheduled entry))
             (deadline (alist-get 'deadline entry))
             (closed (alist-get 'closed entry)))
        (apply
         #'jetpacs-lazy-column
         (delq nil
               (list
                (orgzly-ui--breadcrumbs entry)
                (orgzly-ui--meta-row "folder_open" (alist-get 'book entry)
                                     "Notebook"
                                     :on-tap (orgzly-ui--note-action
                                              "orgzly.note.refile" ref))
                (jetpacs-text-input
                 (format "orgzly-title:%s:%s" (alist-get 'file entry)
                         (alist-get 'pos entry))
                 :value (alist-get 'title entry)
                 :hint "Title"
                 :on-submit (orgzly-ui--note-action "orgzly.note.rename" ref)
                 :single-line t)
                (jetpacs-divider)
                (orgzly-ui--meta-row
                 "label" (and tags (string-join tags " ")) "Tags"
                 :on-tap (orgzly-ui--note-action "orgzly.note.edit-tags" ref)
                 :on-clear (jetpacs-action "orgzly.note.set-tags"
                                        :args (append ref '((value . [])))))
                (orgzly-ui--meta-row
                 "flag" state "State"
                 :on-tap (orgzly-ui--note-action "orgzly.note.pick-state" ref)
                 :on-clear (orgzly-ui--note-action "orgzly.note.set-state" ref
                                                   '(state . "")))
                (orgzly-ui--meta-row
                 "star_border" (and priority (format "Priority %s" priority))
                 "Priority"
                 :on-tap (orgzly-ui--note-action "orgzly.note.pick-priority" ref)
                 :on-clear (orgzly-ui--note-action "orgzly.note.set-priority" ref
                                                   '(priority . "")))
                (orgzly-ui--meta-row
                 "today" (and scheduled (orgzly-ui--ts-label scheduled))
                 "Schedule"
                 :on-tap (orgzly-ui--note-action "orgzly.note.plan-dialog" ref
                                                 '(kind . "scheduled"))
                 :on-clear (orgzly-ui--note-action "orgzly.note.plan-clear" ref
                                                   '(kind . "scheduled")))
                (orgzly-ui--meta-row
                 "alarm" (and deadline (orgzly-ui--ts-label deadline))
                 "Deadline"
                 :on-tap (orgzly-ui--note-action "orgzly.note.plan-dialog" ref
                                                 '(kind . "deadline"))
                 :on-clear (orgzly-ui--note-action "orgzly.note.plan-clear" ref
                                                   '(kind . "deadline")))
                (when closed
                  (orgzly-ui--meta-row
                   "task_alt" (orgzly-ui--ts-label closed) "Closed"
                   :on-tap (orgzly-ui--note-action "orgzly.note.plan-dialog"
                                                   ref '(kind . "closed"))
                   :on-clear (orgzly-ui--note-action "orgzly.note.plan-clear"
                                                     ref '(kind . "closed"))))
                (jetpacs-divider)
                (apply #'jetpacs-column
                       (orgzly-ui--properties-nodes entry))
                (jetpacs-divider)
                (jetpacs-editor (format "orgzly:%s:%s" (alist-get 'file entry)
                                     (alist-get 'pos entry))
                             (alist-get 'content entry)
                             :on-save (orgzly-ui--note-action
                                       "orgzly.note.set-content" ref)
                             :syntax "org" :toolbar "org" :chromeless t))))))))

(defun orgzly-ui--note-menu (entry-or-ref)
  "The note editor's overflow menu: structure and clipboard operations."
  (let ((ref (if (alist-get 'book entry-or-ref)
                 (orgzly-data-entry-ref entry-or-ref)
               entry-or-ref)))
    (jetpacs-menu
     (list
      (jetpacs-menu-item "New note under"
                      (orgzly-ui--note-action "orgzly.note.new-under" ref)
                      :icon "playlist_add")
      (jetpacs-menu-item "Refile…"
                      (orgzly-ui--note-action "orgzly.note.refile" ref)
                      :icon "move_to_inbox")
      (jetpacs-menu-item "Archive"
                      (orgzly-ui--note-action "orgzly.note.archive" ref)
                      :icon "archive")
      (jetpacs-menu-item "Sort content lines"
                      (orgzly-ui--note-action "orgzly.note.sort-content" ref)
                      :icon "sort_by_alpha")
      (jetpacs-menu-item "Clock in"
                      (orgzly-ui--note-action "orgzly.note.clock-in" ref)
                      :icon "play_circle")
      (jetpacs-menu-item "Clock out"
                      (orgzly-ui--note-action "orgzly.note.clock-out" ref)
                      :icon "stop_circle")
      (jetpacs-menu-item "Cancel clock"
                      (orgzly-ui--note-action "orgzly.note.clock-cancel" ref)
                      :icon "cancel")
      (jetpacs-menu-item "Cut" (orgzly-ui--note-action "orgzly.note.cut" ref)
                      :icon "content_cut")
      (jetpacs-menu-item "Copy" (orgzly-ui--note-action "orgzly.note.copy" ref)
                      :icon "content_copy")
      (jetpacs-menu-item "Paste below"
                      (orgzly-ui--note-action "orgzly.note.paste" ref)
                      :icon "content_paste")
      (jetpacs-menu-item "Promote" (orgzly-ui--note-action "orgzly.note.promote" ref)
                      :icon "format_indent_decrease")
      (jetpacs-menu-item "Demote" (orgzly-ui--note-action "orgzly.note.demote" ref)
                      :icon "format_indent_increase")
      (jetpacs-menu-item "Move up" (orgzly-ui--note-action "orgzly.note.move-up" ref)
                      :icon "arrow_upward")
      (jetpacs-menu-item "Move down"
                      (orgzly-ui--note-action "orgzly.note.move-down" ref)
                      :icon "arrow_downward")
      (jetpacs-menu-item "Delete…" (orgzly-ui--note-action "orgzly.note.delete" ref)
                      :icon "delete")))))

(defun orgzly-ui--note-view (snackbar)
  (let ((ref orgzly-ui--note-ref))
    (jetpacs-shell-nav-view
     "Note"
     (orgzly-ui--note-body)
     :back-to (if orgzly-ui--current-book "orgzly.book" "orgzly.books")
     :actions (when ref
                (list (orgzly-ui--note-menu
                       (or (orgzly-ui--entry-for-ref ref) ref))))
     :snackbar snackbar)))

;; ─── Timestamp dialog (Orgzly's dialog_timestamp) ────────────────────────────

(defun orgzly-ui--plan-parts (ref kind)
  "REF's KIND planning as a parts alist (`orgzly-data-set-planning').
String values under `date', `time', `end', `repeater', `warning';
absent when unset."
  (let* ((entry (orgzly-ui--entry-for-ref ref))
         (ts (and entry (alist-get (intern kind) entry)))
         (time (and ts (alist-get 'time ts)))
         (end (and ts (alist-get 'end-time ts)))
         (repeater (and ts (alist-get 'repeater ts)))
         (warning (and ts (alist-get 'warning ts))))
    (delq nil
          (list (and time (cons 'date (format-time-string
                                       "%Y-%m-%d" (seconds-to-time time))))
                (and time (alist-get 'has-time ts)
                     (cons 'time (format-time-string
                                  "%H:%M" (seconds-to-time time))))
                (and end (cons 'end (format-time-string
                                     "%H:%M" (seconds-to-time end))))
                (and repeater (cons 'repeater repeater))
                (and warning (cons 'warning warning))))))

(defun orgzly-ui--plan-dialog (ref kind)
  "The timestamp dialog spec for REF's KIND planning line.
Orgzly's dialog_timestamp: date, time with an end time (range) once a
time is set, repeater, and the warning/delay cookie; × drops just the
time (keeping the date) or just the end."
  (let* ((parts (orgzly-ui--plan-parts ref kind))
         (date (alist-get 'date parts))
         (time (alist-get 'time parts))
         (end (alist-get 'end parts))
         (args (lambda (&rest extra)
                 (append ref (cons (cons 'kind kind) extra)))))
    (apply
     #'jetpacs-lazy-column
     (delq
      nil
      (list
     (jetpacs-row
      (jetpacs-box
       (list (jetpacs-text (pcase kind
                          ("deadline" "Deadline time")
                          ("closed" "Closed time")
                          (_ "Scheduled time"))
                        'title))
       :weight 1.0)
      (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
      :align "center")
     (apply
      #'jetpacs-row
      (append
       (delq nil
             (list
              (jetpacs-icon (pcase kind
                           ("deadline" "alarm")
                           ("closed" "task_alt")
                           (_ "today"))
                         :size 20 :color "outline")
              (jetpacs-date-button (or date "Date")
                                (jetpacs-action "orgzly.note.plan-date"
                                             :args (funcall args '(dialog . t)))
                                :value date)
              (jetpacs-time-button (or time "Time")
                                (jetpacs-action "orgzly.note.plan-time"
                                             :args (funcall args '(dialog . t)))
                                :value time)
              (when time
                (jetpacs-icon-button "close"
                                  (jetpacs-action "orgzly.note.plan-drop-time"
                                               :args (funcall args '(dialog . t)))
                                  :content-description "Remove time"))))
       '(:spacing 8 :align "center")))
     (when time
       (apply
        #'jetpacs-row
        (append
         (delq nil
               (list
                (jetpacs-icon "schedule" :size 20 :color "outline")
                (jetpacs-time-button (or end "End time")
                                  (jetpacs-action "orgzly.note.plan-end"
                                               :args (funcall args '(dialog . t)))
                                  :value end)
                (when end
                  (jetpacs-icon-button "close"
                                    (jetpacs-action "orgzly.note.plan-drop-end"
                                                 :args (funcall args '(dialog . t)))
                                    :content-description "Remove end time"))))
         '(:spacing 8 :align "center"))))
     ;; Orgzly's date shortcuts.
     (let ((shortcut
            (lambda (label days)
              (jetpacs-button label
                           (jetpacs-action
                            "orgzly.note.plan-date"
                            :args (funcall
                                   args '(dialog . t)
                                   (cons 'value
                                         (format-time-string
                                          "%Y-%m-%d"
                                          (time-add nil (* days 86400))))))
                           :variant "text"))))
       (jetpacs-row
        (funcall shortcut "Today" 0)
        (funcall shortcut "Tomorrow" 1)
        (funcall shortcut "Next week" 7)
        :spacing 8 :align "center"))
     ;; A CLOSED stamp carries no repeater or warning.
     (unless (equal kind "closed")
       (jetpacs-row
        (jetpacs-icon "repeat" :size 20 :color "outline")
        (jetpacs-button (or (alist-get 'repeater parts) "Repeat")
                     (jetpacs-action "orgzly.note.plan-repeater"
                                  :args (funcall args '(dialog . t)))
                     :variant "text")
        (jetpacs-button (or (alist-get 'warning parts) "Warning")
                     (jetpacs-action "orgzly.note.plan-warning"
                                  :args (funcall args '(dialog . t)))
                     :variant "text")
        :spacing 8 :align "center"))
     (jetpacs-row
      (jetpacs-spacer :weight 1.0)
      (jetpacs-button "Clear"
                   (jetpacs-action "orgzly.note.plan-clear"
                                :args (funcall args))
                   :variant "text" :icon "close")))))))

(jetpacs-defaction "orgzly.note.plan-dialog"
  (lambda (args _)
    (jetpacs-send-dialog
     (orgzly-ui--plan-dialog (orgzly-ui--args-ref args)
                             (alist-get 'kind args)))))

;; ─── Tags dialog ─────────────────────────────────────────────────────────────

(defun orgzly-ui--known-tags ()
  "Every tag in use across books, plus configured favourites."
  (let ((tags (make-hash-table :test 'equal)))
    (dolist (e (orgzly-data-entries))
      (dolist (tag (alist-get 'tags e))
        (puthash (substring-no-properties tag) t tags)))
    (dolist (ta org-tag-alist)
      (when (and (consp ta) (stringp (car ta)))
        (puthash (car ta) t tags)))
    (sort (hash-table-keys tags) #'string-lessp)))

(defun orgzly-ui--tags-dialog (ref current)
  "The tag-picker dialog spec for REF with CURRENT tags selected."
  (jetpacs-lazy-column
   (jetpacs-row
    (jetpacs-box (list (jetpacs-text "Tags" 'title)) :weight 1.0)
    (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
    :align "center")
   (jetpacs-enum-list "orgzly-note-tags"
                   (orgzly-ui--known-tags)
                   :value current
                   :multi-select t :allow-add t
                   :on-change (orgzly-ui--note-action "orgzly.note.set-tags" ref))))

(jetpacs-defaction "orgzly.note.edit-tags"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (entry (orgzly-ui--entry-for-ref ref)))
      (jetpacs-send-dialog
       (orgzly-ui--tags-dialog
        ref (mapcar #'substring-no-properties
                    (and entry (alist-get 'tags entry))))))))

;; ─── Preface editor view ─────────────────────────────────────────────────────

(defun orgzly-ui--preface-view (snackbar)
  (let* ((book orgzly-ui--preface-book))
    (jetpacs-shell-nav-view
     (format "%s — preface" book)
     (jetpacs-editor (format "orgzly-preface:%s" book)
                  (or (orgzly-data-book-preface book) "")
                  :on-save (jetpacs-action "orgzly.book.set-preface"
                                        :args `((book . ,book)))
                  :syntax "org" :toolbar "org")
     :back-to "orgzly.book"
     :snackbar snackbar)))

;; ─── View registrations ──────────────────────────────────────────────────────

(jetpacs-shell-define-view "orgzly.books"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "orgzly.books" (orgzly-ui--books-body)
                                  :top-bar
                                  (jetpacs-shell-default-top-bar
                                   "Books"
                                   :extra-actions
                                   ;; The top bar renders icon buttons only —
                                   ;; the import lives here because the schema
                                   ;; settings screen has no action rows
                                   ;; (upstream keeps it in Settings > App).
                                   (list (jetpacs-icon-button
                                          "school"
                                          (jetpacs-action
                                           "orgzly.book.import-getting-started"
                                           :when-offline "drop")
                                          :content-description
                                          "Import Getting Started")))
                                  :snackbar snackbar
                                  :fab (jetpacs-fab "create_new_folder"
                                                 :on-tap (jetpacs-action
                                                          "orgzly.book.new"))))
  :tab '(:icon "library_books" :label "Books")
  :order 10)

(jetpacs-shell-define-view "orgzly.note"
  :builder #'orgzly-ui--note-view
  :overlay (lambda () orgzly-ui--note-ref)
  :order 21)

(jetpacs-shell-define-view "orgzly.preface"
  :builder #'orgzly-ui--preface-view
  :overlay (lambda () orgzly-ui--preface-book)
  :order 22)

(jetpacs-shell-define-view "orgzly.book"
  :builder #'orgzly-ui--book-view
  :overlay (lambda () orgzly-ui--current-book)
  :order 23)

;; ─── Actions: books ──────────────────────────────────────────────────────────

(jetpacs-defaction "orgzly.book.open"
  (lambda (args _)
    (setq orgzly-ui--current-book (alist-get 'book args)
          orgzly-ui--note-ref nil
          orgzly-ui--preface-book nil
          orgzly-ui--narrow-ref nil)
    (jetpacs-shell-push nil :switch-to "orgzly.book")))

(jetpacs-defaction "orgzly.note.narrow"
  ;; Upstream's narrow-to-subtree: the book view keeps just this
  ;; subtree until widened.
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (setq orgzly-ui--current-book (orgzly-data-book-name
                                     (alist-get 'file ref))
            orgzly-ui--note-ref nil
            orgzly-ui--narrow-ref ref)
      (jetpacs-shell-push nil :switch-to "orgzly.book"))))

(jetpacs-defaction "orgzly.book.widen"
  (lambda (_ _)
    (setq orgzly-ui--narrow-ref nil)
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.book.cycle-visibility"
  ;; Fold state lives on-device keyed by collapsible id; bumping the
  ;; generation renames every id, so the pushed :collapsed defaults
  ;; take effect (fold all ↔ unfold all).
  (lambda (_ _)
    (setq orgzly-ui--fold-all (not orgzly-ui--fold-all)
          orgzly-ui--fold-generation (1+ orgzly-ui--fold-generation))
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.book.new"
  (lambda (_ _)
    (let ((name (string-trim (read-string "Notebook name: "))))
      (unless (string-empty-p name)
        (orgzly-data-create-book name)
        (jetpacs-shell-notify (format "Created %s" name))
        (jetpacs-shell-push)))))

(defun orgzly-ui--filetags-dialog (book current)
  "Tag-picker dialog for BOOK's #+FILETAGS: with CURRENT selected."
  (jetpacs-lazy-column
   (jetpacs-row
    (jetpacs-box (list (jetpacs-text "File tags" 'title)) :weight 1.0)
    (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
    :align "center")
   (jetpacs-enum-list "orgzly-book-filetags"
                   (orgzly-ui--known-tags)
                   :value current
                   :multi-select t :allow-add t
                   :on-change (jetpacs-action "orgzly.book.set-filetags"
                                           :args `((book . ,book))))))

(jetpacs-defaction "orgzly.book.edit-filetags"
  ;; Orgzly's book "File tags" dialog — the tags every note in the
  ;; book inherits (t. searches see them).
  (lambda (args _)
    (let ((book (alist-get 'book args)))
      (jetpacs-send-dialog
       (orgzly-ui--filetags-dialog
        book (alist-get 'filetags (orgzly-data-book-front-matter book)))))))

(jetpacs-defaction "orgzly.book.set-filetags"
  (lambda (args _)
    (orgzly-data-set-book-filetags (alist-get 'book args)
                                   (append (alist-get 'value args) nil))
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.book.import-getting-started"
  ;; Orgzly's "Import Getting Started with Orgzly" (Settings > App):
  ;; create the tutorial notebook; confirm before resetting an existing
  ;; copy (upstream re-imports silently — but a stray top-bar tap here
  ;; shouldn't discard edits).
  (lambda (_ _)
    (let ((book orgzly-data-getting-started-book))
      (when (or (not (file-exists-p (orgzly-data-book-file book)))
                (y-or-n-p (format "%s exists — replace it? " book)))
        (orgzly-data-import-getting-started)
        (jetpacs-shell-notify (format "Imported %s" book))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.rename"
  (lambda (args _)
    (let* ((book (alist-get 'book args))
           (new (string-trim (read-string (format "Rename %s to: " book) book))))
      (unless (or (string-empty-p new) (equal new book))
        (orgzly-data-rename-book book new)
        (when (equal orgzly-ui--current-book book)
          (setq orgzly-ui--current-book new))
        (jetpacs-shell-notify (format "Renamed to %s" new))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.delete"
  (lambda (args _)
    (let ((book (alist-get 'book args)))
      (when (y-or-n-p (format "Delete notebook %s and all its notes? " book))
        (orgzly-data-delete-book book)
        (when (equal orgzly-ui--current-book book)
          (orgzly-ui--reset-drill-in))
        (jetpacs-shell-notify (format "Deleted %s" book))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.book.set-default"
  (lambda (args _)
    (let ((book (alist-get 'book args)))
      (customize-save-variable 'orgzly-default-book book)
      (jetpacs-shell-notify (format "%s is now the default notebook" book))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.book.select-mode"
  (lambda (_ _)
    (setq orgzly-ui--select-mode (not orgzly-ui--select-mode)
          orgzly-ui--selection nil)
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.book.preface"
  (lambda (args _)
    (setq orgzly-ui--preface-book (alist-get 'book args))
    (jetpacs-shell-push nil :switch-to "orgzly.preface")))

(jetpacs-defaction "orgzly.book.set-preface"
  (lambda (args _)
    (let* ((book (alist-get 'book args))
           (text (alist-get 'value args))
           (file (orgzly-data-book-file book)))
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (goto-char (point-min))
         (let ((end (if (re-search-forward org-outline-regexp-bol nil t)
                        (match-beginning 0)
                      (point-max))))
           (delete-region (point-min) end)
           (goto-char (point-min))
           (let ((clean (string-trim (or text ""))))
             (unless (string-empty-p clean)
               (insert clean "\n\n"))))
         ;; The preface holds #+ keywords (FILETAGS, TODO…); re-read
         ;; them so the live buffer's org state sees the edit.
         (org-set-regexps-and-options))
        (save-buffer))
      (orgzly-data-invalidate)
      (setq orgzly-ui--preface-book nil)
      (jetpacs-shell-notify "Preface saved")
      (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book "orgzly.book" "orgzly.books")))))

;; ─── Actions: single note ────────────────────────────────────────────────────

(defun orgzly-ui--after-mutation (message &optional ref)
  "Common tail of a note mutation: refresh the note ref, notify, push."
  (when ref (setq orgzly-ui--note-ref ref))
  (when message (jetpacs-shell-notify message))
  (jetpacs-shell-push))

(jetpacs-defaction "orgzly.note.open"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (setq orgzly-ui--note-ref ref)
      (unless orgzly-ui--current-book
        (setq orgzly-ui--current-book
              (orgzly-data-book-name (alist-get 'file ref))))
      (jetpacs-shell-push nil :switch-to "orgzly.note"))))

(jetpacs-defaction "orgzly.note.new"
  (lambda (args _)
    (let* ((book (or (alist-get 'book args)
                     orgzly-ui--current-book orgzly-default-book))
           (title (string-trim (read-string "Note title: "))))
      (unless (string-empty-p title)
        (let ((ref (orgzly-data-new-note book title)))
          (setq orgzly-ui--current-book book
                orgzly-ui--note-ref ref)
          (jetpacs-shell-notify "Note created")
          (jetpacs-shell-push nil :switch-to "orgzly.note"))))))

(jetpacs-defaction "orgzly.note.new-under"
  (lambda (args _)
    (let* ((parent (orgzly-ui--args-ref args))
           (book (orgzly-data-book-name (alist-get 'file parent)))
           (title (string-trim (read-string "Note title: "))))
      (unless (string-empty-p title)
        (let ((ref (orgzly-data-new-note book title :under parent)))
          (setq orgzly-ui--note-ref ref)
          (jetpacs-shell-notify "Note created")
          (jetpacs-shell-push nil :switch-to "orgzly.note"))))))

(defun orgzly-ui--new-sibling (args before)
  "Create a same-level sibling of the note in ARGS (above when BEFORE)."
  (let* ((sib (orgzly-ui--args-ref args))
         (book (orgzly-data-book-name (alist-get 'file sib)))
         (title (string-trim (read-string "Note title: "))))
    (unless (string-empty-p title)
      (let ((ref (orgzly-data-new-note book title
                                       :sibling sib :before before)))
        (setq orgzly-ui--note-ref ref)
        (jetpacs-shell-notify "Note created")
        (jetpacs-shell-push nil :switch-to "orgzly.note")))))

(jetpacs-defaction "orgzly.note.new-above"
  (lambda (args _) (orgzly-ui--new-sibling args t)))

(jetpacs-defaction "orgzly.note.new-below"
  (lambda (args _) (orgzly-ui--new-sibling args nil)))

(jetpacs-defaction "orgzly.note.rename"
  ;; From the editor's inline title field (args carry `value') or a
  ;; bridged prompt.
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (new (or (alist-get 'value args)
                    (read-string "Title: " (alist-get 'title ref)))))
      (unless (string-empty-p (string-trim new))
        (orgzly-data-set-title ref new)
        (orgzly-ui--after-mutation "Renamed"
                                   (append `((title . ,new))
                                           (assq-delete-all 'title ref)))))))

(jetpacs-defaction "orgzly.note.set-state"
  (lambda (args _)
    (orgzly-data-set-state (orgzly-ui--args-ref args) (alist-get 'state args))
    (orgzly-ui--after-mutation nil)))

(jetpacs-defaction "orgzly.note.toggle-done"
  (lambda (args _)
    (orgzly-data-toggle-done (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation nil)))

(jetpacs-defaction "orgzly.note.pick-state"
  (lambda (args _)
    (let* ((kws (orgzly-data-todo-keywords))
           (state (completing-read "State: "
                                   (append '("NONE") (car kws) (cdr kws))
                                   nil t)))
      (orgzly-data-set-state (orgzly-ui--args-ref args)
                             (unless (equal state "NONE") state))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-priority"
  (lambda (args _)
    (let ((p (alist-get 'priority args)))
      (orgzly-data-set-priority (orgzly-ui--args-ref args)
                                (unless (or (null p) (string-empty-p p)) p))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.pick-priority"
  (lambda (args _)
    (let* ((letters (cl-loop for c from org-priority-highest
                             to org-priority-lowest
                             collect (char-to-string c)))
           (p (completing-read "Priority: " (cons "None" letters) nil t)))
      (orgzly-data-set-priority (orgzly-ui--args-ref args)
                                (unless (equal p "None") p))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-tags"
  (lambda (args _)
    (let ((tags (alist-get 'value args)))
      (orgzly-data-set-tags (orgzly-ui--args-ref args)
                            (append tags nil))
      (orgzly-ui--after-mutation nil))))

(jetpacs-defaction "orgzly.note.set-content"
  (lambda (args _)
    (orgzly-data-set-content (orgzly-ui--args-ref args)
                             (or (alist-get 'value args) ""))
    (orgzly-ui--after-mutation "Saved")))

;; Planning editors.  The timestamp dialog's date/time buttons inject the
;; picked value into args as `value'; repeater goes through a bridged
;; prompt.  Actions carrying (dialog . t) re-show the dialog with the
;; updated values, so it edits in place like Orgzly's.

(defun orgzly-ui--plan-apply (ref kind parts &optional dialog)
  "Write the PARTS alist onto REF's KIND planning; nil PARTS removes."
  (orgzly-data-set-planning ref (intern kind)
                            (and (alist-get 'date parts) parts))
  (when dialog
    (jetpacs-send-dialog (orgzly-ui--plan-dialog ref kind)))
  (orgzly-ui--after-mutation nil))

(defun orgzly-ui--plan-update (args field value)
  "Re-apply the planning in ARGS with FIELD set to VALUE (nil removes)."
  (let* ((ref (orgzly-ui--args-ref args))
         (kind (alist-get 'kind args))
         (parts (assq-delete-all field (orgzly-ui--plan-parts ref kind))))
    (when value (push (cons field value) parts))
    ;; An end time only makes sense on a timed stamp, and any edit
    ;; before a date exists implies today (Orgzly's dialog defaults).
    (unless (alist-get 'time parts)
      (setq parts (assq-delete-all 'end parts)))
    (unless (alist-get 'date parts)
      (push (cons 'date (format-time-string "%Y-%m-%d")) parts))
    (orgzly-ui--plan-apply ref kind parts (alist-get 'dialog args))))

(jetpacs-defaction "orgzly.note.plan-date"
  (lambda (args _)
    (orgzly-ui--plan-update args 'date (alist-get 'value args))))

(jetpacs-defaction "orgzly.note.plan-time"
  (lambda (args _)
    (orgzly-ui--plan-update args 'time (alist-get 'value args))))

(jetpacs-defaction "orgzly.note.plan-end"
  (lambda (args _)
    (orgzly-ui--plan-update args 'end (alist-get 'value args))))

(jetpacs-defaction "orgzly.note.plan-drop-time"
  (lambda (args _)
    (orgzly-ui--plan-update args 'time nil)))

(jetpacs-defaction "orgzly.note.plan-drop-end"
  (lambda (args _)
    (orgzly-ui--plan-update args 'end nil)))

(jetpacs-defaction "orgzly.note.plan-repeater"
  (lambda (args _)
    (let ((rep (string-trim
                (read-string "Repeater (+1w, ++1m, .+2d; empty to clear): "
                             (or (alist-get 'repeater
                                            (orgzly-ui--plan-parts
                                             (orgzly-ui--args-ref args)
                                             (alist-get 'kind args)))
                                 "")))))
      (orgzly-ui--plan-update args 'repeater
                              (unless (string-empty-p rep) rep)))))

(jetpacs-defaction "orgzly.note.plan-warning"
  (lambda (args _)
    (let ((warn (string-trim
                 (read-string
                  "Warning/delay (-2d; --2d first-only; empty to clear): "
                  (or (alist-get 'warning
                                 (orgzly-ui--plan-parts
                                  (orgzly-ui--args-ref args)
                                  (alist-get 'kind args)))
                      "")))))
      (orgzly-ui--plan-update args 'warning
                              (unless (string-empty-p warn) warn)))))

(jetpacs-defaction "orgzly.note.plan-clear"
  (lambda (args _)
    (jetpacs-dismiss-dialog)
    (orgzly-ui--plan-apply (orgzly-ui--args-ref args) (alist-get 'kind args)
                           nil)))

;; Properties.

(jetpacs-defaction "orgzly.note.add-property"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (name (string-trim (read-string "Property name: ")))
           (value (and (not (string-empty-p name))
                       (read-string (format "%s value: " name)))))
      (unless (string-empty-p name)
        (orgzly-data-set-property ref (upcase name) value)
        (orgzly-ui--after-mutation nil)))))

(jetpacs-defaction "orgzly.note.del-property"
  (lambda (args _)
    (orgzly-data-set-property (orgzly-ui--args-ref args)
                              (alist-get 'name args) nil)
    (orgzly-ui--after-mutation nil)))

;; Structure ops.

(jetpacs-defaction "orgzly.note.delete"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (when (y-or-n-p (format "Delete \"%s\"? " (alist-get 'title ref)))
        (orgzly-data-delete-note ref)
        (when (equal (alist-get 'pos orgzly-ui--note-ref) (alist-get 'pos ref))
          (setq orgzly-ui--note-ref nil))
        (jetpacs-shell-notify "Deleted")
        (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                            "orgzly.book" "orgzly.books"))))))

(jetpacs-defaction "orgzly.note.refile"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (books (mapcar (lambda (b) (alist-get 'name b)) (orgzly-data-books)))
           (target (completing-read "Refile to book: " books nil t)))
      (orgzly-data-refile ref target)
      (setq orgzly-ui--note-ref nil)
      (jetpacs-shell-notify (format "Refiled to %s" target))
      (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                          "orgzly.book" "orgzly.books")))))

(jetpacs-defaction "orgzly.note.archive"
  (lambda (args _)
    (orgzly-data-archive (orgzly-ui--args-ref args))
    (setq orgzly-ui--note-ref nil)
    (jetpacs-shell-notify "Archived")
    (jetpacs-shell-push nil :switch-to (if orgzly-ui--current-book
                                        "orgzly.book" "orgzly.books"))))

(jetpacs-defaction "orgzly.note.cut"
  (lambda (args _)
    (orgzly-data-cut-note (orgzly-ui--args-ref args))
    (setq orgzly-ui--note-ref nil)
    (jetpacs-shell-notify "Cut — paste from a note or book menu")
    (jetpacs-shell-push)))

(jetpacs-defaction "orgzly.note.copy"
  (lambda (args _)
    (orgzly-data-copy-note (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation "Copied")))

(jetpacs-defaction "orgzly.note.sort-content"
  ;; Upstream's "Sort note's content" — alphabetical over the lines.
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (entry (orgzly-ui--entry-for-ref ref))
           (content (and entry (alist-get 'content entry))))
      (when (and (stringp content) (not (string-empty-p content)))
        (orgzly-data-set-content
         ref (string-join (sort (split-string content "\n") #'string-lessp)
                          "\n"))
        (orgzly-ui--after-mutation "Sorted")))))

(jetpacs-defaction "orgzly.note.clock-in"
  (lambda (args _)
    (orgzly-data-clock-in (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation "Clocked in")))

(jetpacs-defaction "orgzly.note.clock-out"
  (lambda (args _)
    (orgzly-data-clock-out (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation "Clocked out")))

(jetpacs-defaction "orgzly.note.clock-cancel"
  (lambda (args _)
    (orgzly-data-clock-cancel (orgzly-ui--args-ref args))
    (orgzly-ui--after-mutation "Clock cancelled")))

(jetpacs-defaction "orgzly.note.focus"
  ;; Upstream's "focus": jump from a search/agenda hit to the note in
  ;; its own notebook, scrolled into view.
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args)))
      (setq orgzly-ui--current-book (orgzly-data-book-name
                                     (alist-get 'file ref))
            orgzly-ui--note-ref nil
            orgzly-ui--focus-ref ref)
      (jetpacs-shell-push nil :switch-to "orgzly.book"))))

(jetpacs-defaction "orgzly.note.paste"
  (lambda (args _)
    (let ((book (or (alist-get 'book args)
                    (and (alist-get 'file args)
                         (orgzly-data-book-name (alist-get 'file args)))
                    orgzly-ui--current-book)))
      (condition-case err
          (progn
            (orgzly-data-paste-note book (and (alist-get 'pos args)
                                              (orgzly-ui--args-ref args)))
            (orgzly-ui--after-mutation "Pasted"))
        (error (orgzly-ui--after-mutation
                (format "Nothing to paste (%s)" (error-message-string err))))))))

(dolist (op '(("orgzly.note.promote" . orgzly-data-promote)
              ("orgzly.note.demote" . orgzly-data-demote)
              ("orgzly.note.move-up" . orgzly-data-move-up)
              ("orgzly.note.move-down" . orgzly-data-move-down)))
  (let ((fn (cdr op)))
    (jetpacs-defaction (car op)
      (lambda (args _)
        (condition-case err
            (progn (funcall fn (orgzly-ui--args-ref args))
                   (orgzly-ui--after-mutation nil))
          (error (orgzly-ui--after-mutation (error-message-string err))))))))

;; ─── Actions: selection & bulk ───────────────────────────────────────────────

(defun orgzly-ui--toggle-selected (ref &optional force)
  (let ((present (orgzly-ui--selected-p ref)))
    (cond
     ((and present (not (eq force 'add)))
      (setq orgzly-ui--selection (delq present orgzly-ui--selection)))
     ((not present)
      (push ref orgzly-ui--selection)))))

(jetpacs-defaction "orgzly.note.select"
  (lambda (args _)
    (let ((ref (orgzly-ui--args-ref args))
          (checked (alist-get 'value args)))
      (orgzly-ui--toggle-selected ref (if (eq checked :false) 'remove 'add))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.note.select-toggle"
  (lambda (args _)
    (orgzly-ui--toggle-selected (orgzly-ui--args-ref args))
    (jetpacs-shell-push)))

(defun orgzly-ui--bulk (fn &optional message keep-selection)
  "Apply FN to every selected ref, then refresh."
  (let ((refs orgzly-ui--selection))
    (if (null refs)
        (jetpacs-shell-notify "Nothing selected")
      (dolist (ref refs) (funcall fn ref))
      (if (not keep-selection)
          (setq orgzly-ui--selection nil orgzly-ui--select-mode nil)
        ;; Mutations shift positions; re-anchor the kept selection on
        ;; the fresh scan so the checkboxes stay lit (mutations already
        ;; re-resolve drifted refs by title).
        (let ((entries (orgzly-data-entries)))
          (setq orgzly-ui--selection
                (delq nil
                      (mapcar
                       (lambda (ref)
                         (when-let ((e (cl-find-if
                                        (lambda (e)
                                          (and (equal (alist-get 'file e)
                                                      (alist-get 'file ref))
                                               (equal (alist-get 'title e)
                                                      (alist-get 'title ref))))
                                        entries)))
                           (orgzly-data-entry-ref e)))
                       refs)))))
      (when message
        (jetpacs-shell-notify (format "%s (%d notes)" message (length refs))))))
  (jetpacs-shell-push))

(jetpacs-defaction "orgzly.bulk.state"
  (lambda (_ _)
    (let* ((kws (orgzly-data-todo-keywords))
           (state (completing-read "State for selected: "
                                   (append '("NONE") (car kws) (cdr kws))
                                   nil t)))
      (orgzly-ui--bulk (lambda (ref)
                         (orgzly-data-set-state
                          ref (unless (equal state "NONE") state)))
                       (format "State → %s" state) t))))

(jetpacs-defaction "orgzly.bulk.toggle-done"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-toggle-done "Toggled" t)))

(jetpacs-defaction "orgzly.bulk.clock-in"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-clock-in "Clocked in" t)))

(jetpacs-defaction "orgzly.bulk.clock-out"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-clock-out "Clocked out" t)))

(jetpacs-defaction "orgzly.bulk.clock-cancel"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-clock-cancel "Clocks cancelled" t)))

(jetpacs-defaction "orgzly.bulk.promote"
  ;; ignore-errors: a top-level note can't promote; skip it, don't
  ;; abort the batch.
  (lambda (_ _)
    (orgzly-ui--bulk (lambda (ref) (ignore-errors (orgzly-data-promote ref)))
                     "Promoted" t)))

(jetpacs-defaction "orgzly.bulk.demote"
  (lambda (_ _)
    (orgzly-ui--bulk (lambda (ref) (ignore-errors (orgzly-data-demote ref)))
                     "Demoted" t)))

(jetpacs-defaction "orgzly.bulk.move"
  ;; Top-first going up, bottom-first going down, so a run of selected
  ;; siblings keeps its relative order (upstream's moving CAB);
  ;; ignore-errors skips the note already at the edge.
  (lambda (args _)
    (let ((up (equal (alist-get 'dir args) "up"))
          (ordered (orgzly-data--refs-in-order orgzly-ui--selection)))
      (setq orgzly-ui--selection (if up ordered (reverse ordered)))
      (orgzly-ui--bulk (lambda (ref)
                         (ignore-errors
                           (if up (orgzly-data-move-up ref)
                             (orgzly-data-move-down ref))))
                       (if up "Moved up" "Moved down") t))))

(jetpacs-defaction "orgzly.bulk.cut"
  (lambda (_ _)
    (let ((refs orgzly-ui--selection))
      (if (null refs)
          (jetpacs-shell-notify "Nothing selected")
        (orgzly-data-cut-notes refs)
        (setq orgzly-ui--selection nil orgzly-ui--select-mode nil)
        (jetpacs-shell-notify (format "Cut %d note%s" (length refs)
                                   (if (= (length refs) 1) "" "s"))))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.bulk.copy"
  (lambda (_ _)
    (let ((refs orgzly-ui--selection))
      (if (null refs)
          (jetpacs-shell-notify "Nothing selected")
        (orgzly-data-copy-notes refs)
        (setq orgzly-ui--selection nil orgzly-ui--select-mode nil)
        (jetpacs-shell-notify (format "Copied %d note%s" (length refs)
                                   (if (= (length refs) 1) "" "s"))))
      (jetpacs-shell-push))))

(jetpacs-defaction "orgzly.bulk.plan"
  (lambda (args _)
    (let ((kind (intern (alist-get 'kind args)))
          (date (alist-get 'value args)))
      (orgzly-ui--bulk (lambda (ref)
                         (orgzly-data-set-planning ref kind
                                                   `((date . ,date))))
                       (format "%s → %s" (capitalize (symbol-name kind)) date)
                       t))))

(jetpacs-defaction "orgzly.bulk.refile"
  (lambda (_ _)
    (let* ((books (mapcar (lambda (b) (alist-get 'name b)) (orgzly-data-books)))
           (target (completing-read "Refile selected to: " books nil t)))
      (orgzly-ui--bulk (lambda (ref) (orgzly-data-refile ref target))
                       (format "Refiled to %s" target)))))

(jetpacs-defaction "orgzly.bulk.archive"
  (lambda (_ _)
    (orgzly-ui--bulk #'orgzly-data-archive "Archived")))

(jetpacs-defaction "orgzly.bulk.delete"
  (lambda (_ _)
    (when (y-or-n-p (format "Delete %d selected notes? "
                            (length orgzly-ui--selection)))
      ;; Delete bottom-most first so earlier positions stay valid.
      (let ((refs (sort (copy-sequence orgzly-ui--selection)
                        (lambda (a b) (> (alist-get 'pos a)
                                         (alist-get 'pos b))))))
        (setq orgzly-ui--selection refs)
        (orgzly-ui--bulk #'orgzly-data-delete-note "Deleted")))))

(provide 'orgzly-ui)
;;; orgzly-ui.el ends here
