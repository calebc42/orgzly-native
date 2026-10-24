;;; orgzly-data.el --- Books & notes data layer over org files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))

;; Orgzly's data model, mapped onto plain org files:
;;   book = an .org file in `orgzly-directory' (name = base name)
;;   note = a heading (any level) in a book
;;
;; This module owns all reads and writes.  Reads produce plain alists —
;; the "entry" shape consumed by orgzly-query.el and every view:
;;
;;   ((book . STR) (file . STR) (pos . INT) (level . INT) (title . STR)
;;    (state . STR|nil) (priority . STR|nil) (tags . LIST) (itags . LIST)
;;    (scheduled . TS) (deadline . TS) (closed . TS) (created . TS)
;;    (events . (TS...)) (content . STR) (folded-content-lines . INT)
;;    (has-children . BOOL))
;;
;; TS = ((time . FLOAT) (has-time . BOOL) (raw . STR) (repeater . STR|nil))
;;
;; Scans are memoised per file mtime; every mutation calls
;; `orgzly-data-invalidate' (and the shell refresh hook drops it too, per
;; the jetpacs cache contract).

;;; Commentary:

;; Recovered Orgzly domain logic; current presentation lives in orgzly-ui.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-archive)
(require 'jetpacs-files)                 ; jetpacs-files-segment-p

(defgroup orgzly nil
  "Orgzly Revived, as a Jetpacs Tier 1."
  :group 'org)

(defcustom orgzly-directory
  (if (boundp 'org-directory) org-directory "~/org")
  "Directory whose .org files are the notebooks."
  :type 'directory :group 'orgzly)

(defcustom orgzly-default-book "inbox"
  "Book (base name, no extension) new notes and shares land in by default."
  :type 'string :group 'orgzly)

(defcustom orgzly-created-property "CREATED"
  "Property name recording a note's creation time for creation-date queries."
  :type 'string :group 'orgzly)

(defcustom orgzly-new-note-state nil
  "TODO state given to new notes, or nil for none."
  :type '(choice (const :tag "None" nil) string) :group 'orgzly)

(defcustom orgzly-new-note-created-property t
  "When non-nil, stamp new notes with `orgzly-created-property'."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-new-note-prepend nil
  "When non-nil, new notes go to the top of the book instead of the end."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-new-note-add-id nil
  "When non-nil, new notes get an :ID: property (Orgzly: populate ID)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-new-note-scheduled nil
  "When non-nil, new notes are scheduled for today (Orgzly: set scheduled)."
  :type 'boolean :group 'orgzly)

;; ─── Books ───────────────────────────────────────────────────────────────────

(defconst orgzly-data-max-books 64
  "Maximum notebooks read in one refresh.")
(defconst orgzly-data-max-file-bytes (* 1024 1024)
  "Maximum bytes in one notebook before opening it.")
(defconst orgzly-data-max-entries 2000
  "Maximum headings parsed from one notebook.")

(defun orgzly-data-book-file (book)
  "Return BOOK's contained local Org file, rejecting aliases and traversal."
  (unless (and (jetpacs-files-segment-p book) (<= (length book) 128)
               (not (string-search "\\" book))
               (not (file-remote-p orgzly-directory)))
    (error "Invalid local notebook name"))
  (let* ((root (file-name-as-directory (file-truename orgzly-directory)))
         (file (expand-file-name (concat book ".org") root)))
    (unless (equal (file-name-directory (file-truename file)) root)
      (error "Notebook escapes Orgzly directory"))
    (when (file-symlink-p file) (error "Notebook aliases are unsupported"))
    file))

(defun orgzly-data-book-name (file)
  "Return the notebook name of FILE."
  (file-name-base file))

(defun orgzly-data-check-file (file)
  "Check FILE's canonical ownership and size before any read or edit."
  (unless (equal (expand-file-name file)
                 (orgzly-data-book-file (orgzly-data-book-name file)))
    (error "File is outside Orgzly's notebook root"))
  (when-let* ((attributes (file-attributes file)))
    (unless (and (null (file-attribute-type attributes))
                 (<= (file-attribute-size attributes) orgzly-data-max-file-bytes))
      (error "Notebook is not a bounded regular file")))
  file)

(defun orgzly-data-book-files ()
  "Return at most `orgzly-data-max-books' sorted, contained notebook files."
  (when (file-directory-p orgzly-directory)
    (let ((files (cl-remove-if (lambda (file) (string-prefix-p ".#" (file-name-nondirectory file)))
                                     (directory-files orgzly-directory t "\\.org\\'"))))
      (when (> (length files) orgzly-data-max-books)
        (error "Too many Orgzly notebooks (maximum %d)" orgzly-data-max-books))
      (mapcar #'orgzly-data-check-file files))))

(defun orgzly-data-books ()
  "List of book alists: name, file, title (#+TITLE, or nil), mtime, count."
  (mapcar (lambda (file)
            (let ((name (orgzly-data-book-name file)))
              `((name . ,name)
                (file . ,file)
                (title . ,(alist-get 'title
                                     (orgzly-data-book-front-matter name)))
                (mtime . ,(float-time
                           (file-attribute-modification-time
                            (file-attributes file))))
                (count . ,(length (orgzly-data-entries name))))))
          (orgzly-data-book-files)))

(defun orgzly-data-create-book (name)
  "Create an empty book NAME.  Errors if it exists."
  (let ((file (orgzly-data-book-file name)))
    (when (file-exists-p file)
      (error "Book %s already exists" name))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert ""))
    (orgzly-data-invalidate)
    file))

(defun orgzly-data-rename-book (name new-name)
  "Rename book NAME to NEW-NAME."
  (let ((file (orgzly-data-book-file name))
        (new (orgzly-data-book-file new-name)))
    (when (file-exists-p new)
      (error "Book %s already exists" new-name))
    (when-let* ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (rename-file file new)
    (orgzly-data-invalidate)))

(defun orgzly-data-delete-book (name)
  "Delete book NAME (its file)."
  (let ((file (orgzly-data-book-file name)))
    (when-let* ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (delete-file file)
    (orgzly-data-invalidate)))

;; ─── Getting Started notebook ────────────────────────────────────────────────

(defconst orgzly-data-getting-started-book "Getting Started with Orgzly"
  "Name of the seeded tutorial notebook, as in Orgzly.")

(defconst orgzly-data--getting-started "\
Welcome to Orgzly!

* Notes
** Tap a note to open it
** Swipe a note, or press and hold it, to open its quick-action menu

Schedule, set a deadline, change state, refile, archive, cut, copy,
paste or delete — straight from the popup.

** To select several notes, tap the checklist button in the top bar

Batch-change state, times or notebook for every selected note at once.

** Fold a note which contains sub-notes by tapping the chevron next to it

*** There is no limit to the number of levels you can have
**** Use this to better organize your notes

** Note can have tags :tag1:tag2:
*** Sub-note inherits the tags :tag3:

If you search for notes tagged with “tag1” using “t.tag1” query, this
note will be included in the search results.

*** A note that has the =ARCHIVE= tag set is grayed out :ARCHIVE:

** Note can have a state

You can configure any number of states to use: TODO, NEXT, DONE, etc.

*** TODO There are two /types/ of states: to-do and done

*** DONE This is a note with /done/ type of state
CLOSED: [2018-01-24 Wed 17:00]

** Parent notes can track state of child notes [1/3] [33%]

*** TODO To track the ratio of TODO vs DONE items, add [/] to the note's title

*** TODO To track the percentage of DONE vs TODO items, add [%] to the note's title

*** DONE Both can be used in tandem
CLOSED: [2025-03-13 Thu 08:37]

The order doesn't matter, and the counts update when a child's state
changes.

** Note can have a scheduled time
SCHEDULED: <2015-02-20 Fri 15:15>

*** Times can have a repeater
SCHEDULED: <2015-02-16 Mon .+2d>

** You can also set a deadline time
DEADLINE: <2015-02-20 Fri>

** Event times can be used anywhere in the note's content

You can have multiple event times:

- <2015-02-20 Fri>
- <2015-02-21 Sat>

** Reminders for notes with scheduled, deadline, or event times are supported

Mark the note as done to shift the repeater.

** [#A] Note can have a priority

You can change the number of priorities in Settings. You can also
change default priority - assumed priority for notes without one set.

** Basic typographical emphasis is supported

You can make words *bold*, /italic/, _underlined_, =verbatim=, ~code~
and +strike-through+.

** Checkbox lists are available

- [X] Task 1
- [ ] Task 2
- [ ] Task 3

Open the note to edit them.

* Search
** There are many search operators supported

You can search notes by state, tag, scheduled, deadline or event
times, etc.

** Search queries can be saved for quick access

Try sample searches from the navigation drawer and note the queries
they use.

You can create your own saved searches by clicking on “Searches” in
the navigation drawer.

* Your notebooks are plain-text files

Notebooks live as [[https://orgmode.org/][Org mode]] files in the
=orgzly-directory= of your on-device Emacs — this very notebook is
one.  Edit them from Emacs and the app side by side, and sync or
version them with any tool you like (git, syncthing, ssh).
"
  "Contents of Orgzly's historical Getting Started tutorial notebook.
Reworded where this app deliberately diverges (quick-action popup,
checklist-toggle multi-select, left-edge fold chevron, sync by
architecture).")

(defun orgzly-data-import-getting-started ()
  "Recreate the Getting Started notebook and return its name.
Like Orgzly's import, re-importing over an existing copy resets it."
  (let ((file (orgzly-data-book-file orgzly-data-getting-started-book)))
    (when-let* ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert orgzly-data--getting-started))
    (orgzly-data-invalidate)
    orgzly-data-getting-started-book))

(defun orgzly-data-book-preface (book)
  "Text of BOOK before its first heading, or nil when empty."
  (let ((file (orgzly-data-book-file book)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((end (if (re-search-forward org-outline-regexp-bol nil t)
                       (match-beginning 0)
                     (point-max))))
          (let ((s (string-trim (buffer-substring (point-min) end))))
            (unless (string-empty-p s) s)))))))

(defun orgzly-data-book-front-matter (book)
  "BOOK's preface split into front matter and prose, or nil when empty.
An alist:
  ((title . STR|nil)                ; #+TITLE
   (created . TS|nil)               ; #+CREATED, parsed
   (filetags . LIST)                ; #+FILETAGS, colons stripped
   (settings . ((KEY . VALUE)...))  ; every other #+KEYWORD line
   (body . STR|nil))                ; the non-keyword prose"
  (when-let* ((preface (orgzly-data-book-preface book)))
    (let (title created filetags settings body)
      (dolist (line (split-string preface "\n"))
        (if (string-match "^[ \t]*#\\+\\([[:alnum:]_]+\\):\\(.*\\)$" line)
            (let* ((key (upcase (match-string 1 line)))
                   (val (string-trim (match-string 2 line)))
                   (ts (and (equal key "CREATED")
                            (orgzly-data--parse-ts val))))
              (cond (ts (setq created ts))
                    ((and (equal key "TITLE") (not (string-empty-p val)))
                     (setq title val))
                    ((equal key "FILETAGS")
                     (setq filetags (split-string val "[: \t]+" t)))
                    (t (push (cons key val) settings))))
          (push line body)))
      (setq body (string-trim (string-join (nreverse body) "\n")))
      `((title . ,title) (created . ,created) (filetags . ,filetags)
        (settings . ,(nreverse settings))
        (body . ,(unless (string-empty-p body) body))))))

(defun orgzly-data-set-book-filetags (book tags)
  "Set BOOK's #+FILETAGS: line to TAGS (empty removes the line).
File tags feed every note's inherited tags, so `t.' searches see them."
  (with-current-buffer (find-file-noselect (orgzly-data-book-file book))
    (org-with-wide-buffer
     (goto-char (point-min))
     (let* ((limit (if (re-search-forward org-outline-regexp-bol nil t)
                       (match-beginning 0)
                     (point-max)))
            (tags (cl-remove-if #'string-empty-p tags))
            (line (and tags (format "#+FILETAGS: :%s:"
                                    (string-join tags ":")))))
       (goto-char (point-min))
       (cond
        ((re-search-forward "^[ \t]*#\\+\\(?:FILETAGS\\|filetags\\):.*$"
                            limit t)
         (if line
             (replace-match line t t)
           (delete-region (line-beginning-position)
                          (min (point-max) (1+ (line-end-position))))))
        (line
         ;; New line goes after the last #+KEYWORD, or at the very top.
         (let ((at (point-min)))
           (while (re-search-forward "^[ \t]*#\\+[[:alnum:]_]+:" limit t)
             (setq at (min limit (1+ (line-end-position)))))
           (goto-char at)
           (insert line "\n")))))
     ;; The live buffer parsed its #+ keywords at open; re-read them so
     ;; `org-file-tags' (and the scan's inherited tags) see the change.
     (org-set-regexps-and-options))
    (save-buffer)
    (orgzly-data-invalidate)))

;; ─── The scan ────────────────────────────────────────────────────────────────

(defvar orgzly-data--cache (make-hash-table :test 'equal)
  "FILE -> (MTIME . ENTRIES) memo for `orgzly-data--file-entries'.")

(defun orgzly-data-invalidate ()
  "Drop all memoised scans (the jetpacs cache contract)."
  (clrhash orgzly-data--cache))

(defun orgzly-data--parse-ts (raw)
  "Parse org timestamp string RAW into the TS alist, or nil.
Besides the start time, captures every cookie Orgzly round-trips:
the end of a time range (`10:00-12:00' → `end-time'), the repeater,
and the warning/delay cookie (`-2d' on DEADLINE, `-2d'/`--2d' on
SCHEDULED → `warning')."
  (when (and (stringp raw) (not (string-empty-p raw)))
    (condition-case nil
        (let* ((time (org-time-string-to-time raw))
               (has-time (and (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" raw) t))
               (end-hhmm (and (string-match
                               "[0-9]\\{1,2\\}:[0-9]\\{2\\}-\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)"
                               raw)
                              (cons (string-to-number (match-string 1 raw))
                                    (string-to-number (match-string 2 raw)))))
               (repeater (and (string-match
                               "\\([.+]?\\+[0-9]+[hdwmy]\\)" raw)
                              (match-string 1 raw)))
               ;; `-N[u]' never collides with dates or times: those
               ;; follow their digits with `-', `:' or space, not a
               ;; repeater unit letter.
               (warning (and (string-match "\\(--?[0-9]+[hdwmy]\\)" raw)
                             (match-string 1 raw)))
               ;; A date range's second stamp: <a>--<b> or [a]--[b].
               (range-end (and (string-match
                                "--\\(<[^>\n]+>\\|\\[[^]\n]+\\]\\)" raw)
                               (condition-case nil
                                   (float-time (org-time-string-to-time
                                                (match-string 1 raw)))
                                 (error nil)))))
          `((time . ,(float-time time))
            (has-time . ,has-time)
            (raw . ,raw)
            (repeater . ,repeater)
            ,@(when range-end `((range-end . ,range-end)))
            ,@(when end-hhmm
                (let ((d (decode-time time)))
                  `((end-time . ,(float-time
                                  (encode-time 0 (cdr end-hhmm) (car end-hhmm)
                                               (nth 3 d) (nth 4 d) (nth 5 d)))))))
            ,@(when warning `((warning . ,warning)))))
      (error nil))))

(defun orgzly-data--entry-at-point (book file)
  "Extract the entry alist for the heading at point in BOOK / FILE.
Point must be at a heading start; leaves point unchanged."
  (save-excursion
    (let* ((components (org-heading-components))
           (level (nth 0 components))
           (state (nth 2 components))
           (priority (and (nth 3 components) (char-to-string (nth 3 components))))
           (title (or (nth 4 components) ""))
           (pos (point))
           (local-tags (org-get-tags nil t))
           (all-tags (org-get-tags))
           (itags (cl-set-difference all-tags local-tags :test #'equal))
           (scheduled (orgzly-data--parse-ts (org-entry-get nil "SCHEDULED")))
           (deadline (orgzly-data--parse-ts (org-entry-get nil "DEADLINE")))
           (closed (orgzly-data--parse-ts (org-entry-get nil "CLOSED")))
           (created (orgzly-data--parse-ts
                     (org-entry-get nil orgzly-created-property)))
           (content-info (orgzly-data--entry-content))
           (events (orgzly-data--entry-events title (car content-info))))
      `((book . ,book) (file . ,file) (pos . ,pos) (level . ,level)
        (title . ,title) (state . ,state) (priority . ,priority)
        (tags . ,local-tags) (itags . ,itags)
        (scheduled . ,scheduled) (deadline . ,deadline)
        (closed . ,closed) (created . ,created)
        (events . ,events)
        (content . ,(nth 0 content-info))
        (content-lines . ,(nth 1 content-info))
        (content-pos . ,(nth 2 content-info))
        (has-children . ,(save-excursion
                           (org-goto-first-child)
                           (not (= pos (point)))))))))

(defun orgzly-data--entry-content ()
  "Content of the entry at point as (TEXT LINE-COUNT BEGIN).
The text between the metadata (planning line, property/logbook drawers)
and the next heading — Orgzly's note content.  BEGIN is its buffer
position, which the rich renderer uses to anchor in-place edits."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    ;; Skip planning line and any leading drawers.
    (while (and (not (eobp))
                (or (looking-at-p org-planning-line-re)
                    (looking-at-p org-drawer-regexp)))
      (if (looking-at-p org-planning-line-re)
          (forward-line 1)
        (if (re-search-forward "^[ \t]*:END:[ \t]*$"
                               (save-excursion (outline-next-heading) (point))
                               t)
            (forward-line 1)
          (forward-line 1))))
    (let* ((beg (point))
           ;; Point may already sit on the next heading (empty content);
           ;; `outline-next-heading' would skip past it.
           (end (if (org-at-heading-p) (point)
                  (progn (outline-next-heading) (point))))
           (text (string-trim-right (buffer-substring-no-properties beg end))))
      (list text
            (if (string-empty-p text) 0
              (1+ (cl-count ?\n text)))
            beg))))

(defun orgzly-data--entry-events (title content)
  "Plain active timestamps in TITLE and CONTENT as a list of TS alists."
  (let ((text (concat title "\n" (or content "")))
        (events nil) (start 0))
    (while (string-match org-ts-regexp text start)
      ;; Capture before parsing: `orgzly-data--parse-ts' runs its own
      ;; string-matches, which would clobber this loop's match data.
      (let ((raw (match-string 0 text))
            (end (match-end 0)))
        (push (orgzly-data--parse-ts raw) events)
        (setq start end)))
    (nreverse (delq nil events))))

(defun orgzly-data--book-buffer (book)
  "A live `org-mode' buffer visiting BOOK's file."
  (let ((file (orgzly-data-book-file book)))
    (unless (file-exists-p file)
      (error "No book %s" book))
    (find-file-noselect file)))

(defun orgzly-data--file-entries (file)
  "All entries of FILE, memoised on its mtime."
  (orgzly-data-check-file file)
  (let* ((mtime (float-time (file-attribute-modification-time
                             (file-attributes file))))
         (hit (gethash file orgzly-data--cache)))
    (if (and hit (= (car hit) mtime))
        (cdr hit)
      (let* ((book (orgzly-data-book-name file))
             (entries
              (with-current-buffer (find-file-noselect file)
                (org-with-wide-buffer
                 (goto-char (point-min))
                 (let (acc)
                   (unless (org-at-heading-p)
                     (outline-next-heading))
                   (while (not (eobp))
                     (when (>= (length acc) orgzly-data-max-entries)
                       (error "Too many headings in notebook"))
                     (push (orgzly-data--entry-at-point book file) acc)
                     (outline-next-heading))
                   (nreverse acc))))))
        (puthash file (cons mtime entries) orgzly-data--cache)
        entries))))

(defun orgzly-data-entries (&optional book)
  "Entries of BOOK, or of every book when BOOK is nil."
  (if book
      (let ((file (orgzly-data-book-file book)))
        (when (file-exists-p file) (orgzly-data--file-entries file)))
    (apply #'append (mapcar #'orgzly-data--file-entries
                            (orgzly-data-book-files)))))

;; ─── State keyword configuration ─────────────────────────────────────────────

(defun orgzly-data-todo-keywords ()
  "Effective keywords as (TODO-LIST . DONE-LIST).
The globally configured `org-todo-keywords' split, which is what the
settings screen edits — per-file #+TODO lines still take effect inside
each buffer for state cycling."
  (let (todo done)
    (dolist (seq org-todo-keywords)
      (let ((kws (mapcar (lambda (k)
                           (replace-regexp-in-string "(.*)" "" k))
                         (cdr seq)))
            (in-done nil))
        (dolist (k kws)
          (cond ((equal k "|") (setq in-done t))
                (in-done (push k done))
                (t (push k todo))))
        ;; No "|" separator: last keyword is the done state.
        (unless in-done
          (when-let* ((last (car todo)))
            (setq todo (cdr todo))
            (push last done)))))
    (cons (nreverse todo) (nreverse done))))

(defun orgzly-data-query-context ()
  "The `orgzly-query-context' reflecting current org config."
  (let ((kws (orgzly-data-todo-keywords)))
    (orgzly-query-context
     :todo-keywords (car kws)
     :done-keywords (cdr kws)
     :default-priority (char-to-string org-priority-default))))

;; ─── Locating a note from a wire ref ─────────────────────────────────────────

(defun orgzly-data--goto-ref (ref)
  "Resolve internal REF exactly; never guess among duplicate titles."
  (let* ((file (orgzly-data-check-file (alist-get 'file ref)))
         (pos (alist-get 'pos ref))
         (title (alist-get 'title ref))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (widen)
      (unless (and (integerp pos) (<= (point-min) pos (point-max)))
        (error "Stale note position"))
      (goto-char pos)
      (unless (and (org-at-heading-p)
                   (or (null title) (equal title (nth 4 (org-heading-components)))))
        (error "Stale note identity")))
    buf))

(defvar orgzly-data--transaction nil
  "Non-nil while an outer atomic save owns a compound note edit.")

(defmacro orgzly-data--with-ref (ref &rest body)
  "Run BODY with point on REF's heading, then save and invalidate."
  (declare (indent 1))
  `(let ((buf (orgzly-data--goto-ref ,ref)))
     (with-current-buffer buf
       (atomic-change-group
         (prog1 (save-excursion ,@body)
           (unless orgzly-data--transaction (save-buffer))
           (orgzly-data-invalidate))))))

(defun orgzly-data-entry-ref (entry)
  "Return the internal position reference of ENTRY; never send it on the wire."
  `((file . ,(alist-get 'file entry))
    (pos . ,(alist-get 'pos entry))
    (title . ,(alist-get 'title entry))))

;; ─── Mutations ───────────────────────────────────────────────────────────────

(defun orgzly-data-set-state (ref state)
  "Set REF's TODO state to STATE (nil or \"\" clears it)."
  (orgzly-data--with-ref ref
    (org-todo (if (or (null state) (string-empty-p state)) 'none state))))

(defun orgzly-data-set-priority (ref priority)
  "Set REF's priority to PRIORITY (a letter string, or nil to remove)."
  (orgzly-data--with-ref ref
    (org-priority (if priority (string-to-char priority) 'remove))))

(defun orgzly-data-set-tags (ref tags)
  "Set REF's own tags to TAGS (a list of strings)."
  (orgzly-data--with-ref ref
    (org-set-tags (cl-remove-if #'string-empty-p tags))))

(defun orgzly-data-set-title (ref title)
  "Rename REF's heading to TITLE."
  (orgzly-data--with-ref ref
    (org-edit-headline title)))

(defun orgzly-data--format-ts (parts &optional inactive)
  "Canonical timestamp assembled from the PARTS alist.
PARTS holds strings under `date' (required, \"2026-07-20\"), `time'
For example \"14:00\", `end' (\"15:30\", only with time), `repeater' (\"+1w\"),
`warning' (\"-2d\").  INACTIVE brackets it [so] instead of <so>."
  (let* ((date (alist-get 'date parts))
         (time (alist-get 'time parts))
         (end (alist-get 'end parts))
         (day (encode-time 0 0 0
                           (string-to-number (substring date 8 10))
                           (string-to-number (substring date 5 7))
                           (string-to-number (substring date 0 4)))))
    (concat (if inactive "[" "<") (format-time-string "%Y-%m-%d %a" day)
            (and time (concat " " time (and end (concat "-" end))))
            (when-let* ((r (alist-get 'repeater parts))) (concat " " r))
            (when-let* ((w (alist-get 'warning parts))) (concat " " w))
            (if inactive "]" ">"))))

(defun orgzly-data-set-planning (ref kind parts)
  "Set REF's planning KIND (`scheduled', `deadline' or `closed') from PARTS.
PARTS is nil to remove the entry, or an alist for
`orgzly-data--format-ts'.  org's planning functions manage the line's
placement and removal, but they drop trailing repeater/warning cookies
from their TIME argument (and would lose an end time passed alongside
them) — so the stamp they insert is rewritten whole from PARTS."
  (orgzly-data--with-ref ref
    (let ((date (alist-get 'date parts))
          (closed (eq kind 'closed)))
      (if (null date)
          (if closed
              (org-add-planning-info nil nil 'closed)
            (funcall (if (eq kind 'deadline) #'org-deadline #'org-schedule)
                     '(4)))
        (if closed
            (org-add-planning-info 'closed (org-read-date nil t date))
          (funcall (if (eq kind 'deadline) #'org-deadline #'org-schedule)
                   nil date))
        (org-back-to-heading t)
        (let ((bound (line-end-position 2))
              (re (pcase kind
                    ('closed (concat org-closed-string
                                     "[ \t]*\\(\\[[^]\n]+\\]\\)"))
                    ('deadline (concat org-deadline-string
                                       "[ \t]*\\(<[^>\n]+>\\)"))
                    (_ (concat org-scheduled-string
                               "[ \t]*\\(<[^>\n]+>\\)")))))
          (when (re-search-forward re bound t)
            (replace-match (orgzly-data--format-ts parts closed)
                           t t nil 1)))))))

(defun orgzly-data-set-content (ref content)
  "Replace REF's body with CONTENT, between metadata and the next heading."
  (orgzly-data--with-ref ref
    (org-back-to-heading t)
    (forward-line 1)
    (while (and (not (eobp))
                (or (looking-at-p org-planning-line-re)
                    (looking-at-p org-drawer-regexp)))
      (if (looking-at-p org-planning-line-re)
          (forward-line 1)
        (if (re-search-forward "^[ \t]*:END:[ \t]*$"
                               (save-excursion (outline-next-heading) (point)) t)
            (forward-line 1)
          (forward-line 1))))
    (let ((beg (point))
          (end (progn (outline-next-heading) (point))))
      (delete-region beg end)
      (goto-char beg)
      (let ((text (string-trim-right (or content ""))))
        (unless (string-empty-p text)
          (insert text "\n"))))))

(defun orgzly-data-set-property (ref name value)
  "Set property NAME to VALUE on REF (nil VALUE deletes it)."
  (orgzly-data--with-ref ref
    (if value
        (org-set-property name value)
      (org-delete-property name))))

(defun orgzly-data-properties (ref)
  "REF's own properties as an alist, sans org-internal specials."
  (let ((buf (orgzly-data--goto-ref ref)))
    (with-current-buffer buf
      (save-excursion
        (cl-remove-if
         (lambda (p) (member (car p) '("CATEGORY" "BLOCKED" "FILE" "PRIORITY"
                                       "ITEM" "TODO" "ALLTAGS" "TAGS")))
         (org-entry-properties nil 'standard))))))

(defun orgzly-data-delete-note (ref)
  "Delete REF's subtree."
  (orgzly-data--with-ref ref
    (org-cut-subtree)
    (setq kill-ring (cdr kill-ring))))  ; a delete is not a cut

(defun orgzly-data-cut-note (ref)
  "Cut REF's subtree onto the org subtree clipboard."
  (orgzly-data--with-ref ref
    (org-cut-subtree)))

(defun orgzly-data-copy-note (ref)
  "Copy REF's subtree onto the org subtree clipboard."
  (let ((buf (orgzly-data--goto-ref ref)))
    (with-current-buffer buf
      (save-excursion (org-copy-subtree)))))

(defun orgzly-data--refs-in-order (refs)
  "REFS sorted into document order (file, then position)."
  (sort (copy-sequence refs)
        (lambda (a b)
          (let ((fa (alist-get 'file a)) (fb (alist-get 'file b)))
            (if (equal fa fb)
                (< (alist-get 'pos a) (alist-get 'pos b))
              (string-lessp fa fb))))))

(defun orgzly-data--subtree-text (&optional delete)
  "Text of the subtree at point, re-rooted to level 1; DELETE cuts it.
Re-rooting keeps a multi-selection paste-able as one block:
`org-paste-subtree' accepts a set of trees only when none sits above
the first."
  (org-back-to-heading t)
  (let* ((level (org-current-level))
         (beg (point))
         (end (progn (org-end-of-subtree t t) (point)))
         (text (buffer-substring-no-properties beg end)))
    (when delete (delete-region beg end))
    (unless (string-suffix-p "\n" text) (setq text (concat text "\n")))
    (if (<= level 1)
        text
      (replace-regexp-in-string
       (format "^\\*\\{%d\\}\\(\\*+ \\)" (1- level)) "\\1" text))))

(defun orgzly-data-cut-notes (refs)
  "Cut REFS' subtrees as one document-ordered clipboard block.
Extraction runs bottom-up so earlier positions stay valid; a single
paste restores all of them (Orgzly's batch cut)."
  (let (texts)
    (dolist (ref (reverse (orgzly-data--refs-in-order refs)))
      (push (orgzly-data--with-ref ref
              (orgzly-data--subtree-text t))
            texts))
    (kill-new (string-join texts ""))))

(defun orgzly-data-copy-notes (refs)
  "Copy REFS' subtrees as one document-ordered clipboard block."
  (kill-new (mapconcat (lambda (ref)
                         (with-current-buffer (orgzly-data--goto-ref ref)
                           (save-excursion (orgzly-data--subtree-text))))
                       (orgzly-data--refs-in-order refs) "")))

(defun orgzly-data-paste-note (book &optional ref)
  "Paste the clipboard subtree into BOOK, after REF or at the end."
  (let ((file (orgzly-data-book-file book)))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (if ref
           (progn (goto-char (alist-get 'pos ref))
                  (org-back-to-heading t)
                  (let ((level (org-current-level)))
                    (org-end-of-subtree t t)
                    (org-paste-subtree level)))
         (goto-char (point-max))
         (unless (bolp) (insert "\n"))
         (org-paste-subtree 1)))
      (save-buffer))
    (orgzly-data-invalidate)))

(defun orgzly-data-refile (ref book)
  "Move REF's subtree to the end of BOOK."
  (let ((target (orgzly-data-book-file book)))
    (unless (file-exists-p target) (orgzly-data-create-book book))
    (orgzly-data--with-ref ref
      (org-cut-subtree))
    (with-current-buffer (find-file-noselect target)
      (org-with-wide-buffer
       (goto-char (point-max))
       (unless (bolp) (insert "\n"))
       (org-paste-subtree 1))
      (save-buffer))
    (orgzly-data-invalidate)))

(defun orgzly-data-archive (ref)
  "Archive REF's subtree per `org-archive-location'."
  (orgzly-data--with-ref ref
    (org-archive-subtree)))

;; ─── Clocking ────────────────────────────────────────────────────────────────
;;
;; Orgzly's clocking is per note — an open CLOCK: line in the LOGBOOK
;; drawer, no global single-clock — so these edit the drawer directly
;; rather than driving `org-clock-in' (which manages one active clock,
;; mode-line state, and auto-clock-out).

(defun orgzly-data--open-clock-pos ()
  "Position of the entry's running CLOCK line at bol, or nil.
Point must be within the entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (org-entry-end-position)))
      (when (re-search-forward "^[ \t]*CLOCK: \\[[^]\n]+\\][ \t]*$" end t)
        (match-beginning 0)))))

(defun orgzly-data-clock-in (ref)
  "Start a clock on REF: an open CLOCK: line atop its LOGBOOK drawer.
No-op while one is already running there (Orgzly's clockIn)."
  (orgzly-data--with-ref ref
    (unless (orgzly-data--open-clock-pos)
      (let ((org-log-into-drawer "LOGBOOK"))
        (goto-char (org-log-beginning t))
        (insert (format-time-string "CLOCK: [%Y-%m-%d %a %H:%M]\n"))))))

(defun orgzly-data-clock-out (ref)
  "Close REF's running clock, appending the end time and duration."
  (orgzly-data--with-ref ref
    (when-let* ((pos (orgzly-data--open-clock-pos)))
      (goto-char pos)
      (re-search-forward org-ts-regexp-inactive (line-end-position))
      (let* ((start (org-time-string-to-time (match-string 0)))
             (now (current-time))
             (mins (max 0 (floor (float-time (time-subtract now start)) 60))))
        (end-of-line)
        (delete-horizontal-space)
        (insert (format-time-string "--[%Y-%m-%d %a %H:%M]" now)
                (format " =>  %d:%02d" (/ mins 60) (% mins 60)))))))

(defun orgzly-data-clock-cancel (ref)
  "Remove REF's running clock line; drop the drawer if it ends up empty."
  (orgzly-data--with-ref ref
    (when-let* ((pos (orgzly-data--open-clock-pos)))
      (goto-char pos)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position))))
      ;; An emptied LOGBOOK is deleted, like org would.
      (forward-line -1)
      (when (and (looking-at-p "^[ \t]*:LOGBOOK:[ \t]*$")
                 (save-excursion
                   (forward-line 1)
                   (looking-at-p "^[ \t]*:END:[ \t]*$")))
        (delete-region (line-beginning-position)
                       (min (point-max)
                            (save-excursion (forward-line 2) (point))))))))

(defun orgzly-data-promote (ref)
  "Promote REF's subtree one level."
  (orgzly-data--with-ref ref (org-promote-subtree)))

(defun orgzly-data-demote (ref)
  "Demote REF's subtree one level."
  (orgzly-data--with-ref ref (org-demote-subtree)))

(defun orgzly-data-move-up (ref)
  "Swap REF's subtree with the previous same-level sibling."
  (orgzly-data--with-ref ref (org-move-subtree-up)))

(defun orgzly-data-move-down (ref)
  "Swap REF's subtree with the next same-level sibling."
  (orgzly-data--with-ref ref (org-move-subtree-down)))

(defun orgzly-data-toggle-done (ref)
  "Cycle REF between the first todo and first done keyword.
The home-widget checkmark semantics: repeater-aware because it goes
through `org-todo' (a repeating task reschedules instead of closing)."
  (let ((kws (orgzly-data-todo-keywords)))
    (orgzly-data--with-ref ref
      (let ((state (nth 2 (org-heading-components))))
        (org-todo (if (member state (cdr kws))
                      (or (caar kws) "TODO")
                    (or (cadr kws) "DONE")))))))

(cl-defun orgzly-data-new-note (book title &key state priority tags
                                     scheduled deadline content under
                                     sibling before)
  "Create a note in BOOK titled TITLE; returns its ref.
STATE/PRIORITY/TAGS/SCHEDULED/DEADLINE/CONTENT seed the metadata
Values are strings; SCHEDULED/DEADLINE are org date strings).  UNDER, when
non-nil, is a parent ref the note is created beneath; SIBLING a ref
the note is created next to at the same level — after its subtree, or
directly above it when BEFORE (Orgzly's new above/under/below).
Otherwise the note is a top-level heading appended to the book (or
prepended, per `orgzly-new-note-prepend').  Defaults honour the Orzgly
new-note settings (`orgzly-new-note-state', created-at property)."
  (let ((file (orgzly-data-book-file book))
        (state (or state orgzly-new-note-state))
        (scheduled (or scheduled
                       (and orgzly-new-note-scheduled
                            (format-time-string "%Y-%m-%d")))))
    (unless (file-exists-p file) (orgzly-data-create-book book))
    (with-current-buffer (find-file-noselect file)
      (org-with-wide-buffer
       (let ((level 1))
         (cond
          (under
           (goto-char (alist-get 'pos under))
           (org-back-to-heading t)
           (setq level (1+ (org-current-level)))
           (org-end-of-subtree t t))
          (sibling
           (goto-char (alist-get 'pos sibling))
           (org-back-to-heading t)
           (setq level (org-current-level))
           (unless before
             (org-end-of-subtree t t)
             (unless (bolp) (insert "\n"))))
          (orgzly-new-note-prepend
           (goto-char (point-min))
           (if (re-search-forward org-outline-regexp-bol nil t)
               (beginning-of-line)
             (goto-char (point-max))
             (unless (bolp) (insert "\n"))))
          (t
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))))
         (let ((beg (point)))
           (insert (make-string level ?*) " "
                   (if (and state (not (string-empty-p state)))
                       (concat state " ") "")
                   title "\n")
           (goto-char beg)
           (when priority (org-priority (string-to-char priority)))
           (when tags (org-set-tags tags))
           (when (and scheduled (not (string-empty-p scheduled)))
             (org-schedule nil scheduled))
           (when (and deadline (not (string-empty-p deadline)))
             (org-deadline nil deadline))
           (when orgzly-new-note-created-property
             (org-set-property orgzly-created-property
                               (format-time-string "[%Y-%m-%d %a %H:%M]")))
           (when orgzly-new-note-add-id
             (require 'org-id)
             (org-set-property "ID" (org-id-uuid)))
           (when (and content (not (string-empty-p (string-trim content))))
             (org-end-of-subtree t t)
             (insert (string-trim-right content) "\n"))
           (goto-char beg)
           (prog1 `((file . ,file) (pos . ,(point)) (title . ,title))
             (unless orgzly-data--transaction (save-buffer))
             (orgzly-data-invalidate))))))))

(provide 'orgzly-data)
;;; orgzly-data.el ends here
