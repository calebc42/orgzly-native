;;; orgzly-data.el --- Books & notes data layer over org files -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'org-archive)

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
  "Property name recording a note's creation time (cr. queries read it)."
  :type 'string :group 'orgzly)

(defcustom orgzly-new-note-state nil
  "TODO state given to new notes, or nil for none (Orgzly: New note / Set state)."
  :type '(choice (const :tag "None" nil) string) :group 'orgzly)

(defcustom orgzly-new-note-created-property t
  "When non-nil, stamp new notes with `orgzly-created-property'."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-new-note-prepend nil
  "When non-nil, new notes go to the top of the book instead of the end."
  :type 'boolean :group 'orgzly)

;; ─── Books ───────────────────────────────────────────────────────────────────

(defun orgzly-data-book-file (book)
  "Absolute path of BOOK's org file."
  (expand-file-name (concat book ".org") orgzly-directory))

(defun orgzly-data-book-name (file)
  "Book name of FILE."
  (file-name-base file))

(defun orgzly-data-book-files ()
  "The notebook files: .org files directly under `orgzly-directory'."
  (when (file-directory-p orgzly-directory)
    (sort (directory-files orgzly-directory t "\\.org\\'" t) #'string-lessp)))

(defun orgzly-data-books ()
  "List of book alists: name, file, mtime, note count."
  (mapcar (lambda (file)
            `((name . ,(orgzly-data-book-name file))
              (file . ,file)
              (mtime . ,(float-time
                         (file-attribute-modification-time
                          (file-attributes file))))
              (count . ,(length (orgzly-data-entries
                                 (orgzly-data-book-name file))))))
          (orgzly-data-book-files)))

(defun orgzly-data-create-book (name)
  "Create an empty book NAME. Errors if it exists."
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
    (when-let ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (rename-file file new)
    (orgzly-data-invalidate)))

(defun orgzly-data-delete-book (name)
  "Delete book NAME (its file)."
  (let ((file (orgzly-data-book-file name)))
    (when-let ((buf (find-buffer-visiting file)))
      (kill-buffer buf))
    (delete-file file)
    (orgzly-data-invalidate)))

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

;; ─── The scan ────────────────────────────────────────────────────────────────

(defvar orgzly-data--cache (make-hash-table :test 'equal)
  "FILE -> (MTIME . ENTRIES) memo for `orgzly-data--file-entries'.")

(defun orgzly-data-invalidate ()
  "Drop all memoised scans (the jetpacs cache contract)."
  (clrhash orgzly-data--cache))

(defun orgzly-data--parse-ts (raw)
  "Parse org timestamp string RAW into the TS alist, or nil."
  (when (and (stringp raw) (not (string-empty-p raw)))
    (condition-case nil
        (let* ((time (org-time-string-to-time raw))
               (has-time (and (string-match "[0-9]\\{1,2\\}:[0-9]\\{2\\}" raw) t))
               (repeater (and (string-match
                               "\\([.+]?\\+[0-9]+[hdwmy]\\)" raw)
                              (match-string 1 raw))))
          `((time . ,(float-time time))
            (has-time . ,has-time)
            (raw . ,raw)
            (repeater . ,repeater)))
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
        (content . ,(car content-info))
        (content-lines . ,(cdr content-info))
        (has-children . ,(save-excursion
                           (org-goto-first-child)
                           (not (= pos (point)))))))))

(defun orgzly-data--entry-content ()
  "Content of the entry at point as (TEXT . LINE-COUNT).
The text between the metadata (planning line, property/logbook drawers)
and the next heading — Orgzly's note content."
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
      (cons text
            (if (string-empty-p text) 0
              (1+ (cl-count ?\n text)))))))

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
  "A live org-mode buffer visiting BOOK's file."
  (let ((file (orgzly-data-book-file book)))
    (unless (file-exists-p file)
      (error "No book %s" book))
    (find-file-noselect file)))

(defun orgzly-data--file-entries (file)
  "All entries of FILE, memoised on its mtime."
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
          (when-let ((last (car todo)))
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
  "Move point (in the right buffer) to the heading REF names.
REF is an alist with `file' and `pos', optionally `title' for
verification; returns the buffer. Positions can drift after edits: when
the heading at pos doesn't match the expected title, fall back to
searching the file for it."
  (let* ((file (alist-get 'file ref))
         (pos (alist-get 'pos ref))
         (title (alist-get 'title ref))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (org-with-wide-buffer nil)
      (widen)
      (goto-char (min (max (or pos 1) 1) (point-max)))
      (unless (org-at-heading-p) (ignore-errors (org-back-to-heading t)))
      (when (and title
                 (not (equal title (nth 4 (ignore-errors
                                            (org-heading-components))))))
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (re-search-forward org-outline-regexp-bol nil t))
            (when (equal title (nth 4 (org-heading-components)))
              (setq found t)))
          (unless found (error "Note \"%s\" not found in %s" title file))
          (beginning-of-line))))
    buf))

(defmacro orgzly-data--with-ref (ref &rest body)
  "Run BODY with point on REF's heading, then save and invalidate."
  (declare (indent 1))
  `(let ((buf (orgzly-data--goto-ref ,ref)))
     (with-current-buffer buf
       (prog1 (save-excursion ,@body)
         (save-buffer)
         (orgzly-data-invalidate)))))

(defun orgzly-data-entry-ref (entry)
  "The wire ref (file/pos/title) of ENTRY, for embedding in actions."
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

(defun orgzly-data-set-planning (ref kind value)
  "Set REF's planning KIND (`scheduled' or `deadline') to VALUE.
VALUE is an org date/time string (\"2026-07-10\", \"2026-07-10 14:00\",
optionally with a trailing repeater like \"+1w\"), or nil to remove."
  (orgzly-data--with-ref ref
    (let ((fn (if (eq kind 'deadline) #'org-deadline #'org-schedule)))
      (if (or (null value) (string-empty-p value))
          (funcall fn '(4))
        (funcall fn nil value)))))

(defun orgzly-data-set-content (ref content)
  "Replace REF's content (text between metadata and next heading)."
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
                                     scheduled deadline content under)
  "Create a note in BOOK titled TITLE; returns its ref.
STATE/PRIORITY/TAGS/SCHEDULED/DEADLINE/CONTENT seed the metadata
(strings; SCHEDULED/DEADLINE are org date strings).  UNDER, when
non-nil, is a parent ref the note is created beneath; otherwise the
note is a top-level heading appended to the book (or prepended, per
`orgzly-new-note-prepend').  Defaults honour the Orzgly new-note
settings (`orgzly-new-note-state', created-at property)."
  (let ((file (orgzly-data-book-file book))
        (state (or state orgzly-new-note-state)))
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
           (when (and content (not (string-empty-p (string-trim content))))
             (org-end-of-subtree t t)
             (insert (string-trim-right content) "\n"))
           (goto-char beg)
           (prog1 `((file . ,file) (pos . ,(point)) (title . ,title))
             (save-buffer)
             (orgzly-data-invalidate))))))))

(provide 'orgzly-data)
;;; orgzly-data.el ends here
