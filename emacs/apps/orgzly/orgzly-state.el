;;; orgzly-state.el --- Bounded Orgzly snapshots and opaque identities -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Disk access and reference creation happen here, never inside builders.
;;; Code:
(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-agenda)

(defvar orgzly-state nil "The last complete app snapshot; builders only read it.")
(defvar orgzly-state-folded nil "Opaque note ids whose descendants are folded.")
(defvar orgzly-state-query "" "The active bounded dotted search query.")
(defvar orgzly-state-dark nil "Non-nil selects the dark Orgzly preset.")
(defvar orgzly-state-page 0 "Zero-based page in the active outline or search.")
(defconst orgzly-state-page-size 40 "Maximum note rows emitted per screen.")
(defcustom orgzly-saved-searches
  '(("Agenda" . ".it.done ad.7") ("Next 3 days" . ".it.done s.ge.today ad.3")
    ("Scheduled" . "s.today .it.done") ("To Do" . "i.todo"))
  "Named dotted queries exposed beside notebooks."
  :type '(alist :key-type string :value-type string) :group 'orgzly)

(define-error 'orgzly-stale "Notebook changed; refresh Orgzly")

(defun orgzly-state-revision (file)
  "Hash bounded FILE bytes, refusing unsaved or externally changed buffers."
  (orgzly-data-check-file file)
  (when-let* ((buffer (get-file-buffer file)))
    (with-current-buffer buffer
      (when (or (buffer-modified-p) (not (verify-visited-file-modtime buffer)))
        (signal 'orgzly-stale nil))))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 (1+ orgzly-data-max-file-bytes))
    (when (> (buffer-size) orgzly-data-max-file-bytes) (error "Notebook grew beyond its limit"))
    (secure-hash 'sha256 (current-buffer))))

(defun orgzly-state-id (file revision &optional pos)
  "Return an opaque identifier for FILE, REVISION and optional heading POS."
  (concat "oz-" (substring (secure-hash 'sha256 (format "%S" (list file revision pos))) 0 32)))

(defun orgzly-refresh (&optional now)
  "Publish one bounded notebook snapshot at NOW, or the current time.
Failure keeps the previous complete snapshot.  External edits are reloaded
only into unmodified buffers.  Local unsaved edits are never overwritten."
  (let* ((now (or now (current-time)))
         (enable-local-eval nil) (enable-local-variables :safe)
         (refs (make-hash-table :test 'equal)) books entries)
    (orgzly-data-invalidate)
    (dolist (file (orgzly-data-book-files))
      (when-let* ((buf (get-file-buffer file)))
        (with-current-buffer buf
          (when (buffer-modified-p) (signal 'orgzly-stale nil))
          (unless (verify-visited-file-modtime buf) (revert-buffer t t t))))
      (let* ((name (orgzly-data-book-name file))
             (revision (orgzly-state-revision file))
             (id (orgzly-state-id file revision))
             (notes (copy-tree (orgzly-data-entries name)))
             (front (orgzly-data-book-front-matter name)))
        (when (> (+ (length entries) (length notes)) orgzly-data-max-entries)
          (error "Too many notes across Orgzly notebooks"))
        (unless (equal revision (orgzly-state-revision file)) (signal 'orgzly-stale nil))
        (dolist (entry notes)
          (let ((note-id (orgzly-state-id file revision (alist-get 'pos entry))))
            (push (cons 'id note-id) entry)
            (push (cons 'revision revision) entry)
            (puthash note-id entry refs)
            (push entry entries)))
        (let ((book `((id . ,id) (name . ,name) (file . ,file) (revision . ,revision)
                      (title . ,(or (alist-get 'title front) name))
                      (count . ,(length notes))
                      (preface . ,(orgzly-data-book-preface name)))))
          (puthash id book refs) (push book books))))
    (let* ((kws (orgzly-data-todo-keywords))
           (ctx (orgzly-query-context :now now :todo-keywords (car kws)
                                      :done-keywords (cdr kws)
                                      :default-priority (char-to-string org-priority-default))))
      (setq orgzly-state (list :books (nreverse books) :entries (nreverse entries)
                              :refs refs :now now :ctx ctx :searches (copy-tree orgzly-saved-searches)))))
  orgzly-state)

(defun orgzly-state-resolve (id &optional fresh)
  "Resolve opaque ID in the snapshot; FRESH also checks notebook bytes."
  (let ((entry (and (stringp id) (hash-table-p (plist-get orgzly-state :refs))
                    (gethash id (plist-get orgzly-state :refs)))))
    (unless entry (signal 'orgzly-stale nil))
    (when (and fresh (not (equal (alist-get 'revision entry)
                                 (orgzly-state-revision (alist-get 'file entry)))))
      (signal 'orgzly-stale nil))
    entry))

(defun orgzly-state-book (name)
  "Return the snapshot notebook named NAME."
  (cl-find name (plist-get orgzly-state :books) :key (lambda (b) (alist-get 'name b)) :test #'equal))

(defun orgzly-state-entries (book)
  "Return snapshot entries of BOOK; nil selects all notebooks."
  (if book (cl-remove-if-not (lambda (e) (equal book (alist-get 'book e)))
                             (plist-get orgzly-state :entries))
    (plist-get orgzly-state :entries)))

(defun orgzly-state-visible (entries)
  "Filter ENTRIES by the explicit fold state without changing it."
  (let (hidden-level result)
    (dolist (entry entries)
      (let ((level (alist-get 'level entry)))
        (when (and hidden-level (<= level hidden-level)) (setq hidden-level nil))
        (unless hidden-level
          (push entry result)
          (when (member (alist-get 'id entry) orgzly-state-folded)
            (setq hidden-level level)))))
    (nreverse result)))

(provide 'orgzly-state)
;;; orgzly-state.el ends here
