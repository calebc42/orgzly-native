;;; orgzly-capture.el --- Quick note & share-in -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's capture paths: text shared from any app (the companion's
;; share sheet emits the app-agnostic `share.text', queued offline) lands
;; as a note in the default notebook; the drawer's "New note" prompts and
;; files there too.

;;; Code:

(require 'jetpacs-shell)
(require 'orgzly-data)

(declare-function orgzly-ui--reset-drill-in "orgzly-ui")

(defvar orgzly-ui--note-ref)
(defvar orgzly-ui--current-book)

(defcustom orgzly-share-open-note nil
  "When non-nil, open the editor on a note created from a share.
Off matches Orgzly: sharing creates the note and gets out of the way."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-shared-text-placement 'title
  "Where subject-less shared text goes (Orgzly: shared text placement).
`title' makes the first line the note's title, the rest content;
`content' keeps the whole text as content under an \"Untitled\" note —
Orgzly's default is content with an empty heading, but an untitled
heading serves the outline better here."
  :type '(choice (const :tag "First line as title" title)
                 (const :tag "All in content" content))
  :group 'orgzly)

(defun orgzly-capture--on-share (args _payload)
  "Create a note in the default book from a share sheet payload."
  (let* ((text (alist-get 'text args))
         (subject (alist-get 'subject args))
         (text (and (stringp text) (not (string-empty-p (string-trim text)))
                    (string-trim text)))
         (subject (and (stringp subject)
                       (not (string-empty-p (string-trim subject)))
                       (string-trim subject)))
         (in-content (and (not subject)
                          (eq orgzly-shared-text-placement 'content)))
         ;; Title: the subject, else the shared text's first line —
         ;; unless the placement pref keeps the whole text as content.
         (title (cond (subject subject)
                      (in-content "Untitled")
                      (t (car (split-string (or text "Shared note") "\n")))))
         (content (cond ((or subject in-content) text)
                        ((and text (string-match-p "\n" text))
                         (mapconcat #'identity
                                    (cdr (split-string text "\n")) "\n"))))
         (ref (orgzly-data-new-note orgzly-default-book title
                                    :content content)))
    (jetpacs-shell-notify (format "Saved to %s" orgzly-default-book))
    (if orgzly-share-open-note
        (progn (setq orgzly-ui--note-ref ref
                     orgzly-ui--current-book orgzly-default-book)
               (jetpacs-shell-push nil :switch-to "orgzly.note"))
      (jetpacs-shell-push))))

(jetpacs-defaction "share.text" #'orgzly-capture--on-share)

;; The drawer's quick capture into the default notebook — owned, so it
;; rides only Orgzly's drawer once a second app exists.
(with-jetpacs-owner "orgzly"
  (jetpacs-shell-add-drawer-item
   20 (lambda ()
        (jetpacs-drawer-item "add" "New note"
                          (jetpacs-action "orgzly.note.new"
                                       :args `((book . ,orgzly-default-book)))))))

(provide 'orgzly-capture)
;;; orgzly-capture.el ends here
