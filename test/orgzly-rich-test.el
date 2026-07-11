;;; orgzly-rich-test.el --- ERT tests for the rich renderer & its actions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'orgzly-rich)
(require 'ob-emacs-lisp)

(defun orgzly-rich-test--strings (node)
  "Every string anywhere in spec NODE, flattened."
  (cond ((stringp node) (list node))
        ((vectorp node) (mapcan #'orgzly-rich-test--strings
                                (append node nil)))
        ((consp node) (append (orgzly-rich-test--strings (car node))
                              (orgzly-rich-test--strings (cdr node))))))

(defun orgzly-rich-test--nodes-of-type (node type)
  "Every spec node inside NODE whose (t . TYPE) entry matches TYPE."
  (let (out)
    (cl-labels ((walk (n)
                  (cond ((vectorp n) (mapc #'walk (append n nil)))
                        ((consp n)
                         (when (and (listp (cdr n))
                                    (equal (cdr (assq 't n)) type))
                           (push n out))
                         (walk (car n))
                         (walk (cdr n))))))
      (walk node))
    (nreverse out)))

(defmacro orgzly-rich-test--with-book (content &rest body)
  "Run BODY with a temp `orgzly-directory' holding one book of CONTENT.
Binds `file' to the book's path."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "orgzly-rich" t))
          (orgzly-directory dir)
          (file (expand-file-name "rich.org" dir))
          (org-inhibit-startup t))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           (orgzly-data-invalidate)
           ,@body)
       (dolist (buf (buffer-list))
         (when (and (buffer-file-name buf)
                    (string-prefix-p dir (buffer-file-name buf)))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory dir t))))

;; ─── Rendering ───────────────────────────────────────────────────────────────

(ert-deftest orgzly-rich-spans-and-links ()
  "Emphasis becomes styled spans; links become tappable spans."
  (let* ((nodes (orgzly-rich-body
                 "Some *bold* and [[https://orgmode.org][a link]] here."))
         (strings (orgzly-rich-test--strings nodes)))
    (should (= (length nodes) 1))
    (should (member "bold" strings))
    (should (member "a link" strings))
    (should (member "orgzly.link.open" strings))))

(ert-deftest orgzly-rich-table-native ()
  "Org tables render as native grids: header row, alignment, no cookies."
  (let* ((nodes (orgzly-rich-body "\
| Item | Qty |
|------+-----|
| <l>  | <r> |
| Foo  |   1 |
| Bar  |  22 |"))
         (tables (orgzly-rich-test--nodes-of-type nodes "table"))
         (strings (orgzly-rich-test--strings nodes)))
    (should (= (length tables) 1))
    ;; Cookie row dropped from display; data intact.
    (should-not (member "<l>" strings))
    (should (member "Foo" strings))
    (should (equal (append (alist-get 'aligns (car tables)) nil)
                   '("start" "end")))
    ;; First row group is the header.
    (let ((rows (append (alist-get 'rows (car tables)) nil)))
      (should (alist-get 'header (car rows))))))

(ert-deftest orgzly-rich-results-read-only ()
  "#+RESULTS content folds and carries no edit affordances."
  (let* ((nodes (orgzly-rich-body "\
#+RESULTS:
| a | 1 |
" nil "/tmp/x.org" 0))
         (strings (orgzly-rich-test--strings nodes)))
    (should (member "RESULTS" strings))
    ;; Read-only: no cell edit actions even with file context.
    (should-not (member "orgzly.table.edit" strings))))

(ert-deftest orgzly-rich-src-block-play-gating ()
  "The run header appears only for languages this Emacs can execute."
  (let ((runnable (orgzly-rich-test--strings
                   (orgzly-rich-body
                    "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src"
                    nil "/tmp/x.org" 0)))
        (unknown (orgzly-rich-test--strings
                  (orgzly-rich-body
                   "#+begin_src nosuchlang\nboo\n#+end_src"
                   nil "/tmp/x.org" 0))))
    (should (member "orgzly.babel.execute" runnable))
    (should-not (member "orgzly.babel.execute" unknown))))

(ert-deftest orgzly-rich-checkboxes-interactive ()
  "Checkbox items get tappable toggle icons under file context."
  (let ((strings (orgzly-rich-test--strings
                  (orgzly-rich-body "- [ ] one\n- [X] two"
                                    nil "/tmp/x.org" 0))))
    (should (member "orgzly.checkbox.toggle" strings))
    (should (member "check_box_outline_blank" strings))
    (should (member "check_box" strings))))

(ert-deftest orgzly-rich-block-editing ()
  "Editable blocks carry the tap; the edit-region block becomes an editor."
  (let* ((body "First paragraph.\n\nSecond paragraph.\n")
         (plain (orgzly-rich-body body nil "/tmp/x.org" 0))
         (strings (orgzly-rich-test--strings plain)))
    (should (member "orgzly.note.edit-block" strings))
    ;; Now open the first block (real begin = offset 0 + temp begin 1;
    ;; the element's end includes its post-blank line, temp pos 19).
    (let* ((orgzly-rich--edit-region (cons 1 19))
           (edited (orgzly-rich-body body nil "/tmp/x.org" 0))
           (editors (orgzly-rich-test--nodes-of-type edited "editor")))
      (should (= (length editors) 1))
      (should (equal (alist-get 'value (car editors)) "First paragraph."))
      (should (member "orgzly.note.save-block"
                      (orgzly-rich-test--strings edited))))))

;; ─── Round trips through the real file ───────────────────────────────────────

(defun orgzly-rich-test--call (action args)
  "Invoke ACTION's handler with ARGS, pushes and notifications stubbed."
  (cl-letf (((symbol-function 'jetpacs-shell-push) #'ignore)
            ((symbol-function 'jetpacs-shell-notify) #'ignore))
    (funcall (gethash action jetpacs-action-handlers) args nil)))

(ert-deftest orgzly-rich-table-edit-round-trip ()
  "Cell edits write through org's table machinery; #+TBLFM recalculates."
  (orgzly-rich-test--with-book "\
* Note
| a | b | sum |
|---+---+-----|
| 1 | 2 |   3 |
#+TBLFM: $3=$1+$2
"
    (let ((pos (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (search-forward "| 1")
                 (- (point) 1))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "10")))
        (orgzly-rich-test--call "orgzly.table.edit"
                                `((file . ,file) (pos . ,pos))))
      (let ((s (with-temp-buffer (insert-file-contents file)
                                 (buffer-string))))
        (should (string-match-p "| 10 |" s))
        ;; The formula recomputed the sum column.
        (should (string-match-p "| *12 |" s))))))

(ert-deftest orgzly-rich-table-add-row-and-col ()
  (orgzly-rich-test--with-book "\
* Note
| a | b |
| 1 | 2 |
"
    (let ((pos (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (search-forward "| a")
                 (- (point) 1))))
      (orgzly-rich-test--call "orgzly.table.add-row"
                              `((file . ,file) (pos . ,pos)))
      (orgzly-rich-test--call "orgzly.table.add-col"
                              `((file . ,file) (pos . ,pos)))
      (with-temp-buffer
        (insert-file-contents file)
        ;; 3 table lines now, each with 3 pipes (2 cols + 1).
        (goto-char (point-min))
        (should (= (cl-count-if (lambda (l) (string-prefix-p "|" l))
                                (split-string (buffer-string) "\n"))
                   3))
        (goto-char (point-min))
        (search-forward "| a")
        (should (= (cl-count ?| (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                   4))))))

(ert-deftest orgzly-rich-checkbox-toggle-round-trip ()
  (orgzly-rich-test--with-book "* Note\n- [ ] task\n"
    (let ((pos (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (search-forward "- [ ]")
                 (line-beginning-position))))
      (orgzly-rich-test--call "orgzly.checkbox.toggle"
                              `((file . ,file) (pos . ,pos)))
      (should (string-match-p "- \\[X\\] task"
                              (with-temp-buffer (insert-file-contents file)
                                                (buffer-string)))))))

(ert-deftest orgzly-rich-babel-execute-round-trip ()
  "A phone play-tap runs the block and lands #+RESULTS in the file."
  (orgzly-rich-test--with-book "\
* Note
#+begin_src emacs-lisp
(+ 40 2)
#+end_src
"
    (let ((pos (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (search-forward "#+begin_src")
                 (line-beginning-position)))
          (org-confirm-babel-evaluate nil))
      (orgzly-rich-test--call "orgzly.babel.execute"
                              `((file . ,file) (pos . ,pos)))
      (let ((s (with-temp-buffer (insert-file-contents file)
                                 (buffer-string))))
        (should (string-match-p "#\\+RESULTS:" s))
        (should (string-match-p ": 42" s))))))

(provide 'orgzly-rich-test)
;;; orgzly-rich-test.el ends here
