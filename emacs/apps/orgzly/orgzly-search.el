;;; orgzly-search.el --- Search view & saved searches -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; The dotted-query search screen and Orgzly's saved searches.  A query
;; with an ad.N option renders day-grouped like the agenda; anything else
;; renders as a flat result list showing each hit's book.  Saved searches
;; ship with Orgzly's defaults, are managed from the Searches drill-in
;; (add/rename/edit/reorder/delete), persist through Customize, and feed
;; the agenda tab and the home-screen widget.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-agenda)

(declare-function orgzly-ui--note-row "orgzly-ui")

(defcustom orgzly-saved-searches
  '(("Agenda" . ".it.done ad.7")
    ("Next 3 days" . ".it.done s.ge.today ad.3")
    ("Scheduled" . "s.today .it.done")
    ("To Do" . "i.todo"))
  "Saved searches (NAME . DOTTED-QUERY), Orgzly's defaults preloaded."
  :type '(alist :key-type string :value-type string)
  :group 'orgzly)

(defun orgzly-search-saved-searches ()
  "The saved searches alist (a function so other modules stay decoupled)."
  orgzly-saved-searches)

(defun orgzly-search--persist ()
  (customize-save-variable 'orgzly-saved-searches orgzly-saved-searches))

(defvar orgzly-search--query ""
  "The last submitted query string.")

(defvar orgzly-search--manage nil
  "Non-nil while the Searches management drill-in is open.")

(defconst orgzly-search--result-cap 200
  "Maximum rendered results; a sanity bound on spec size.")

;; ─── The search tab ──────────────────────────────────────────────────────────

(defun orgzly-search--results-nodes ()
  "Result widgets for the current query, or nil when it is empty."
  (let ((q (string-trim orgzly-search--query)))
    (unless (string-empty-p q)
      (condition-case err
          (let* ((query (orgzly-query-parse q))
                 (ctx (orgzly-data-query-context)))
            (if (plist-get query :agenda-days)
                (orgzly-agenda-day-nodes (orgzly-data-entries) query ctx)
              (let* ((hits (orgzly-query-select (orgzly-data-entries) query ctx))
                     (n (length hits))
                     (shown (seq-take hits orgzly-search--result-cap)))
                (cons
                 (jetpacs-section-header
                  (format "%d result%s" n (if (= n 1) "" "s")))
                 (mapcar (lambda (e)
                           (jetpacs-card (list (orgzly-ui--note-row e :show-book t))))
                         shown)))))
        (error
         (list (jetpacs-card
                (list (jetpacs-text (format "Query error: %s"
                                         (error-message-string err))
                                 'body nil "#e53935")))))))))

(defun orgzly-search--body ()
  (apply #'jetpacs-lazy-column
         (append
          (list
           (jetpacs-text-input "orgzly-search-input"
                            :value orgzly-search--query
                            :hint "b.gtd i.todo t.work s.1w …"
                            :label "Search"
                            :on-submit (jetpacs-action "orgzly.search.run")
                            :single-line t)
           (apply #'jetpacs-scroll-row
                  (append
                   (mapcar (lambda (ss)
                             (jetpacs-chip (car ss)
                                        :selected (equal (cdr ss)
                                                         orgzly-search--query)
                                        :on-tap (jetpacs-action
                                                 "orgzly.search.saved"
                                                 :args `((name . ,(car ss)))
                                                 :when-offline "drop")))
                           orgzly-saved-searches)
                   (list (jetpacs-assist-chip "Edit searches" :icon "tune"
                                           :on-tap (jetpacs-action
                                                    "orgzly.search.manage"
                                                    :when-offline "drop"))))))
          (or (orgzly-search--results-nodes)
              (list (jetpacs-empty-state
                     :icon "search" :title "Search your notebooks"
                     :caption "Dotted queries: i.todo t.tag s.today b.book — or free text"))))))

;; ─── Saved searches management drill-in ──────────────────────────────────────

(defun orgzly-search--manage-body ()
  (apply #'jetpacs-lazy-column
         (append
          (mapcar
           (lambda (ss)
             (let ((name (car ss)))
               (jetpacs-card
                (list
                 (jetpacs-row
                  (jetpacs-box
                   (list (jetpacs-column
                          (jetpacs-text name 'body)
                          (jetpacs-text (cdr ss) 'caption nil "#8a8a8a")))
                   :weight 1.0
                   :on-tap (jetpacs-action "orgzly.search.saved"
                                        :args `((name . ,name))
                                        :when-offline "drop"))
                  (jetpacs-menu
                   (list
                    (jetpacs-menu-item "Run" (jetpacs-action "orgzly.search.saved"
                                                       :args `((name . ,name))
                                                       :when-offline "drop")
                                    :icon "play_arrow")
                    (jetpacs-menu-item "Rename…"
                                    (jetpacs-action "orgzly.search.rename"
                                                 :args `((name . ,name)))
                                    :icon "edit")
                    (jetpacs-menu-item "Edit query…"
                                    (jetpacs-action "orgzly.search.edit"
                                                 :args `((name . ,name)))
                                    :icon "manage_search")
                    (jetpacs-menu-item "Move up"
                                    (jetpacs-action "orgzly.search.move"
                                                 :args `((name . ,name) (dir . "up")))
                                    :icon "arrow_upward")
                    (jetpacs-menu-item "Move down"
                                    (jetpacs-action "orgzly.search.move"
                                                 :args `((name . ,name) (dir . "down")))
                                    :icon "arrow_downward")
                    (jetpacs-menu-item "Delete…"
                                    (jetpacs-action "orgzly.search.delete"
                                                 :args `((name . ,name)))
                                    :icon "delete")))
                  :align "center")))))
           orgzly-saved-searches)
          (list (jetpacs-button "Add search"
                             (jetpacs-action "orgzly.search.add")
                             :icon "add" :variant "tonal")))))

(defun orgzly-search--manage-view (snackbar)
  (jetpacs-shell-nav-view "Searches" (orgzly-search--manage-body)
                       :back-to "search"
                       :snackbar snackbar))

;; ─── Views & actions ─────────────────────────────────────────────────────────

(jetpacs-shell-define-view "search"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "search" (orgzly-search--body)
                                  :snackbar snackbar))
  :tab '(:icon "search" :label "Search")
  :order 12)

(jetpacs-shell-define-view "searches"
  :builder #'orgzly-search--manage-view
  :overlay (lambda () orgzly-search--manage)
  :order 24)

(add-hook 'jetpacs-shell-view-switched-hook
          (lambda (&optional _view) (setq orgzly-search--manage nil)))

(jetpacs-defaction "orgzly.search.run"
  (lambda (args _)
    (setq orgzly-search--query (or (alist-get 'value args) ""))
    (jetpacs-shell-push "search")))

(jetpacs-defaction "orgzly.search.saved"
  (lambda (args _)
    (when-let ((ss (assoc (alist-get 'name args) orgzly-saved-searches)))
      (setq orgzly-search--query (cdr ss)
            orgzly-search--manage nil)
      (jetpacs-shell-push "search" :switch-to "search"))))

(jetpacs-defaction "orgzly.search.manage"
  (lambda (_ _)
    (setq orgzly-search--manage t)
    (jetpacs-shell-push nil :switch-to "searches")))

(jetpacs-defaction "orgzly.search.add"
  (lambda (_ _)
    (let* ((name (string-trim (read-string "Search name: ")))
           (query (and (not (string-empty-p name))
                       (string-trim (read-string "Query: "
                                                 orgzly-search--query)))))
      (unless (or (string-empty-p name) (string-empty-p (or query "")))
        (when (assoc name orgzly-saved-searches)
          (setq orgzly-saved-searches
                (assoc-delete-all name orgzly-saved-searches)))
        (setq orgzly-saved-searches
              (append orgzly-saved-searches (list (cons name query))))
        (orgzly-search--persist)
        (jetpacs-shell-notify (format "Saved \"%s\"" name))
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.search.rename"
  (lambda (args _)
    (let* ((name (alist-get 'name args))
           (ss (assoc name orgzly-saved-searches))
           (new (and ss (string-trim (read-string "New name: " name)))))
      (when (and ss (not (string-empty-p new)) (not (equal new name)))
        (setcar ss new)
        (orgzly-search--persist)
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.search.edit"
  (lambda (args _)
    (let* ((name (alist-get 'name args))
           (ss (assoc name orgzly-saved-searches))
           (new (and ss (string-trim (read-string "Query: " (cdr ss))))))
      (when (and ss (not (string-empty-p new)))
        (setcdr ss new)
        (orgzly-search--persist)
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.search.move"
  (lambda (args _)
    (let* ((name (alist-get 'name args))
           (dir (alist-get 'dir args))
           (i (cl-position name orgzly-saved-searches
                           :key #'car :test #'equal))
           (j (and i (if (equal dir "up") (1- i) (1+ i)))))
      (when (and j (>= j 0) (< j (length orgzly-saved-searches)))
        (cl-rotatef (nth i orgzly-saved-searches)
                    (nth j orgzly-saved-searches))
        (orgzly-search--persist)
        (jetpacs-shell-push)))))

(jetpacs-defaction "orgzly.search.delete"
  (lambda (args _)
    (let ((name (alist-get 'name args)))
      (when (y-or-n-p (format "Delete saved search \"%s\"? " name))
        (setq orgzly-saved-searches
              (assoc-delete-all name orgzly-saved-searches))
        (orgzly-search--persist)
        (jetpacs-shell-push)))))

(provide 'orgzly-search)
;;; orgzly-search.el ends here
