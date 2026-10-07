;;; orgzly-ui.el --- Orgzly screens built with Compose Foundation -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "30.1"))
;;; Commentary:
;; Builders return the actual Jetpacs IR and read only the published snapshot.
;;; Code:
(require 'orgzly-state)
(require 'orgzly-design)
(require 'jetpacs-chrome)

(defvar orgzly-field-scope nil
  "Dynamic identity namespace of the current form, shared by its capture handler.")

(defun orgzly-field-scope (route)
  "Return the stable form namespace for ROUTE and its revision-bound identity."
  (jetpacs-wire-id (symbol-name (plist-get route :kind))
                   (or (plist-get route :id) (plist-get route :parent-id)
                       (plist-get route :book-id) "form")))

(defun orgzly-field-id (id)
  "Qualify logical field ID with the current form's identity namespace."
  (if orgzly-field-scope (jetpacs-wire-id orgzly-field-scope id) id))

(defconst orgzly-ui-fields '("oz-title" "oz-state" "oz-priority" "oz-tags"
                            "oz-scheduled" "oz-deadline" "oz-content")
  "Controlled note fields captured by the single atomic save action.")

(defun orgzly-ui-action (name &optional args confirm fields)
  "Build NAME with opaque ARGS, optional CONFIRM and captured FIELDS."
  (jetpacs-action name :args args :when-offline "drop" :confirm confirm :capture-fields (mapcar #'orgzly-field-id fields)))

(defun orgzly-ui-icon (icon label action)
  "Return a labeled Foundation ICON button for ACTION with accessible LABEL."
  (jetpacs-icon-button icon action :content-description label))

(defun orgzly-ui-navigation ()
  "Return the app's compact, horizontally scrollable destinations."
  (apply #'jetpacs-row
         (append
          (mapcar (lambda (pair)
                    (jetpacs-chip (car pair)
                                  :on-tap (orgzly-ui-action "orgzly.open" (list :view (cdr pair)))))
                  '(("Notebooks" . "books") ("Agenda" . "agenda")
                    ("Search" . "search") ("Settings" . "settings")))
          '(:scroll t :spacing 4))))

(defun orgzly-ui--metadata (entry show-book)
  "Return ENTRY's planning caption; SHOW-BOOK prefixes its notebook name."
  (string-join
   (delq nil
         (list (and show-book (alist-get 'book entry))
               (when (alist-get 'scheduled entry)
                 (concat "◷ " (alist-get 'raw (alist-get 'scheduled entry))))
               (when (alist-get 'deadline entry)
                 (concat "⚑ " (alist-get 'raw (alist-get 'deadline entry))))))
   "   "))

(defun orgzly-ui-note-row (entry &optional show-book)
  "Build ENTRY's compact outline row; SHOW-BOOK adds its notebook name."
  (let* ((id (alist-get 'id entry)) (state (alist-get 'state entry))
         (done (member state (alist-get 'done (plist-get orgzly-state :ctx))))
         (level (min 8 (max 0 (1- (alist-get 'level entry)))))
         (metadata (orgzly-ui--metadata entry show-book))
         (spans (append
                 (when state (list (jetpacs-span (concat state " ") :font-weight "bold"
                                                :color (if done "success" "error"))))
                 (when-let* ((priority (alist-get 'priority entry)))
                   (list (jetpacs-span (concat "[#" priority "] ") :font-weight "bold")))
                 (list (jetpacs-span (alist-get 'title entry)))
                 (when (alist-get 'tags entry)
                   (list (jetpacs-span (concat "  :" (string-join (alist-get 'tags entry) ":") ":")
                                       :color "secondary")))))
         (face (apply #'jetpacs-column
                      (append (list (jetpacs-rich-text spans))
                              (unless (string-empty-p metadata)
                                (list (jetpacs-text metadata :style "caption" :color "secondary")))
                              (when (and (not (string-empty-p (alist-get 'content entry)))
                                         (not (member id orgzly-state-folded)))
                                (list (jetpacs-text
                                       (truncate-string-to-width (alist-get 'content entry) 200 nil nil "…")
                                       :style "caption" :max-lines 2 :syntax "org")))
                              '(:spacing 2)))))
    (jetpacs-with-attrs
     (jetpacs-column
      (jetpacs-row
       (jetpacs-spacer :width (* 12 level))
       (jetpacs-with-attrs
        (jetpacs-design-pressable (if done '("orgzly.row" "orgzly.done") '("orgzly.row"))
                                 (orgzly-ui-action "orgzly.note.open" (list :id id)) (list face))
        :weight 1)
       (orgzly-ui-icon
        (if (alist-get 'has-children entry)
            (if (member id orgzly-state-folded) "chevron_right" "expand_more")
          "more_vert")
        (if (alist-get 'has-children entry) "Fold or unfold note" "Note actions")
        (orgzly-ui-action (if (alist-get 'has-children entry) "orgzly.note.fold" "orgzly.note.open")
                          (list :id id)))
       :align "center")
      (jetpacs-divider))
     :key id)))

(defun orgzly-ui-note-row-template ()
  "Build the one outline row the Companion expands per `/rows' item.
SPEC 13.8.5: every varying value is a relative `jetpacs-ref' into the
item `orgzly-ui-outline-row' builds, and the shape never varies, so a
fold, a page turn or an insert is one `surface.data' patch of `/rows'
instead of a screen rebuild.  The tap handlers read the item's id from
the injected `item' argument.  The inline content preview of
`orgzly-ui-note-row' has no fixed-shape form and is not shown here."
  (jetpacs-template "/rows"
    (jetpacs-with-attrs
     (jetpacs-column
      (jetpacs-row
       (jetpacs-spacer :width (jetpacs-ref "indent"))
       (jetpacs-with-attrs
        (jetpacs-design-pressable
         (jetpacs-ref "styles")
         (orgzly-ui-action "orgzly.note.open")
         (list (jetpacs-column
                (jetpacs-row
                 (jetpacs-text (jetpacs-ref "state") :font-weight "bold"
                               :color (jetpacs-ref "state_color"))
                 (jetpacs-text (jetpacs-ref "priority") :font-weight "bold")
                 (jetpacs-text (jetpacs-ref "title"))
                 (jetpacs-text (jetpacs-ref "tags") :color "secondary")
                 :spacing 4)
                (jetpacs-text (jetpacs-ref "metadata") :style "caption" :color "secondary")
                :spacing 2)))
        :weight 1)
       (jetpacs-icon-button (jetpacs-ref "icon") (orgzly-ui-action "orgzly.note.tap")
                            :content-description (jetpacs-ref "icon_label"))
       :align "center")
      (jetpacs-divider))
     :key (jetpacs-ref "id"))))

(defun orgzly-ui-outline-row (entry)
  "Return ENTRY's `/rows' item: the values `orgzly-ui-note-row-template' binds."
  (let* ((id (alist-get 'id entry)) (state (alist-get 'state entry))
         (done (member state (alist-get 'done (plist-get orgzly-state :ctx))))
         (level (min 8 (max 0 (1- (alist-get 'level entry)))))
         (children (alist-get 'has-children entry))
         (row (make-hash-table :test #'equal :size 12)))
    (puthash "id" id row)
    (puthash "indent" (* 12 level) row)
    (puthash "styles" (if done ["orgzly.row" "orgzly.done"] ["orgzly.row"]) row)
    (puthash "state" (if state (concat state " ") "") row)
    (puthash "state_color" (if done "success" "error") row)
    (puthash "priority" (if-let* ((priority (alist-get 'priority entry)))
                            (format "[#%s] " priority) "")
             row)
    (puthash "title" (alist-get 'title entry) row)
    (puthash "tags" (if (alist-get 'tags entry)
                        (format "  :%s:" (string-join (alist-get 'tags entry) ":")) "")
             row)
    (puthash "metadata" (orgzly-ui--metadata entry nil) row)
    (puthash "icon" (if children
                        (if (member id orgzly-state-folded) "chevron_right" "expand_more")
                      "more_vert")
             row)
    (puthash "icon_label" (if children "Fold or unfold note" "Note actions") row)
    row))

(defun orgzly-ui-page-window (count)
  "Return (PAGE START END) of the active page over COUNT entries."
  (let* ((page (min orgzly-state-page (max 0 (/ (1- count) orgzly-state-page-size))))
         (start (* page orgzly-state-page-size)))
    (list page start (min count (+ start orgzly-state-page-size)))))

(defun orgzly-ui-pager (count)
  "Return the Previous/Next row when COUNT exceeds one page, else nil."
  (pcase-let ((`(,page ,start ,end) (orgzly-ui-page-window count)))
    (when (> count orgzly-state-page-size)
      (jetpacs-row
       (jetpacs-button "Previous" (orgzly-ui-action "orgzly.page" (list :page (max 0 (1- page)))))
       (jetpacs-text (format "%d–%d of %d" (1+ start) end count) :style "caption")
       (jetpacs-button "Next" (orgzly-ui-action "orgzly.page" (list :page (1+ page))))
       :spacing 8 :align "center"))))

(defun orgzly-ui-page (entries &optional show-book)
  "Return bounded page nodes for ENTRIES, optionally showing SHOW-BOOK."
  (pcase-let ((`(,_ ,start ,end) (orgzly-ui-page-window (length entries))))
    (append
     (mapcar (lambda (entry) (orgzly-ui-note-row entry show-book)) (cl-subseq entries start end))
     (when-let* ((pager (orgzly-ui-pager (length entries)))) (list pager)))))

(defun orgzly-ui-outline-rows (name)
  "Return the `/rows' array for NAME's visible outline at the active page."
  (let ((entries (orgzly-state-visible (orgzly-state-entries name))))
    (pcase-let ((`(,_ ,start ,end) (orgzly-ui-page-window (length entries))))
      (vconcat (mapcar #'orgzly-ui-outline-row (cl-subseq entries start end))))))

(defun orgzly-ui-document ()
  "Return the surface data document (SPEC 13.8): the open outline's rows.
A deterministic reader of the snapshot and the fold state, like every
builder; `orgzly-state-outline' names the notebook whose outline screen
is on the stack, or nil for an empty array."
  (list :rows (if orgzly-state-outline (orgzly-ui-outline-rows orgzly-state-outline) [])))

(defun orgzly-ui-books ()
  "Return the notebook list from the published snapshot."
  (append
   (list (orgzly-ui-navigation))
   (mapcar
    (lambda (book)
      (jetpacs-column
       (jetpacs-component-list-item
        (alist-get 'title book) :leading (jetpacs-icon "description")
        :subtitle (format "%s.org   ·   %d notes" (alist-get 'name book) (alist-get 'count book))
        :trailing (jetpacs-icon "chevron_right")
        :on-tap (orgzly-ui-action "orgzly.book.open" (list :id (alist-get 'id book))))
       (jetpacs-divider)))
    (plist-get orgzly-state :books))
   (unless (plist-get orgzly-state :books)
     (list (jetpacs-text "No notebooks yet. Create one to start writing.")))
   (list (jetpacs-button "New notebook" (orgzly-ui-action "orgzly.open" '(:view "new-book")) :icon "add"))))

(defun orgzly-ui-book (name)
  "Return NAME's outline, fold state and notebook actions from the snapshot."
  (let ((book (orgzly-state-book name)))
    (if (null book) (list (jetpacs-text "This notebook is no longer available. Refresh to continue."))
      (append
       (list (jetpacs-row
              (jetpacs-button "New note" (orgzly-ui-action "orgzly.note.new" (list :id (alist-get 'id book))) :icon "add")
              (jetpacs-button "Notebook details"
                               (orgzly-ui-action "orgzly.book.edit" (list :id (alist-get 'id book))) :variant "text")
              :scroll t :spacing 8))
       ;; The rows are a template over the document (SPEC 13.8.5); the
       ;; document carries this page of the visible outline.
       (list (jetpacs-with-attrs
              (jetpacs-lazy-column (orgzly-ui-note-row-template) :spacing 4)
              :weight 1))
       (when-let* ((pager (orgzly-ui-pager
                           (length (orgzly-state-visible (orgzly-state-entries name))))))
         (list pager))
       (unless (orgzly-state-entries name) (list (jetpacs-text "This notebook is empty.")))))))

(defun orgzly-ui-search ()
  "Return the dotted-query search field, saved queries and bounded results."
  (append
   (list (orgzly-ui-navigation)
         (jetpacs-text-input (orgzly-field-id "oz-query") :value orgzly-state-query :label "Search notebooks"
                             :hint "i.todo t.tag s.today b.book — or text" :single-line t
                             :on-submit (orgzly-ui-action "orgzly.search.run" nil nil '("oz-query")))
         (jetpacs-row
          (jetpacs-button "Search" (orgzly-ui-action "orgzly.search.run" nil nil '("oz-query")))
          (jetpacs-button "Save search" (orgzly-ui-action "orgzly.open" '(:view "save-search")) :variant "text")
          :spacing 8))
   (mapcar (lambda (search)
             (jetpacs-component-list-item
              (car search) :subtitle (cdr search) :leading (jetpacs-icon "search")
              :on-tap (orgzly-ui-action "orgzly.search.saved" (list :name (car search)))
              :trailing (orgzly-ui-icon "delete" "Delete saved search"
                           (orgzly-ui-action "orgzly.search.delete" (list :name (car search)) "Delete this saved search?"))))
           (plist-get orgzly-state :searches))
   (unless (string-empty-p orgzly-state-query)
     (let ((query (orgzly-query-parse orgzly-state-query)))
       (if (plist-get query :agenda-days) (orgzly-ui-agenda query)
         (or (orgzly-ui-page (orgzly-query-select (orgzly-state-entries nil) query
                                                 (plist-get orgzly-state :ctx)) t)
             (list (jetpacs-text "No matching notes."))))))))

(defun orgzly-ui-agenda (&optional query)
  "Return bounded day sections for QUERY using the snapshot clock."
  (let ((sections (orgzly-agenda-sections
                   (orgzly-state-entries nil) (or query (orgzly-query-parse ".it.done ad.7"))
                   (plist-get orgzly-state :ctx) (plist-get orgzly-state :now)))
        (remaining orgzly-state-page-size) nodes)
    (dolist (section sections)
      (when (and (> remaining 0) (cdr section))
        (push (jetpacs-section-header
               (if (eq (car section) 'overdue) "Overdue"
                 (format-time-string "%A, %b %-d" (seconds-to-time (car section))))) nodes)
        (dolist (item (cdr section))
          (when (> remaining 0)
            (let ((row (orgzly-ui-note-row (alist-get 'entry item) t)))
              ;; One heading may occur on several days; lazy keys are occurrence-owned.
              (setq row (plist-put row :key (format "occ-%d" remaining)))
              (push row nodes))
            (setq remaining (1- remaining))))))
    (when (= remaining 0) (push (jetpacs-text "Showing the first 40 occurrences. Narrow the query for more." :style "caption") nodes))
    (or (nreverse nodes) (list (jetpacs-text "Nothing scheduled.")))))

(defun orgzly-ui-field (id label value &optional multiline)
  "Build field ID with LABEL and VALUE; MULTILINE enables the Org content area."
  (jetpacs-text-input (orgzly-field-id id) :label label :value (or value "")
                      :single-line (if multiline :json-false t)
                      :min-lines (if multiline 6 1) :max-lines (if multiline 16 1)
                      :max-length (if multiline 16000 512)
                      :monospace (and multiline t) :syntax (and multiline "org")))

(defun orgzly-ui-note-form (route)
  "Build the atomic note editor or new-note form described by ROUTE."
  (let* ((id (plist-get route :id))
         (entry (and id (ignore-errors (orgzly-state-resolve id))))
         (book (or (alist-get 'book entry) (plist-get route :book)))
         (args (if id (list :id id) (list :book-id (plist-get route :book-id)
                                                :parent-id (plist-get route :parent-id)))))
    (if (and id (null entry)) (list (jetpacs-text "This note changed. Reopen it from its notebook."))
      (append
       (list (jetpacs-text (concat "Notebook  ›  " book) :style "caption")
             (orgzly-ui-field "oz-title" "Title" (alist-get 'title entry))
             (orgzly-ui-field "oz-state" "State" (alist-get 'state entry))
             (orgzly-ui-field "oz-priority" "Priority (A–C)" (alist-get 'priority entry))
             (orgzly-ui-field "oz-tags" "Tags" (string-join (alist-get 'tags entry) " "))
             (orgzly-ui-field "oz-scheduled" "Scheduled (Org timestamp)" (alist-get 'raw (alist-get 'scheduled entry)))
             (orgzly-ui-field "oz-deadline" "Deadline (Org timestamp)" (alist-get 'raw (alist-get 'deadline entry)))
             (orgzly-ui-field "oz-content" "Content" (alist-get 'content entry) t)
             (jetpacs-button "Save note" (orgzly-ui-action "orgzly.note.save" args nil orgzly-ui-fields) :icon "save"))
       (when entry
         (list
          (jetpacs-section-header "Note actions")
          (apply #'jetpacs-row
                 (append
                  (mapcar (lambda (item)
                            (jetpacs-button (car item)
                                             (orgzly-ui-action "orgzly.note.operate" (list :id id :operation (cdr item))
                                                               (and (equal (cdr item) "delete") "Delete this note and its children?"))
                                             :variant "text"))
                          '(("Done / TODO" . "toggle") ("Promote" . "promote") ("Demote" . "demote")
                            ("Move up" . "up") ("Move down" . "down") ("Copy" . "copy")
                            ("Paste below" . "paste") ("Delete" . "delete")))
                  '(:scroll t :spacing 4)))
          (jetpacs-button "New child note" (orgzly-ui-action "orgzly.note.child" (list :id id)) :variant "text")))))))

(defun orgzly-ui-book-form (route)
  "Build notebook metadata form for ROUTE, with explicit destructive confirmation."
  (let* ((id (plist-get route :id)) (book (and id (ignore-errors (orgzly-state-resolve id)))))
    (if (and id (null book)) (list (jetpacs-text "This notebook changed. Reopen its details."))
      (append
       (list (orgzly-ui-field "oz-name" "Notebook filename" (alist-get 'name book))
             (orgzly-ui-field "oz-preface" "Preface, #+TITLE and #+FILETAGS" (alist-get 'preface book) t)
             (jetpacs-button "Save notebook" (orgzly-ui-action "orgzly.book.save" (and id (list :id id)) nil
                                                               '("oz-name" "oz-preface"))))
       (when id (list (jetpacs-button "Delete notebook"
                                      (orgzly-ui-action "orgzly.book.delete" (list :id id)
                                                        "Delete this notebook and every note in it?") :variant "text")))))))

(defun orgzly-ui-screen (route &optional back)
  "Render ROUTE and BACK from explicit app state; perform no reads or mutations."
  (let* ((kind (plist-get route :kind))
         (orgzly-field-scope (orgzly-field-scope route))
         (title (pcase kind ('books "Notebooks") ('book (plist-get route :book))
                        ('note "Note") ('new-note "New note") ('search "Search")
                        ('agenda "Agenda") ('settings "Settings") (_ "Orgzly")))
         (nodes
          (pcase kind
            ('books (orgzly-ui-books)) ('book (orgzly-ui-book (plist-get route :book)))
            ((or 'note 'new-note) (orgzly-ui-note-form route))
            ((or 'new-book 'book-edit) (orgzly-ui-book-form route))
            ('search (orgzly-ui-search))
            ('agenda (cons (orgzly-ui-navigation) (orgzly-ui-agenda)))
            ('save-search (list (orgzly-ui-field "oz-name" "Search name" "")
                                (orgzly-ui-field "oz-query" "Dotted query" orgzly-state-query)
                                (jetpacs-button "Save search" (orgzly-ui-action "orgzly.search.save" nil nil '("oz-name" "oz-query")))))
            ('settings (list (orgzly-ui-navigation)
                              (jetpacs-text "Orgzly Native" :style "title")
                              (jetpacs-text "Plain Org files. Foundation components. Styles you can change.")
                              (jetpacs-button (if orgzly-state-dark "Use light appearance" "Use dark appearance")
                                               (orgzly-ui-action "orgzly.theme"))
                              (jetpacs-text "Notebook and Org defaults are configured in Emacs through the orgzly Customize group." :style "caption")))
            (_ (list (jetpacs-text "Unknown Orgzly screen.")))))
         (body (cond
                ;; The outline's rows are their own lazy column; the
                ;; toolbar and pager sit around it in a plain column.
                ((eq kind 'book) (apply #'jetpacs-column (append nodes '(:spacing 4))))
                (t (let ((list-node (apply #'jetpacs-lazy-column (append nodes '(:spacing 4)))))
                     (if (memq kind '(note new-note new-book book-edit save-search settings search))
                         (jetpacs-with-attrs list-node :padding 16)
                       list-node))))))
    (let* ((screen (jetpacs-chrome-screen
                    title body :back back :scaffold '(:top-bar-style nil)
                    :actions (list (orgzly-ui-icon "refresh" "Refresh notebooks"
                                                   (orgzly-ui-action "orgzly.refresh")))))
           (bar (plist-get screen :top_bar)))
      ;; Keep the direct leading arrow spine for native system-back handling.
      ;; Only the generic scaffold supplies insets; this bar is a Foundation row.
      (cl-loop for child across (plist-get bar :children)
               when (member (plist-get child :t) '("text" "icon_button"))
               do (plist-put child :color "#FFFFFF"))
      (setq screen (plist-put screen :top_bar
                              (jetpacs-with-attrs bar :bg (if orgzly-state-dark "#292929" "#546E7A")
                                                     :height 56)))
      (orgzly-design-wrap screen orgzly-state-dark))))

(provide 'orgzly-ui)
;;; orgzly-ui.el ends here
