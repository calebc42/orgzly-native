;;; orgzly.el --- Orgzly Revived in elisp (Jetpacs Tier-1 app), single-file bundle -*- lexical-binding: t; -*-
;;
;; GENERATED FILE -- do not edit by hand.
;; Produced by emacs/build-bundle.el from the emacs/apps/orgzly/ sources.
;; Concatenated in dependency order; each part keeps its own `provide',
;; so the inter-file `require' forms resolve within this file.
;;
;; Requires the Jetpacs core (jetpacs-core.el) on `load-path' first.
;;
;;; Code:

(require 'jetpacs-core)

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-query.el
;;; ==================================================================

;;; orgzly-query.el --- Orgzly dotted query language in elisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; A faithful port of Orgzly Revived's dotted search language
;; (QueryTokenizer/QueryParser/DottedQueryParser/SqliteQueryBuilder) that
;; parses and evaluates queries in pure elisp against the entry alists
;; produced by orgzly-data.el.
;;
;; The language, in one breath:
;;   b.BOOK i.STATE it.todo|done|none p.X ps.X t.TAG tn.TAG
;;   s|d|e|c|cr[.REL].INTERVAL   o.KEY   ad.N   free text  "quoted text"
;;   ( ... )  and / or            .PREFIX negates a condition or flips a sort
;; INTERVAL: none now today tomorrow yesterday  or  [-+]N h|d|w|m|y
;; REL: eq ne lt le gt ge (default: eq for c/e, le for s/d/cr)
;;
;; Parse result: a plist (:cond COND :orders ORDERS :agenda-days N).
;; COND is nil or a tree of
;;   (and COND...) (or COND...)
;;   (book NAME NOT) (state STATE NOT) (state-type TYPE NOT)
;;   (priority P NOT) (set-priority P NOT) (tag TAG NOT) (own-tag TAG NOT)
;;   (time KIND REL (UNIT . VALUE))       ; KIND scheduled|deadline|event|closed|created
;;   (text STRING QUOTED)
;; ORDERS is a list of (KEY . DESC) with KEY in
;;   book|title|scheduled|deadline|event|closed|created|priority|state|position.
;;
;; Known deliberate divergence from upstream (documented in docs/PARITY.md):
;; upstream's `ne' relation compiles to an unsatisfiable SQL AND; here it
;; means "outside the period", the evident intent.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; ─── Tokenizer ───────────────────────────────────────────────────────────────

(defconst orgzly-query--token-re
  (let* ((ch "[^\")([:space:]]")
         (dq "\"[^\"\\\\]*\\(?:\\\\.[^\"\\\\]*\\)*\""))
    (concat ch "*" dq        ; optionally-prefixed double-quoted (t."my tag")
            "\\|(" "\\|)"    ; group open / close
            "\\|" ch "+"))   ; anything else up to whitespace/quote/paren
  "Token pattern mirroring Orgzly's QueryTokenizer.")

(defun orgzly-query--tokenize (str)
  "Split STR into dotted-query tokens."
  (let ((pos 0) tokens)
    (while (string-match orgzly-query--token-re str pos)
      (push (match-string 0 str) tokens)
      (setq pos (max (match-end 0) (1+ (match-beginning 0)))))
    (nreverse tokens)))

(defun orgzly-query--unquote (s)
  "Strip one level of double quotes and backslash escapes from S.
Mirrors QueryTokenizer.unquote: only acts when S both starts and ends
with a double quote."
  (if (or (< (length s) 2)
          (not (eq (aref s 0) ?\"))
          (not (eq (aref s (1- (length s))) ?\")))
      s
    (let ((out (make-string 0 0)) (quote nil))
      (dotimes (i (- (length s) 2))
        (let ((c (aref s (1+ i))))
          (if (and (eq c ?\\) (not quote))
              (setq quote t)
            (setq quote nil)
            (setq out (concat out (string c))))))
      out)))

;; ─── Interval parsing ────────────────────────────────────────────────────────

(defun orgzly-query--parse-interval (str)
  "Parse STR as a query interval, returning (UNIT . VALUE) or nil.
UNIT is one of the symbols none/now/hour/day/week/month/year."
  (let ((s (downcase str)))
    (cond
     ((member s '("none" "no"))          '(none . 0))
     ((equal s "now")                    '(now . 0))
     ((member s '("today" "tod"))        '(day . 0))
     ((member s '("tomorrow" "tmrw" "tom")) '(day . 1))
     ((equal s "yesterday")              '(day . -1))
     ((string-match "\\`\\([-+]?[0-9]+\\)\\([hdwmy]\\)\\'" s)
      (cons (pcase (match-string 2 s)
              ("h" 'hour) ("d" 'day) ("w" 'week) ("m" 'month) ("y" 'year))
            (string-to-number (match-string 1 s)))))))

;; ─── Token classification ────────────────────────────────────────────────────

(defun orgzly-query--parse-condition-token (token)
  "Parse TOKEN as a condition, or return nil.
The regexes and their order mirror DottedQueryParser: all are
prefix-anchored (upstream uses Regex.find), so trailing garbage after a
match is ignored exactly as upstream ignores it."
  (let ((case-fold-search nil))
    (cond
     ((string-match "\\`\\(\\.\\)?b\\.\\(.+\\)" token)
      (list 'book (orgzly-query--unquote (match-string 2 token))
            (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?i\\.\\(.+\\)" token)
      (list 'state (orgzly-query--unquote (match-string 2 token))
            (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?it\\.\\(todo\\|done\\|none\\)" token)
      (list 'state-type (intern (match-string 2 token))
            (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?p\\.\\([a-zA-Z]\\)" token)
      (list 'priority (match-string 2 token) (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?ps\\.\\([a-zA-Z]\\)" token)
      (list 'set-priority (match-string 2 token) (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?t\\.\\(.+\\)" token)
      (list 'tag (orgzly-query--unquote (match-string 2 token))
            (and (match-string 1 token) t)))
     ((string-match "\\`\\(\\.\\)?tn\\.\\(.+\\)" token)
      (list 'own-tag (orgzly-query--unquote (match-string 2 token))
            (and (match-string 1 token) t)))
     ((string-match
       "\\`\\(e\\|s\\|d\\|c\\|cr\\)\\(?:\\.\\(eq\\|ne\\|lt\\|le\\|gt\\|ge\\)\\)?\\.\\(.+\\)"
       token)
      (let* ((kind-str (match-string 1 token))
             (rel-str (match-string 2 token))
             (interval (orgzly-query--parse-interval
                        (orgzly-query--unquote (match-string 3 token))))
             (kind (pcase kind-str
                     ("e" 'event) ("s" 'scheduled) ("d" 'deadline)
                     ("c" 'closed) ("cr" 'created)))
             (rel (if rel-str (intern rel-str)
                    ;; Default when no relation given, as upstream.
                    (if (memq kind '(closed event)) 'eq 'le))))
        (and interval (list 'time kind rel interval)))))))

(defconst orgzly-query--sort-keys
  '(("notebook\\|book\\|b"                 . book)
    ("title\\|t"                           . title)
    ("scheduled\\|sched\\|s"               . scheduled)
    ("deadline\\|dead\\|d"                 . deadline)
    ("event\\|e"                           . event)
    ("closed\\|close\\|c"                  . closed)
    ("created\\|cr"                        . created)
    ("priority\\|prio\\|pri\\|p"           . priority)
    ("state\\|st"                          . state)
    ("position\\|pos"                      . position))
  "Sort-order name alternatives, in upstream's match order.")

(defun orgzly-query--parse-sort-token (token)
  "Parse TOKEN as a sort order (KEY . DESC), or return nil."
  (let ((case-fold-search nil))
    (cl-loop for (alts . key) in orgzly-query--sort-keys
             when (string-match (format "\\`\\(\\.\\)?o\\.\\(?:%s\\)\\'" alts)
                                token)
             return (cons key (and (match-string 1 token) t)))))

(defun orgzly-query--parse-option-token (token)
  "Parse TOKEN as an option, returning (agenda-days . N) or nil."
  (when (string-match "\\`ad\\.\\([0-9]+\\)\\'" token)
    (let ((days (string-to-number (match-string 1 token))))
      (when (> days 0) (cons 'agenda-days days)))))

;; ─── Parser ──────────────────────────────────────────────────────────────────

(defvar orgzly-query--tokens nil "Remaining token list while parsing.")
(defvar orgzly-query--orders nil "Sort orders collected while parsing.")
(defvar orgzly-query--agenda-days nil "ad.N option collected while parsing.")

(defun orgzly-query-parse (str)
  "Parse dotted query STR into (:cond COND :orders ORDERS :agenda-days N)."
  (let ((orgzly-query--tokens (orgzly-query--tokenize (or str "")))
        (orgzly-query--orders nil)
        (orgzly-query--agenda-days nil))
    (let ((cond (orgzly-query--parse-expression)))
      (list :cond cond
            :orders (nreverse orgzly-query--orders)
            :agenda-days orgzly-query--agenda-days))))

(defun orgzly-query--parse-expression (&rest initial)
  "Parse tokens into a condition tree, starting from INITIAL members.
A direct port of QueryParser.parseExpression, including the implicit-AND
inside OR regrouping (AND binds tighter than OR)."
  (let ((members (copy-sequence initial))
        (operator 'and)
        (last-was-condition nil)
        (done nil))
    (cl-flet ((add-condition
                (c)
                (if (and last-was-condition (eq operator 'or))
                    ;; c1 OR c2 OR c3 c4  =>  OR(c1 c2 AND(c3 c4 ...))
                    (let ((prev (car (last members))))
                      (setq members (butlast members))
                      (let ((sub (orgzly-query--parse-expression prev c)))
                        (when sub (setq members (append members (list sub))))))
                  (setq members (append members (list c))))
                (setq last-was-condition t)))
      (while (and (not done) orgzly-query--tokens)
        (let ((token (pop orgzly-query--tokens)))
          (cond
           ((equal token "(")
            (let ((sub (orgzly-query--parse-expression)))
              (when sub (setq members (append members (list sub)))))
            (setq last-was-condition nil))
           ((equal token ")")
            (setq done t))
           ((member token '("and" "AND"))
            (when (> (length members) 0)
              (when (eq operator 'or)
                ;; c1 OR c2 OR c3 AND …  =>  OR(c1 c2 AND(c3 …))
                (let ((prev (car (last members))))
                  (setq members (butlast members))
                  (let ((sub (orgzly-query--parse-expression prev)))
                    (when sub (setq members (append members (list sub)))))))
              (setq last-was-condition nil)))
           ((member token '("or" "OR"))
            (when (> (length members) 0)
              (when (and (eq operator 'and) (> (length members) 1))
                ;; c1 c2 c3 OR …  =>  OR(AND(c1 c2 c3) …)
                (setq members (list (cons 'and members))))
              (setq operator 'or)
              (setq last-was-condition nil)))
           (t
            (let ((c (orgzly-query--parse-condition-token token)))
              (if c (add-condition c)
                (let ((o (orgzly-query--parse-sort-token token)))
                  (if o (push o orgzly-query--orders)
                    (let ((opt (orgzly-query--parse-option-token token)))
                      (if opt (setq orgzly-query--agenda-days (cdr opt))
                        ;; Plain text (unless empty after unquoting).
                        (let ((unq (orgzly-query--unquote token)))
                          (unless (string-empty-p unq)
                            (add-condition
                             (list 'text unq
                                   (not (equal unq token))))))))))))))))
      (when members
        (cons (if (eq operator 'or) 'or 'and) members)))))

;; ─── Time boundaries ─────────────────────────────────────────────────────────

(defun orgzly-query--boundary (interval &optional plus-one now)
  "Float-time boundary for INTERVAL (UNIT . VALUE), truncated per unit.
PLUS-ONE returns the exclusive end of the unit period instead (mirrors
TimeUtils.timeFromNow with addOneMore). NOW overrides the current time
for tests."
  (let* ((now (or now (current-time)))
         (unit (car interval))
         (value (cdr interval))
         (d (decode-time now))
         (sec (nth 0 d)) (min (nth 1 d)) (hour (nth 2 d))
         (day (nth 3 d)) (mon (nth 4 d)) (year (nth 5 d)))
    (pcase unit
      ('now (+ (float-time now) (if plus-one 0.001 0)))
      ('hour (float-time
              (encode-time 0 0 (+ hour value (if plus-one 1 0)) day mon year)))
      ('day (float-time
             (encode-time 0 0 0 (+ day value (if plus-one 1 0)) mon year)))
      ('week (float-time
              (encode-time 0 0 0 (+ day (* 7 value) (if plus-one 1 0)) mon year)))
      ('month (float-time
               (encode-time 0 0 0 (if plus-one 2 1) (+ mon value) year)))
      ('year (float-time
              (encode-time 0 0 0 (if plus-one 2 1) 1 (+ year value))))
      (_ (error "orgzly-query: no boundary for unit %s" unit)))))

(defun orgzly-query--time-in-relation-p (ts rel interval now)
  "Non-nil when float-time TS satisfies REL against INTERVAL at NOW.
TS nil means the entry has no such timestamp."
  (if (eq (car interval) 'none)
      (null ts)
    (and ts
         (let ((b (orgzly-query--boundary interval nil now))
               (b1 (orgzly-query--boundary interval t now)))
           (pcase rel
             ('eq (and (<= b ts) (< ts b1)))
             ;; Upstream compiles NE to an unsatisfiable AND; we implement
             ;; the evident intent: outside the period.
             ('ne (or (< ts b) (>= ts b1)))
             ('lt (< ts b))
             ('le (< ts b1))
             ('gt (>= ts b1))
             ('ge (>= ts b)))))))

(defun orgzly-query--events-in-relation-p (events rel interval now)
  "Match EVENTS (a list of float times) against REL/INTERVAL at NOW.
Mirrors upstream's use of the note's earliest event start and latest
event end (here: min and max of EVENTS)."
  (if (eq (car interval) 'none)
      (null events)
    (and events
         (let ((emin (apply #'min events))
               (emax (apply #'max events)))
           (pcase rel
             ('eq (and (orgzly-query--time-in-relation-p emin 'ge interval now)
                       (orgzly-query--time-in-relation-p emax 'le interval now)))
             ('ne (and (orgzly-query--time-in-relation-p emin 'lt interval now)
                       (orgzly-query--time-in-relation-p emax 'gt interval now)))
             ((or 'lt 'le) (orgzly-query--time-in-relation-p emin rel interval now))
             ((or 'gt 'ge) (orgzly-query--time-in-relation-p emax rel interval now)))))))

;; ─── Entry accessors ─────────────────────────────────────────────────────────
;;
;; Entries are the alists produced by `orgzly-data-entries':
;;   book file pos level title state priority tags itags
;;   scheduled deadline closed created events content
;; Timestamp values are alists ((time . FLOAT) (has-time . BOOL)
;; (raw . STR) (repeater . STR)); `events' is a list of those.

(defun orgzly-query--entry-time (entry kind)
  "Float time of ENTRY's KIND timestamp, or nil."
  (alist-get 'time (alist-get kind entry)))

(defun orgzly-query--entry-events (entry)
  "List of float times of ENTRY's plain active timestamps."
  (delq nil (mapcar (lambda (ts) (alist-get 'time ts))
                    (alist-get 'events entry))))

;; ─── Matcher ─────────────────────────────────────────────────────────────────

(defun orgzly-query--substring-p (needle haystack)
  "Case-insensitive substring test, nil-safe on HAYSTACK."
  (and haystack
       (let ((case-fold-search t))
         (string-match-p (regexp-quote needle) haystack))))

(cl-defun orgzly-query-context (&key now todo-keywords done-keywords
                                     default-priority)
  "Build a matcher context.
NOW is a time value (default: current time). TODO-KEYWORDS and
DONE-KEYWORDS classify states for it.todo / it.done; DEFAULT-PRIORITY
backs p. matching on notes with no explicit priority."
  (list (cons 'now (or now (current-time)))
        (cons 'todo (or todo-keywords '("TODO" "NEXT")))
        (cons 'done (or done-keywords '("DONE")))
        (cons 'default-priority (or default-priority "B"))))

(defun orgzly-query-match-p (cond entry ctx)
  "Non-nil when ENTRY satisfies condition tree COND under context CTX.
A nil COND matches everything."
  (if (null cond) t
    (pcase cond
      (`(and . ,cs) (cl-every (lambda (c) (orgzly-query-match-p c entry ctx)) cs))
      (`(or . ,cs) (cl-some (lambda (c) (orgzly-query-match-p c entry ctx)) cs))
      (`(book ,name ,not)
       (xor not (equal name (alist-get 'book entry))))
      (`(state ,state ,not)
       (xor not (equal (upcase state) (or (alist-get 'state entry) ""))))
      (`(state-type ,type ,not)
       (let ((state (alist-get 'state entry)))
         (xor not
              (pcase type
                ('todo (member state (alist-get 'todo ctx)))
                ('done (member state (alist-get 'done ctx)))
                ('none (null state))))))
      (`(priority ,p ,not)
       (let ((eff (or (alist-get 'priority entry)
                      (alist-get 'default-priority ctx))))
         (xor not (equal (downcase p) (downcase eff)))))
      (`(set-priority ,p ,not)
       (xor not (equal (downcase p)
                       (downcase (or (alist-get 'priority entry) "")))))
      (`(tag ,tag ,not)
       (let ((all (append (alist-get 'tags entry) (alist-get 'itags entry))))
         (xor not (orgzly-query--substring-p tag (string-join all " ")))))
      (`(own-tag ,tag ,not)
       (xor not (orgzly-query--substring-p
                 tag (string-join (alist-get 'tags entry) " "))))
      (`(time event ,rel ,interval)
       (orgzly-query--events-in-relation-p
        (orgzly-query--entry-events entry) rel interval (alist-get 'now ctx)))
      (`(time ,kind ,rel ,interval)
       (orgzly-query--time-in-relation-p
        (orgzly-query--entry-time entry kind) rel interval (alist-get 'now ctx)))
      (`(text ,str ,_quoted)
       (or (orgzly-query--substring-p str (alist-get 'title entry))
           (orgzly-query--substring-p str (alist-get 'content entry))
           (orgzly-query--substring-p
            str (string-join (alist-get 'tags entry) " "))))
      (_ (error "orgzly-query: unknown condition %S" cond)))))

;; ─── Condition inspection (for default sort and agenda) ─────────────────────

(defun orgzly-query-uses-time-p (cond kind)
  "Non-nil when condition tree COND contains a KIND time condition."
  (pcase cond
    (`(,(or 'and 'or) . ,cs)
     (cl-some (lambda (c) (orgzly-query-uses-time-p c kind)) cs))
    (`(time ,k . ,_) (eq k kind))
    (_ nil)))

;; ─── Sorting ─────────────────────────────────────────────────────────────────

(defun orgzly-query--state-rank (state ctx)
  "Rank of STATE in the configured keyword order (todo then done)."
  (let ((seq (append (alist-get 'todo ctx) (alist-get 'done ctx))))
    (or (cl-position state seq :test #'equal)
        (if state (length seq) (1+ (length seq))))))

(defun orgzly-query--sort-value (entry key ctx)
  "Comparable value of ENTRY under sort KEY."
  (pcase key
    ('book (or (alist-get 'book entry) ""))
    ('title (or (alist-get 'title entry) ""))
    ('priority (downcase (or (alist-get 'priority entry)
                             (alist-get 'default-priority ctx))))
    ('state (orgzly-query--state-rank (alist-get 'state entry) ctx))
    ('position (alist-get 'pos entry))
    ('event (let ((evs (orgzly-query--entry-events entry)))
              (and evs (apply #'min evs))))
    (_ (orgzly-query--entry-time entry key))))

(defun orgzly-query--compare (a b desc)
  "Three-way-ish compare of sort values A and B honouring DESC.
nil sorts after everything in either direction (missing timestamps go
last). Returns `lt', `gt', or `eq'."
  (cond
   ((and (null a) (null b)) 'eq)
   ((null a) 'gt)
   ((null b) 'lt)
   (t (let* ((lt (if (stringp a) (string-lessp a b) (< a b)))
             (gt (if (stringp a) (string-lessp b a) (> a b))))
        (cond ((and desc gt) 'lt)
              ((and desc lt) 'gt)
              (lt 'lt)
              (gt 'gt)
              (t 'eq))))))

(defun orgzly-query-sort (entries query ctx)
  "Sort ENTRIES per QUERY's o.* orders (or Orgzly's defaults).
Defaults mirror upstream: book then position; a query using s./d.
conditions gets scheduled/deadline time prepended."
  (let* ((orders (plist-get query :orders))
         (cond (plist-get query :cond))
         (orders
          (or orders
              (append
               (when (orgzly-query-uses-time-p cond 'scheduled)
                 '((scheduled . nil)))
               (when (orgzly-query-uses-time-p cond 'deadline)
                 '((deadline . nil)))
               '((book . nil) (position . nil))))))
    (sort (copy-sequence entries)
          (lambda (ea eb)
            (cl-loop for (key . desc) in orders
                     for c = (orgzly-query--compare
                              (orgzly-query--sort-value ea key ctx)
                              (orgzly-query--sort-value eb key ctx)
                              desc)
                     unless (eq c 'eq) return (eq c 'lt)
                     finally return nil)))))

(defun orgzly-query-select (entries query ctx)
  "Filter and sort ENTRIES by parsed QUERY under CTX."
  (orgzly-query-sort
   (cl-remove-if-not
    (lambda (e) (orgzly-query-match-p (plist-get query :cond) e ctx))
    entries)
   query ctx))

(provide 'orgzly-query)
;;; orgzly-query.el ends here

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-data.el
;;; ==================================================================

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

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-ui.el
;;; ==================================================================

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

(defcustom orgzly-display-book-name-in-search t
  "When non-nil, search and agenda rows show the note's notebook."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-content-preview-lines 8
  "Maximum content lines shown under a title in note lists."
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

(defun orgzly-ui--reset-drill-in (&optional _view)
  "Clear all drill-in state (used when the user lands on a tab)."
  (setq orgzly-ui--current-book nil
        orgzly-ui--note-ref nil
        orgzly-ui--preface-book nil
        orgzly-ui--select-mode nil
        orgzly-ui--selection nil))

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
                         :color (if done orgzly-ui--done-color
                                  orgzly-ui--todo-color))))
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
           (when (and show-book orgzly-display-book-name-in-search)
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
  "ENTRY's content shown under the title (truncated, org-highlighted), or nil."
  (when orgzly-display-content
    (let ((content (alist-get 'content entry)))
      (when (and (stringp content) (not (string-empty-p content)))
        (let ((lines (split-string content "\n")))
          (jetpacs-markup
           (if (> (length lines) orgzly-content-preview-lines)
               (concat (string-join
                        (seq-take lines orgzly-content-preview-lines) "\n")
                       "\n…")
             content)
           :syntax "org" :style 'caption))))))

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
                           (cons (orgzly-ui--title-node
                                  entry :hide-content hide-content)
                                 (orgzly-ui--meta-lines
                                  entry :show-book show-book
                                  :only-kind only-kind))))
              :weight 1.0 :padding 4
              :on-tap (if orgzly-ui--select-mode
                          (orgzly-ui--note-action "orgzly.note.select-toggle" ref)
                        (orgzly-ui--note-action "orgzly.note.open" ref))))
            (list :align "center")))))

;; ─── Quick-action popup (Orgzly's note popup) ────────────────────────────────

(defconst orgzly-ui--quick-ops
  '(("state"       "State"       "flag"                "orgzly.note.pick-state")
    ("toggle-done" "Done"        "check_circle"        "orgzly.note.toggle-done")
    ("new-under"   "New under"   "playlist_add"        "orgzly.note.new-under")
    ("refile"      "Refile"      "move_to_inbox"       "orgzly.note.refile")
    ("archive"     "Archive"     "archive"             "orgzly.note.archive")
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
            (mapcar btn '("new-under" "refile" "archive")))
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
  "One notebook as Orgzly's book card: title plus icon-led detail lines."
  (let ((name (alist-get 'name b)))
    (jetpacs-card
     (list
      (jetpacs-row
       (jetpacs-box
        (list
         (jetpacs-column
          (jetpacs-rich-text
           (append
            (list (jetpacs-span name :bold t))
            (when (equal name orgzly-default-book)
              (list (jetpacs-span "  ★" :color orgzly-ui--muted-color))))
           :style 'title)
          (jetpacs-spacer :height 6)
          (orgzly-ui--icon-line
           "access_time"
           (format-time-string "%b %-d %H:%M"
                               (seconds-to-time (alist-get 'mtime b))))
          (orgzly-ui--icon-line
           "format_list_bulleted"
           (format "%d note%s" (alist-get 'count b)
                   (if (= (alist-get 'count b) 1) "" "s")))))
        :weight 1.0
        :on-tap (jetpacs-action "orgzly.book.open"
                             :args `((book . ,name))
                             :when-offline "drop"))
       (orgzly-ui--book-menu name)
       :align "center")))))

(defun orgzly-ui--books-body ()
  (let ((books (orgzly-data-books)))
    (if (null books)
        (jetpacs-empty-state :icon "library_books" :title "No notebooks"
                          :caption (format "Create your first notebook in %s"
                                           orgzly-directory)
                          :on-tap (jetpacs-action "orgzly.book.new")
                          :action-label "New notebook")
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
    (jetpacs-collapsible
     (format "orgzly-fold:%s:%s" (alist-get 'file entry) (alist-get 'pos entry))
     (orgzly-ui--note-row entry :hide-content (not orgzly-display-content))
     (delq nil (cons content (mapcar #'orgzly-ui--note-node children)))
     :collapsed (and (null children) (null content))
     :on-long-tap quick
     :on-swipe quick)))

(defun orgzly-ui--selection-bar ()
  "Orgzly's multi-select action bar, shown while selecting."
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
                  :variant "text" :icon "delete")))
   :color "surface_container" :shape "rounded" :padding 4 :fill t))

(defun orgzly-ui--book-body ()
  (let* ((book orgzly-ui--current-book)
         (entries (orgzly-data-entries book))
         (preface (orgzly-data-book-preface book)))
    (apply #'jetpacs-lazy-column
           (delq nil
                 (append
                  (when orgzly-ui--select-mode
                    (list (orgzly-ui--selection-bar)))
                  (when preface
                    (list (jetpacs-box
                           (list (jetpacs-markup preface :syntax "org"
                                              :style 'caption))
                           :padding 8
                           :on-tap (jetpacs-action "orgzly.book.preface"
                                                :args `((book . ,book))
                                                :when-offline "drop"))))
                  (or (mapcar #'orgzly-ui--note-node
                              (orgzly-ui--forest-of entries 1))
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
   :actions (list
             (jetpacs-icon-button "search" (jetpacs-shell-switch-view "orgzly.search")
                               :content-description "Search")
             (jetpacs-icon-button (if orgzly-ui--select-mode "close" "checklist")
                               (jetpacs-action "orgzly.book.select-mode"
                                            :when-offline "drop")
                               :content-description "Select notes"))
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
                  (orgzly-ui--meta-row "task_alt"
                                       (orgzly-ui--ts-label closed) "Closed"))
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
  "Current (DATE TIME REPEATER) strings of REF's KIND planning, or nils."
  (let* ((entry (orgzly-ui--entry-for-ref ref))
         (ts (and entry (alist-get (intern kind) entry)))
         (time (and ts (alist-get 'time ts))))
    (list (and time (format-time-string "%Y-%m-%d" (seconds-to-time time)))
          (and ts (alist-get 'has-time ts)
               (format-time-string "%H:%M" (seconds-to-time time)))
          (and ts (alist-get 'repeater ts)))))

(defun orgzly-ui--plan-dialog (ref kind)
  "The timestamp dialog spec for REF's KIND planning line."
  (let* ((parts (orgzly-ui--plan-parts ref kind))
         (args (lambda (&rest extra)
                 (append ref (cons (cons 'kind kind) extra)))))
    (jetpacs-lazy-column
     (jetpacs-row
      (jetpacs-box
       (list (jetpacs-text (if (equal kind "deadline")
                            "Deadline time" "Scheduled time")
                        'title))
       :weight 1.0)
      (jetpacs-button "Done" (jetpacs-action "dialog.dismiss") :variant "text")
      :align "center")
     (jetpacs-row
      (jetpacs-icon (if (equal kind "deadline") "alarm" "today")
                 :size 20 :color "outline")
      (jetpacs-date-button (or (nth 0 parts) "Date")
                        (jetpacs-action "orgzly.note.plan-date"
                                     :args (funcall args '(dialog . t)))
                        :value (nth 0 parts))
      (jetpacs-time-button (or (nth 1 parts) "Time")
                        (jetpacs-action "orgzly.note.plan-time"
                                     :args (funcall args '(dialog . t)))
                        :value (nth 1 parts))
      (jetpacs-button (or (nth 2 parts) "Repeat")
                   (jetpacs-action "orgzly.note.plan-repeater"
                                :args (funcall args '(dialog . t)))
                   :variant "text")
      :spacing 8 :align "center")
     (jetpacs-row
      (jetpacs-spacer :weight 1.0)
      (jetpacs-button "Clear"
                   (jetpacs-action "orgzly.note.plan-clear"
                                :args (funcall args))
                   :variant "text" :icon "close")))))

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
          orgzly-ui--preface-book nil)
    (jetpacs-shell-push nil :switch-to "orgzly.book")))

(jetpacs-defaction "orgzly.book.new"
  (lambda (_ _)
    (let ((name (string-trim (read-string "Notebook name: "))))
      (unless (string-empty-p name)
        (orgzly-data-create-book name)
        (jetpacs-shell-notify (format "Created %s" name))
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
               (insert clean "\n\n")))))
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

(defun orgzly-ui--plan-apply (ref kind date time repeater &optional dialog)
  "Write the planning string assembled from DATE/TIME/REPEATER onto REF."
  (orgzly-data-set-planning
   ref (intern kind)
   (and date (string-join (delq nil (list date time repeater)) " ")))
  (when dialog
    (jetpacs-send-dialog (orgzly-ui--plan-dialog ref kind)))
  (orgzly-ui--after-mutation nil))

(jetpacs-defaction "orgzly.note.plan-date"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind)))
      (orgzly-ui--plan-apply ref kind (alist-get 'value args)
                             (nth 1 parts) (nth 2 parts)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-time"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind))
           (date (or (nth 0 parts) (format-time-string "%Y-%m-%d"))))
      (orgzly-ui--plan-apply ref kind date (alist-get 'value args)
                             (nth 2 parts)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-repeater"
  (lambda (args _)
    (let* ((ref (orgzly-ui--args-ref args))
           (kind (alist-get 'kind args))
           (parts (orgzly-ui--plan-parts ref kind))
           (rep (string-trim
                 (read-string "Repeater (+1w, ++1m, .+2d; empty to clear): "
                              (or (nth 2 parts) "")))))
      (orgzly-ui--plan-apply ref kind
                             (or (nth 0 parts) (format-time-string "%Y-%m-%d"))
                             (nth 1 parts)
                             (unless (string-empty-p rep) rep)
                             (alist-get 'dialog args)))))

(jetpacs-defaction "orgzly.note.plan-clear"
  (lambda (args _)
    (jetpacs-dismiss-dialog)
    (orgzly-ui--plan-apply (orgzly-ui--args-ref args) (alist-get 'kind args)
                           nil nil nil)))

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
      (unless keep-selection
        (setq orgzly-ui--selection nil orgzly-ui--select-mode nil))
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

(jetpacs-defaction "orgzly.bulk.plan"
  (lambda (args _)
    (let ((kind (intern (alist-get 'kind args)))
          (date (alist-get 'value args)))
      (orgzly-ui--bulk (lambda (ref) (orgzly-data-set-planning ref kind date))
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

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-agenda.el
;;; ==================================================================

;;; orgzly-agenda.el --- Query-driven day agenda -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's agenda: a saved search whose ad.N option expands matching
;; notes into day buckets over the next N days.  Scheduled and deadline
;; times generate occurrences (repeaters expanded); events (plain active
;; timestamps) land on their day; overdue scheduled/deadline items group
;; under today, qualified with how late they are.
;;
;; The expansion lives here so the search view and the reminders module
;; reuse the same occurrence math.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-query)

(declare-function orgzly-ui--note-row "orgzly-ui")
(declare-function orgzly-ui--icon-line "orgzly-ui")

(defconst orgzly-agenda--meta-color "#8a8a8a")

;; ─── Occurrence expansion ────────────────────────────────────────────────────

(defun orgzly-agenda--day-start (&optional offset now)
  "Float time of midnight OFFSET days from NOW's day."
  (let ((d (decode-time (or now (current-time)))))
    (float-time (encode-time 0 0 0 (+ (nth 3 d) (or offset 0))
                             (nth 4 d) (nth 5 d)))))

(defun orgzly-agenda--parse-repeater (repeater)
  "Parse REPEATER (\"+1w\", \"++2d\", \".+1m\") into (UNIT . VALUE), or nil."
  (when (and (stringp repeater)
             (string-match "\\`[.+]?\\+\\([0-9]+\\)\\([hdwmy]\\)\\'" repeater))
    (cons (pcase (match-string 2 repeater)
            ("h" 'hour) ("d" 'day) ("w" 'week) ("m" 'month) ("y" 'year))
          (string-to-number (match-string 1 repeater)))))

(defun orgzly-agenda--step (time unit value)
  "TIME advanced by VALUE UNITs, calendar-aware for months and years."
  (pcase unit
    ('hour (+ time (* value 3600)))
    ('day (+ time (* value 86400)))
    ('week (+ time (* value 7 86400)))
    (_ (let ((d (decode-time (seconds-to-time time))))
         (float-time
          (encode-time (nth 0 d) (nth 1 d) (nth 2 d) (nth 3 d)
                       (+ (nth 4 d) (if (eq unit 'month) value 0))
                       (+ (nth 5 d) (if (eq unit 'year) value 0))))))))

(defun orgzly-agenda--occurrences (ts window-start window-end &optional no-overdue)
  "Occurrence float-times of TS within [WINDOW-START, WINDOW-END).
A repeating timestamp contributes every repetition inside the window;
a base time before WINDOW-START contributes one overdue occurrence
(the base itself) unless NO-OVERDUE."
  (when-let ((time (alist-get 'time ts)))
    (let ((rep (orgzly-agenda--parse-repeater (alist-get 'repeater ts))))
      (cond
       ((null rep)
        (cond ((and (< time window-start) (not no-overdue)) (list time))
              ((and (>= time window-start) (< time window-end)) (list time))))
       (t
        (let ((occ time) (out nil) (guard 0))
          ;; Catch up to the window, keeping one overdue base if applicable.
          (when (and (< occ window-start) (not no-overdue))
            (push occ out))
          (while (and (< occ window-start) (< guard 1000))
            (setq occ (orgzly-agenda--step occ (car rep) (cdr rep))
                  guard (1+ guard)))
          ;; Collect repetitions inside the window.
          (while (and (< occ window-end) (< guard 1000))
            (when (>= occ window-start) (push occ out))
            (setq occ (orgzly-agenda--step occ (car rep) (cdr rep))
                  guard (1+ guard)))
          (nreverse out)))))))

(defun orgzly-agenda-items (entries query ctx &optional now)
  "Expand QUERY's hits from ENTRIES into agenda items.
Returns a list of item alists sorted by (day, time):
  ((day . FLOAT midnight) (time . FLOAT) (kind . scheduled|deadline|event)
   (overdue-days . INT|nil) (entry . ENTRY))
Days span QUERY's ad.N (default 1) from today; overdue occurrences
collapse into today."
  (let* ((now (or now (current-time)))
         (days (or (plist-get query :agenda-days) 1))
         (window-start (orgzly-agenda--day-start 0 now))
         (window-end (orgzly-agenda--day-start days now))
         (hits (orgzly-query-select entries query ctx))
         (items nil))
    (dolist (entry hits)
      (let ((done (member (alist-get 'state entry) (alist-get 'done ctx))))
        (dolist (kind '(scheduled deadline))
          (when-let ((ts (alist-get kind entry)))
            (dolist (occ (orgzly-agenda--occurrences
                          ts window-start window-end done))
              (let* ((overdue (< occ window-start))
                     (day (if overdue window-start
                            (orgzly-agenda--day-start
                             0 (seconds-to-time occ)))))
                (push `((day . ,day) (time . ,occ) (kind . ,kind)
                        (overdue-days
                         . ,(when overdue
                              (max 1 (floor (/ (- window-start occ) 86400)))))
                        (entry . ,entry))
                      items)))))
        (dolist (ts (alist-get 'events entry))
          (dolist (occ (orgzly-agenda--occurrences ts window-start window-end t))
            (push `((day . ,(orgzly-agenda--day-start 0 (seconds-to-time occ)))
                    (time . ,occ) (kind . event) (overdue-days . nil)
                    (entry . ,entry))
                  items)))))
    (sort (nreverse items)
          (lambda (a b)
            (or (< (alist-get 'day a) (alist-get 'day b))
                (and (= (alist-get 'day a) (alist-get 'day b))
                     (< (alist-get 'time a) (alist-get 'time b))))))))

(defun orgzly-agenda-day-groups (entries query ctx &optional now)
  "Agenda items grouped per day: ((DAY . ITEMS) ...), days without items kept.
Overdue occurrences land in today's group (their `overdue-days' marks
them); `orgzly-agenda-sections' splits them out Orgzly-style."
  (let* ((now (or now (current-time)))
         (days (or (plist-get query :agenda-days) 1))
         (items (orgzly-agenda-items entries query ctx now)))
    (cl-loop for i from 0 below days
             for day = (orgzly-agenda--day-start i now)
             collect (cons day
                           (cl-remove-if-not
                            (lambda (it) (= (alist-get 'day it) day))
                            items)))))

(defcustom orgzly-agenda-group-scheduled-with-today nil
  "When non-nil, overdue scheduled notes group under Today.
Otherwise every overdue occurrence sits in the leading Overdue section,
as Orgzly does."
  :type 'boolean :group 'orgzly)

(defun orgzly-agenda-sections (entries query ctx &optional now)
  "Agenda sections: (`overdue' . ITEMS) first, then (DAY . ITEMS) per day.
The Overdue section collects occurrences whose base time is before
today — except overdue scheduled ones when
`orgzly-agenda-group-scheduled-with-today', which stay under Today."
  (let* ((now (or now (current-time)))
         (groups (orgzly-agenda-day-groups entries query ctx now))
         (overdue (cl-remove-if-not
                   (lambda (it)
                     (and (alist-get 'overdue-days it)
                          (not (and orgzly-agenda-group-scheduled-with-today
                                    (eq (alist-get 'kind it) 'scheduled)))))
                   (cdr (car groups)))))
    (when overdue
      (setcdr (car groups)
              (cl-remove-if (lambda (it) (memq it overdue)) (cdr (car groups)))))
    (if overdue (cons (cons 'overdue overdue) groups) groups)))

;; ─── Rendering ───────────────────────────────────────────────────────────────

(defun orgzly-agenda--day-header (day now)
  (let* ((today (orgzly-agenda--day-start 0 now))
         (label (format-time-string "%A, %b %-d" (seconds-to-time day)))
         (qualifier (cond ((= day today) "Today")
                          ((= day (orgzly-agenda--day-start 1 now)) "Tomorrow"))))
    (jetpacs-section-header
     (if qualifier (format "%s · %s" qualifier label) label))))

(defun orgzly-agenda--item-row (item)
  "One agenda item as the standard Orgzly note row.
Only the planning time responsible for the item's presence shows, and
an overdue item carries how late it is in red."
  (let* ((entry (alist-get 'entry item))
         (kind (alist-get 'kind item))
         (overdue (alist-get 'overdue-days item))
         (row (orgzly-ui--note-row entry :show-book t :only-kind kind
                                   :hide-content t)))
    (if (null overdue)
        row
      (jetpacs-column
       row
       (jetpacs-row
        (jetpacs-spacer :width 4)
        (orgzly-ui--icon-line "history"
                              (format "%d day%s overdue" overdue
                                      (if (= overdue 1) "" "s"))
                              "#e53935"))))))

(defun orgzly-agenda--empty-day-node ()
  (jetpacs-rich-text
   (list (jetpacs-span "No notes" :color orgzly-agenda--meta-color))
   :style 'caption :padding 8))

(defun orgzly-agenda-day-nodes (entries query ctx &optional now hide-empty)
  "Widget nodes: Overdue then day sections, shared with the search view."
  (let ((now (or now (current-time))))
    (cl-loop for (day . items) in (orgzly-agenda-sections entries query ctx now)
             when (or items (and (not (eq day 'overdue)) (not hide-empty)))
             append (cons (if (eq day 'overdue)
                              (jetpacs-section-header "Overdue")
                            (orgzly-agenda--day-header day now))
                          (or (mapcar #'orgzly-agenda--item-row items)
                              (list (orgzly-agenda--empty-day-node)))))))

;; ─── The agenda tab ──────────────────────────────────────────────────────────

(defcustom orgzly-agenda-hide-empty-days nil
  "When non-nil, days with no notes are omitted (Orgzly: hide empty days)."
  :type 'boolean :group 'orgzly)

(defvar orgzly-agenda--current "Agenda"
  "Name of the saved search the agenda tab is showing.")

(declare-function orgzly-search-saved-searches "orgzly-search")

(defun orgzly-agenda--searches ()
  "The agenda-shaped saved searches (those carrying ad.N)."
  (cl-remove-if-not
   (lambda (ss) (plist-get (orgzly-query-parse (cdr ss)) :agenda-days))
   (orgzly-search-saved-searches)))

(defun orgzly-agenda--body ()
  (let* ((searches (orgzly-agenda--searches))
         (current (or (assoc orgzly-agenda--current searches)
                      (car searches)))
         (query-str (or (cdr current) ".it.done ad.7"))
         (query (orgzly-query-parse query-str))
         (ctx (orgzly-data-query-context)))
    (apply #'jetpacs-lazy-column
           (append
            (when (> (length searches) 1)
              (list (apply #'jetpacs-scroll-row
                           (mapcar (lambda (ss)
                                     (jetpacs-chip (car ss)
                                                :selected (equal (car ss)
                                                                 (car current))
                                                :on-tap (jetpacs-action
                                                         "orgzly.agenda.switch"
                                                         :args `((name . ,(car ss)))
                                                         :when-offline "drop")))
                                   searches))))
            (or (orgzly-agenda-day-nodes (orgzly-data-entries) query ctx nil
                                         orgzly-agenda-hide-empty-days)
                (list (jetpacs-empty-state :icon "event_available"
                                        :title "Nothing scheduled")))))))

(jetpacs-shell-define-view "orgzly.agenda"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "orgzly.agenda" (orgzly-agenda--body)
                                  :snackbar snackbar))
  :tab '(:icon "event" :label "Agenda")
  :order 11)

(jetpacs-defaction "orgzly.agenda.switch"
  (lambda (args _)
    (setq orgzly-agenda--current (alist-get 'name args))
    (jetpacs-shell-push)))

(provide 'orgzly-agenda)
;;; orgzly-agenda.el ends here

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-search.el
;;; ==================================================================

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
                           (orgzly-ui--note-row e :show-book t :hide-content t))
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
                       :back-to "orgzly.search"
                       :snackbar snackbar))

;; ─── Views & actions ─────────────────────────────────────────────────────────

(jetpacs-shell-define-view "orgzly.search"
  :builder (lambda (snackbar)
             (jetpacs-shell-tab-view "orgzly.search" (orgzly-search--body)
                                  :snackbar snackbar))
  :tab '(:icon "search" :label "Search")
  :order 12)

(jetpacs-shell-define-view "orgzly.searches"
  :builder #'orgzly-search--manage-view
  :overlay (lambda () orgzly-search--manage)
  :order 24)

(add-hook 'jetpacs-shell-view-switched-hook
          (lambda (&optional _view) (setq orgzly-search--manage nil)))

(jetpacs-defaction "orgzly.search.run"
  (lambda (args _)
    (setq orgzly-search--query (or (alist-get 'value args) ""))
    (jetpacs-shell-push "orgzly.search")))

(jetpacs-defaction "orgzly.search.saved"
  (lambda (args _)
    (when-let ((ss (assoc (alist-get 'name args) orgzly-saved-searches)))
      (setq orgzly-search--query (cdr ss)
            orgzly-search--manage nil)
      (jetpacs-shell-push "orgzly.search" :switch-to "orgzly.search"))))

(jetpacs-defaction "orgzly.search.manage"
  (lambda (_ _)
    (setq orgzly-search--manage t)
    (jetpacs-shell-push nil :switch-to "orgzly.searches")))

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

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-reminders.el
;;; ==================================================================

;;; orgzly-reminders.el --- Scheduled/deadline/event reminders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's reminder classes on the jetpacs `reminders.set' frame: exact
;; alarms for scheduled times, deadline times, and events, each behind its
;; own toggle; date-only items remind at `orgzly-reminders-daily-time'.
;; The set is replace-set semantics and persisted across reboots by the
;; companion, so each shell push simply re-derives the upcoming window.
;;
;; Companion-protocol limit (see docs/PARITY.md): the reminder
;; notification carries no Done/Snooze buttons — tapping it opens the
;; app, where the note is one tap away.

;;; Code:

(require 'cl-lib)
(require 'jetpacs)
(require 'jetpacs-shell)
(require 'orgzly-data)
(require 'orgzly-agenda)

(defcustom orgzly-reminders-scheduled t
  "Remind at scheduled times (Orgzly: reminders for scheduled)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-deadline t
  "Remind at deadline times (Orgzly: reminders for deadlines)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-event nil
  "Remind at event times — plain active timestamps (Orgzly: events)."
  :type 'boolean :group 'orgzly)

(defcustom orgzly-reminders-daily-time "09:00"
  "Clock time (HH:MM) at which date-only items remind."
  :type 'string :group 'orgzly)

(defcustom orgzly-reminders-horizon-hours 24
  "How far ahead reminders are armed; each push re-derives the window."
  :type 'integer :group 'orgzly)

(defun orgzly-reminders--daily-offset ()
  "Seconds past midnight of `orgzly-reminders-daily-time'."
  (if (string-match "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                    orgzly-reminders-daily-time)
      (+ (* 3600 (string-to-number (match-string 1 orgzly-reminders-daily-time)))
         (* 60 (string-to-number (match-string 2 orgzly-reminders-daily-time))))
      (* 9 3600)))

(defun orgzly-reminders--kinds ()
  (append (when orgzly-reminders-scheduled '(scheduled))
          (when orgzly-reminders-deadline '(deadline))
          (when orgzly-reminders-event '(event))))

(defun orgzly-reminders--entry-specs (entry now horizon done-keywords)
  "Reminder spec alists for ENTRY within [NOW, NOW+HORIZON)."
  (unless (member (alist-get 'state entry) done-keywords)
    (let ((end (+ now horizon))
          (daily (orgzly-reminders--daily-offset))
          specs)
      (dolist (kind (orgzly-reminders--kinds))
        (dolist (ts (if (eq kind 'event)
                        (alist-get 'events entry)
                      (when-let ((one (alist-get kind entry))) (list one))))
          ;; Expand from slightly in the past so a repeater's occurrence
          ;; earlier today still steps forward into the window.
          (dolist (occ (orgzly-agenda--occurrences ts now end t))
            (let ((at (if (alist-get 'has-time ts)
                          occ
                        ;; Date-only: remind at the daily reminder time.
                        (+ (orgzly-agenda--day-start
                            0 (seconds-to-time occ))
                           daily))))
              (when (and (> at now) (< at end))
                (push
                 `((id . ,(format "orgzly:%s:%s:%s:%d"
                                  (alist-get 'file entry)
                                  (alist-get 'pos entry) kind (truncate at)))
                   (at_ms . ,(truncate (* at 1000)))
                   (title . ,(alist-get 'title entry))
                   (body . ,(format "%s · %s · %s"
                                    (capitalize (symbol-name kind))
                                    (format-time-string
                                     "%H:%M" (seconds-to-time at))
                                    (alist-get 'book entry))))
                 specs))))))
      specs)))

(defun orgzly-reminders-upcoming (&optional now)
  "Every reminder spec within the horizon, across all books."
  (let* ((now (float-time (or now (current-time))))
         (horizon (* 3600 orgzly-reminders-horizon-hours))
         (done (cdr (orgzly-data-todo-keywords)))
         specs)
    (dolist (entry (orgzly-data-entries))
      (setq specs (nconc specs
                         (orgzly-reminders--entry-specs entry now horizon done))))
    (sort specs (lambda (a b) (< (alist-get 'at_ms a) (alist-get 'at_ms b))))))

(defvar orgzly-reminders--last 'unset
  "Previous reminder set, to suppress identical sends.")

(defun orgzly-reminders-sync ()
  "Push the upcoming reminders as a replace-set (memo-guarded)."
  (let ((rems (condition-case nil (orgzly-reminders-upcoming) (error nil))))
    (unless (equal rems orgzly-reminders--last)
      (setq orgzly-reminders--last rems)
      (jetpacs-send "reminders.set" `((reminders . ,(vconcat rems)))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly-reminders-sync)
(add-hook 'jetpacs-shell-refresh-hook
          (lambda () (setq orgzly-reminders--last 'unset)))

(provide 'orgzly-reminders)
;;; orgzly-reminders.el ends here

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-widget.el
;;; ==================================================================

;;; orgzly-widget.el --- Home-screen list widget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's saved-search list widget on the jetpacs `widget:agenda'
;; surface: one widget view per saved search, switched companion-side
;; from the widget header (offline-capable, served from cache).  Rows
;; carry the todo-cycle checkmark button, exactly like Orgzly's widget.

;;; Code:

(require 'cl-lib)
(require 'jetpacs-shell)
(require 'jetpacs-surfaces)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-agenda)
(require 'orgzly-search)

(defconst orgzly-widget--cap 20
  "Rows per widget view; a sanity bound on spec size, the list scrolls.")

(defun orgzly-widget--ts-meta (entry)
  "The row's metadata line: earliest planning time, then the book."
  (let* ((bits (delq nil
                     (list
                      (when-let ((s (alist-get 'scheduled entry)))
                        (format-time-string
                         (if (alist-get 'has-time s) "%b %-d %H:%M" "%b %-d")
                         (seconds-to-time (alist-get 'time s))))
                      (alist-get 'book entry)))))
    (string-join bits " · ")))

(defun orgzly-widget--icon (entry)
  (cond ((alist-get 'deadline entry) "deadline")
        ((alist-get 'scheduled entry) "scheduled")
        ((alist-get 'events entry) "event")
        (t "folder")))

(defun orgzly-widget--row (entry)
  (let ((ref (orgzly-data-entry-ref entry))
        (todo (alist-get 'state entry))
        (done (orgzly-ui--done-p entry)))
    (jetpacs-widget-item
     (alist-get 'title entry)
     :todo todo :done done
     :meta (orgzly-widget--ts-meta entry)
     :icon (orgzly-widget--icon entry)
     :on-tap (jetpacs-action "orgzly.note.open" :args ref) :in-app t
     :button (and todo (if done "todo_done" "todo_open"))
     :on-button (and todo (jetpacs-action "orgzly.note.toggle-done" :args ref)))))

(defun orgzly-widget--agenda-rows (query ctx)
  "Sectioned widget rows for an ad.N QUERY: Overdue, then day dividers."
  (let ((now (current-time))
        (rows nil) (count 0))
    (cl-loop
     for (day . items) in (orgzly-agenda-sections
                           (orgzly-data-entries) query ctx now)
     while (< count orgzly-widget--cap)
     when items
     do (let* ((today (orgzly-agenda--day-start 0 now))
               (label (cond ((eq day 'overdue) "Overdue")
                            ((= day today) "Today")
                            ((= day (orgzly-agenda--day-start 1 now)) "Tomorrow")
                            (t (format-time-string "%a, %b %-d"
                                                   (seconds-to-time day))))))
          (push (jetpacs-widget-divider label) rows)
          (dolist (it items)
            (when (< count orgzly-widget--cap)
              (push (orgzly-widget--row (alist-get 'entry it)) rows)
              (cl-incf count)))))
    (nreverse rows)))

(defun orgzly-widget--search-rows (query ctx)
  (mapcar #'orgzly-widget--row
          (seq-take (orgzly-query-select (orgzly-data-entries) query ctx)
                    orgzly-widget--cap)))

(defun orgzly-widget--views ()
  "One widget view per saved search, day-grouped when the query is ad.N."
  (let ((ctx (orgzly-data-query-context)))
    (mapcar
     (lambda (ss)
       (let ((query (orgzly-query-parse (cdr ss))))
         (cons (intern (car ss))
               `((title . ,(car ss))
                 (items . ,(vconcat
                            (condition-case nil
                                (if (plist-get query :agenda-days)
                                    (orgzly-widget--agenda-rows query ctx)
                                  (orgzly-widget--search-rows query ctx))
                              (error nil))))))))
     (orgzly-search-saved-searches))))

(defvar orgzly-widget--last 'unset
  "Previous widget spec, to suppress identical pushes.")

(defun orgzly-widget-push ()
  "Push the `widget:agenda' surface (memo-guarded)."
  (let ((views (condition-case nil (orgzly-widget--views) (error nil))))
    (when views
      (unless (equal views orgzly-widget--last)
        (setq orgzly-widget--last views)
        (jetpacs-surface-push
         "widget:agenda"
         `((views . ,views)
           (initial_view . ,(symbol-name (car (car views))))))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly-widget-push)
(add-hook 'jetpacs-shell-refresh-hook
          (lambda () (setq orgzly-widget--last 'unset)))

(provide 'orgzly-widget)
;;; orgzly-widget.el ends here

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-capture.el
;;; ==================================================================

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

(defun orgzly-capture--on-share (args _payload)
  "Create a note in the default book from a share sheet payload."
  (let* ((text (alist-get 'text args))
         (subject (alist-get 'subject args))
         (text (and (stringp text) (not (string-empty-p (string-trim text)))
                    (string-trim text)))
         (subject (and (stringp subject)
                       (not (string-empty-p (string-trim subject)))
                       (string-trim subject)))
         ;; Title: the subject, else the shared text's first line.
         (title (or subject (car (split-string (or text "Shared note") "\n"))))
         (content (cond ((and subject text) text)
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

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly-settings.el
;;; ==================================================================

;;; orgzly-settings.el --- Settings screen & registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Orgzly's preference screens, mapped onto the jetpacs schema-driven
;; settings machinery: each section below mirrors one of Orgzly's pref
;; screens; the registry doubles as the wire allowlist (only listed
;; symbols are settable from the phone), values validate against their
;; `custom-type', and persistence goes through Customize.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-settings)
(require 'jetpacs-widgets)
(require 'orgzly-data)
(require 'orgzly-ui)
(require 'orgzly-reminders)
(require 'orgzly-agenda)

;; Every mutation from the settings screen may change what the views and
;; scans derive, so drop the memo (the standing cache contract).
(add-hook 'jetpacs-settings-after-set-hook
          (lambda (&rest _) (orgzly-data-invalidate)))

;; Registered under the app's owner id, so coexisting apps' settings
;; never interleave: these sections render only while Orgzly is current.
(with-jetpacs-owner "orgzly"
  (jetpacs-settings-register-section
   "Notebooks"
   '((orgzly-directory . (:label "Notebooks directory"))
     (orgzly-default-book . (:label "Default notebook"))))

  (jetpacs-settings-register-section
   "New note"
   '((orgzly-new-note-state . (:label "Initial state"))
     (orgzly-new-note-prepend . (:label "Add to top of notebook"))
     (orgzly-new-note-created-property . (:label "Stamp created time"))
     (orgzly-created-property . (:label "Created property name"))))

  (jetpacs-settings-register-section
   "Display"
   '((orgzly-display-content . (:label "Note content in lists"))
     (orgzly-display-content-line-count . (:label "Content line count"))
     (orgzly-display-planning . (:label "Planning times"))
     (orgzly-display-book-name-in-search . (:label "Notebook name in results"))
     (orgzly-content-preview-lines . (:label "Content preview lines"))))

  (jetpacs-settings-register-section
   "Agenda"
   '((orgzly-agenda-hide-empty-days . (:label "Hide empty days"))
     (orgzly-agenda-group-scheduled-with-today
      . (:label "Group overdue scheduled with today"))))

  (jetpacs-settings-register-section
   "Reminders"
   '((orgzly-reminders-scheduled . (:label "For scheduled times"))
     (orgzly-reminders-deadline . (:label "For deadlines"))
     (orgzly-reminders-event . (:label "For events"))
     (orgzly-reminders-daily-time . (:label "Daily time for date-only notes"))
     (orgzly-reminders-horizon-hours . (:label "Arm window (hours)"))))

  (jetpacs-settings-register-section
   "Org file format"
   '((org-archive-location . (:label "Archive location"))
     (org-log-done . (:label "Log state changes"))
     (org-priority-default . (:label "Default priority (char code)"))
     (org-priority-highest . (:label "Highest priority (char code)"))
     (org-priority-lowest . (:label "Lowest priority (char code)")))))

;; No settings view and no drawer entry of our own: the stock core
;; "settings" screen renders the sections above (scoped to this app),
;; and the stock drawer entry already targets it.  An app only defines
;; "<appid>.settings" when it needs controls the schema can't express.

(provide 'orgzly-settings)
;;; orgzly-settings.el ends here

;;; ==================================================================
;;; BEGIN apps/orgzly/orgzly.el
;;; ==================================================================

;;; orgzly.el --- Orgzly Revived as a pure-elisp Jetpacs Tier 1 -*- lexical-binding: t; -*-

;; Copyright (C) 2026 calebc42 and contributors
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "28.1"))

;; Orgzly Revived, rebuilt on the Jetpacs foundation with zero Kotlin:
;; the companion is the stock app-agnostic renderer; every screen,
;; query, reminder, and mutation below is elisp, and org files on disk
;; are the database.
;;
;;   (add-to-list 'load-path "…/jetpacs/emacs/core")  ; or jetpacs-core.el
;;   (add-to-list 'load-path "…/emacs/apps/orgzly")
;;   (require 'orgzly)
;;
;; Feature coverage against orgzly-android-revived is tracked in
;; docs/PARITY.md.

;;; Code:

(require 'jetpacs-shell)
(require 'jetpacs-apps)

(require 'orgzly-data)
(require 'orgzly-query)
(require 'orgzly-ui)
(require 'orgzly-agenda)
(require 'orgzly-search)
(require 'orgzly-reminders)
(require 'orgzly-widget)
(require 'orgzly-capture)
(require 'orgzly-settings)

(setq jetpacs-shell-drawer-header "Orgzly")

;; ─── The drawer: searches and notebooks, like Orgzly's navigation ────────────
;;
;; Orgzly's drawer lists every saved search and every notebook for
;; one-tap jumps.  Drawer builders run per push (so selection highlights
;; stay current), but the *set* of items only changes when a search or
;; book is added, renamed, or removed — the sync below runs after each
;; push and is memo-guarded on the name lists, so a stable set costs
;; nothing and never re-pushes.

(defvar orgzly--drawer-memo nil
  "The (SEARCH-NAMES BOOK-NAMES) the drawer was last built for.")

(defun orgzly--drawer-sync ()
  "Mirror saved searches and notebooks into the drawer."
  (let ((key (list (mapcar #'car (orgzly-search-saved-searches))
                   (mapcar #'orgzly-data-book-name
                           (orgzly-data-book-files)))))
    (unless (equal key orgzly--drawer-memo)
      (setq orgzly--drawer-memo key)
      ;; Orders 30–49 are the drawer band owned by this sync.
      (setq jetpacs-shell-drawer-items
            (cl-remove-if (lambda (e) (and (>= (car e) 30) (< (car e) 50)))
                          jetpacs-shell-drawer-items))
      ;; Registered under the app's owner id — this sync runs from a
      ;; push hook, outside any load-time owner scope, and the entries
      ;; must ride only Orgzly's drawer once a second app exists.
      (with-jetpacs-owner "orgzly"
       (let ((order 30.0))
        (jetpacs-shell-add-drawer-item
         order (lambda ()
                 (jetpacs-drawer-item "manage_search" "Searches"
                                   (jetpacs-action "orgzly.search.manage"
                                                :when-offline "drop"))))
        (dolist (name (nth 0 key))
          (setq order (+ order 0.01))
          (jetpacs-shell-add-drawer-item
           order
           (lambda ()
             (jetpacs-drawer-item
              "search" name
              (jetpacs-action "orgzly.search.saved"
                           :args `((name . ,name)) :when-offline "drop")
              :selected (equal (cdr (assoc name (orgzly-search-saved-searches)))
                               orgzly-search--query)))))
        (setq order 40.0)
        (jetpacs-shell-add-drawer-item
         order (lambda ()
                 (jetpacs-drawer-item "library_books" "Notebooks"
                                   (jetpacs-shell-switch-view "orgzly.books"))))
        (dolist (name (nth 1 key))
          (setq order (+ order 0.01))
          (jetpacs-shell-add-drawer-item
           order
           (lambda ()
             (jetpacs-drawer-item
              "description" name
              (jetpacs-action "orgzly.book.open"
                           :args `((book . ,name)) :when-offline "drop")
              :selected (equal name orgzly-ui--current-book))))))))))

(add-hook 'jetpacs-shell-after-push-hook #'orgzly--drawer-sync)
(orgzly--drawer-sync)

;; Every view name carries the "orgzly." namespace so a coexisting app
;; (glasspane's "glasspane.agenda", say) can never replace one of these
;; in the registry.  Settings is absent by design: Orgzly's sections
;; ride the stock core settings screen.
(jetpacs-defapp "orgzly"
  :label "Orgzly" :icon "book"
  :views '("orgzly.books" "orgzly.agenda" "orgzly.search" "orgzly.book"
           "orgzly.note" "orgzly.preface" "orgzly.searches")
  :order 10)

(provide 'orgzly)
;;; orgzly.el ends here

(provide 'orgzly)
;;; orgzly.el ends here
