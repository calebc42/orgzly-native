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
