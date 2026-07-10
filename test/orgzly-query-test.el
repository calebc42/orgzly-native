;;; orgzly-query-test.el --- ERT tests for the dotted query language -*- lexical-binding: t; -*-

;; Run: test/run-tests.sh  (or emacs --batch -L emacs/apps/orgzly -l this-file
;; -f ert-run-tests-batch-and-exit)

;;; Code:

(require 'ert)
(require 'orgzly-query)

;; ─── Fixtures ────────────────────────────────────────────────────────────────

(defconst orgzly-query-test--now
  (encode-time 0 30 14 9 7 2026)
  "Fixed \"now\": 2026-07-09 Thu 14:30 local time.")

(defun orgzly-query-test--ctx ()
  (orgzly-query-context :now orgzly-query-test--now
                        :todo-keywords '("TODO" "NEXT")
                        :done-keywords '("DONE" "CANCELLED")
                        :default-priority "B"))

(defun orgzly-query-test--ts (y m d &optional hh mm)
  "Timestamp alist at Y-M-D (optionally HH:MM)."
  `((time . ,(float-time (encode-time 0 (or mm 0) (or hh 0) d m y)))
    (has-time . ,(and hh t))
    (raw . ,(format "<%04d-%02d-%02d>" y m d))))

(cl-defun orgzly-query-test--entry (&key (book "gtd") (pos 1) (level 1)
                                         (title "A note") state priority
                                         tags itags scheduled deadline
                                         closed created events content)
  `((book . ,book) (file . ,(concat "/notes/" book ".org")) (pos . ,pos)
    (level . ,level) (title . ,title) (state . ,state) (priority . ,priority)
    (tags . ,tags) (itags . ,itags) (scheduled . ,scheduled)
    (deadline . ,deadline) (closed . ,closed) (created . ,created)
    (events . ,events) (content . ,(or content ""))))

(defun orgzly-query-test--match (query-str entry)
  (orgzly-query-match-p (plist-get (orgzly-query-parse query-str) :cond)
                        entry (orgzly-query-test--ctx)))

;; ─── Tokenizer ───────────────────────────────────────────────────────────────

(ert-deftest orgzly-query-tokenize-basic ()
  (should (equal (orgzly-query--tokenize "i.todo t.work")
                 '("i.todo" "t.work"))))

(ert-deftest orgzly-query-tokenize-quoted-with-prefix ()
  (should (equal (orgzly-query--tokenize "t.\"my tag\" b.\"Notes book\"")
                 '("t.\"my tag\"" "b.\"Notes book\""))))

(ert-deftest orgzly-query-tokenize-groups ()
  (should (equal (orgzly-query--tokenize "(i.todo or i.next) t.home")
                 '("(" "i.todo" "or" "i.next" ")" "t.home"))))

(ert-deftest orgzly-query-unquote ()
  (should (equal (orgzly-query--unquote "\"my tag\"") "my tag"))
  (should (equal (orgzly-query--unquote "\"a \\\"b\\\"\"") "a \"b\""))
  (should (equal (orgzly-query--unquote "plain") "plain")))

;; ─── Parser ──────────────────────────────────────────────────────────────────

(ert-deftest orgzly-query-parse-default-agenda ()
  "Orgzly's default saved search: .it.done ad.7"
  (let ((q (orgzly-query-parse ".it.done ad.7")))
    (should (equal (plist-get q :cond) '(and (state-type done t))))
    (should (= (plist-get q :agenda-days) 7))))

(ert-deftest orgzly-query-parse-state ()
  (should (equal (plist-get (orgzly-query-parse "i.todo") :cond)
                 '(and (state "todo" nil)))))

(ert-deftest orgzly-query-parse-implicit-and-explicit-or ()
  ;; a b or c  =>  OR(AND(a b), c)
  (should (equal (plist-get (orgzly-query-parse "t.a t.b or t.c") :cond)
                 '(or (and (tag "a" nil) (tag "b" nil)) (tag "c" nil))))
  ;; a or b c  =>  OR(a, AND(b c))
  (should (equal (plist-get (orgzly-query-parse "t.a or t.b t.c") :cond)
                 '(or (tag "a" nil) (and (tag "b" nil) (tag "c" nil))))))

(ert-deftest orgzly-query-parse-groups ()
  (should (equal (plist-get (orgzly-query-parse "(i.todo or i.next) t.home") :cond)
                 '(and (or (state "todo" nil) (state "next" nil))
                       (tag "home" nil)))))

(ert-deftest orgzly-query-parse-time-defaults ()
  ;; s./d. default to le; c./e. default to eq
  (should (equal (plist-get (orgzly-query-parse "s.today") :cond)
                 '(and (time scheduled le (day . 0)))))
  (should (equal (plist-get (orgzly-query-parse "d.1w") :cond)
                 '(and (time deadline le (week . 1)))))
  (should (equal (plist-get (orgzly-query-parse "c.yesterday") :cond)
                 '(and (time closed eq (day . -1)))))
  (should (equal (plist-get (orgzly-query-parse "e.today") :cond)
                 '(and (time event eq (day . 0)))))
  (should (equal (plist-get (orgzly-query-parse "cr.now") :cond)
                 '(and (time created le (now . 0))))))

(ert-deftest orgzly-query-parse-time-relations ()
  (should (equal (plist-get (orgzly-query-parse "s.ge.today") :cond)
                 '(and (time scheduled ge (day . 0)))))
  (should (equal (plist-get (orgzly-query-parse "s.none") :cond)
                 '(and (time scheduled le (none . 0))))))

(ert-deftest orgzly-query-parse-free-text ()
  (let ((q (orgzly-query-parse "call mom")))
    (should (equal (plist-get q :cond)
                   '(and (text "call" nil) (text "mom" nil)))))
  (should (equal (plist-get (orgzly-query-parse "\"call mom\"") :cond)
                 '(and (text "call mom" t)))))

(ert-deftest orgzly-query-parse-sort-orders ()
  (let ((q (orgzly-query-parse "i.todo o.p .o.d o.book")))
    (should (equal (plist-get q :orders)
                   '((priority . nil) (deadline . t) (book . nil))))))

(ert-deftest orgzly-query-parse-invalid-interval-is-text ()
  "s.bogus has no valid interval; upstream falls through to plain text."
  (should (equal (plist-get (orgzly-query-parse "s.bogus") :cond)
                 '(and (text "s.bogus" nil)))))

;; ─── Matcher: states, priorities, tags, books ────────────────────────────────

(ert-deftest orgzly-query-match-state-and-type ()
  (let ((todo (orgzly-query-test--entry :state "TODO"))
        (done (orgzly-query-test--entry :state "DONE"))
        (none (orgzly-query-test--entry)))
    (should (orgzly-query-test--match "i.todo" todo))
    (should-not (orgzly-query-test--match "i.todo" done))
    (should (orgzly-query-test--match ".i.todo" done))
    (should (orgzly-query-test--match "it.todo" todo))
    (should (orgzly-query-test--match "it.done" done))
    (should (orgzly-query-test--match "it.none" none))
    (should (orgzly-query-test--match ".it.done" todo))
    (should-not (orgzly-query-test--match ".it.done" done))))

(ert-deftest orgzly-query-match-priority ()
  (let ((pa (orgzly-query-test--entry :priority "A"))
        (unset (orgzly-query-test--entry)))
    (should (orgzly-query-test--match "p.a" pa))
    (should (orgzly-query-test--match "p.A" pa))
    ;; Default priority B applies to unset notes for p. but not ps.
    (should (orgzly-query-test--match "p.b" unset))
    (should-not (orgzly-query-test--match "ps.b" unset))
    (should (orgzly-query-test--match "ps.a" pa))))

(ert-deftest orgzly-query-match-tags ()
  (let ((e (orgzly-query-test--entry :tags '("work" "phone")
                                     :itags '("project"))))
    (should (orgzly-query-test--match "t.work" e))
    (should (orgzly-query-test--match "t.project" e))   ; inherited counts
    (should-not (orgzly-query-test--match "tn.project" e)) ; own-only doesn't
    (should (orgzly-query-test--match "tn.phone" e))
    ;; Substring semantics, as upstream's LIKE %tag%
    (should (orgzly-query-test--match "t.wor" e))
    (should (orgzly-query-test--match ".t.errand" e))))

(ert-deftest orgzly-query-match-book ()
  (let ((e (orgzly-query-test--entry :book "gtd")))
    (should (orgzly-query-test--match "b.gtd" e))
    (should-not (orgzly-query-test--match "b.other" e))
    (should (orgzly-query-test--match ".b.other" e))))

(ert-deftest orgzly-query-match-text ()
  (let ((e (orgzly-query-test--entry :title "Call mom"
                                     :content "about the trip"
                                     :tags '("family"))))
    (should (orgzly-query-test--match "call" e))
    (should (orgzly-query-test--match "trip" e))
    (should (orgzly-query-test--match "family" e))
    (should (orgzly-query-test--match "\"call mom\"" e))
    (should-not (orgzly-query-test--match "\"mom call\"" e))))

;; ─── Matcher: time conditions (now = 2026-07-09 Thu 14:30) ──────────────────

(ert-deftest orgzly-query-match-scheduled-le ()
  (let ((today (orgzly-query-test--entry
                :scheduled (orgzly-query-test--ts 2026 7 9)))
        (in3d (orgzly-query-test--entry
               :scheduled (orgzly-query-test--ts 2026 7 12)))
        (in10d (orgzly-query-test--entry
                :scheduled (orgzly-query-test--ts 2026 7 19)))
        (past (orgzly-query-test--entry
               :scheduled (orgzly-query-test--ts 2026 7 1)))
        (unsched (orgzly-query-test--entry)))
    ;; s.today = scheduled le today: today and overdue match
    (should (orgzly-query-test--match "s.today" today))
    (should (orgzly-query-test--match "s.today" past))
    (should-not (orgzly-query-test--match "s.today" in3d))
    ;; s.3d covers through the day 3 days out
    (should (orgzly-query-test--match "s.3d" in3d))
    (should-not (orgzly-query-test--match "s.3d" in10d))
    ;; unscheduled never matches a non-none interval
    (should-not (orgzly-query-test--match "s.today" unsched))
    (should (orgzly-query-test--match "s.none" unsched))
    (should-not (orgzly-query-test--match "s.none" today))))

(ert-deftest orgzly-query-match-scheduled-relations ()
  (let ((today (orgzly-query-test--entry
                :scheduled (orgzly-query-test--ts 2026 7 9)))
        (tomorrow (orgzly-query-test--entry
                   :scheduled (orgzly-query-test--ts 2026 7 10)))
        (past (orgzly-query-test--entry
               :scheduled (orgzly-query-test--ts 2026 7 1))))
    (should (orgzly-query-test--match "s.eq.today" today))
    (should-not (orgzly-query-test--match "s.eq.today" tomorrow))
    (should (orgzly-query-test--match "s.ge.today" today))
    (should (orgzly-query-test--match "s.ge.today" tomorrow))
    (should-not (orgzly-query-test--match "s.ge.today" past))
    (should (orgzly-query-test--match "s.lt.today" past))
    (should-not (orgzly-query-test--match "s.lt.today" today))
    (should (orgzly-query-test--match "s.gt.today" tomorrow))
    (should-not (orgzly-query-test--match "s.gt.today" today))
    (should (orgzly-query-test--match "s.ne.today" tomorrow))
    (should-not (orgzly-query-test--match "s.ne.today" today))))

(ert-deftest orgzly-query-match-deadline-week ()
  (let ((in6d (orgzly-query-test--entry
               :deadline (orgzly-query-test--ts 2026 7 15)))
        (in9d (orgzly-query-test--entry
               :deadline (orgzly-query-test--ts 2026 7 18))))
    ;; d.1w: deadline before end of the day one week from now
    (should (orgzly-query-test--match "d.1w" in6d))
    (should-not (orgzly-query-test--match "d.1w" in9d))))

(ert-deftest orgzly-query-match-hour-granularity ()
  (let ((in30m (orgzly-query-test--entry
                :scheduled (orgzly-query-test--ts 2026 7 9 14 55)))
        (in2h (orgzly-query-test--entry
               :scheduled (orgzly-query-test--ts 2026 7 9 16 30))))
    ;; s.1h: before the end of the hour one hour out (16:00 exclusive)
    (should (orgzly-query-test--match "s.1h" in30m))
    (should-not (orgzly-query-test--match "s.1h" in2h))))

(ert-deftest orgzly-query-match-event ()
  (let ((today (orgzly-query-test--entry
                :events (list (orgzly-query-test--ts 2026 7 9 10 0))))
        (nextweek (orgzly-query-test--entry
                   :events (list (orgzly-query-test--ts 2026 7 16)))))
    (should (orgzly-query-test--match "e.today" today))
    (should-not (orgzly-query-test--match "e.today" nextweek))
    (should (orgzly-query-test--match "e.ge.today" nextweek))))

;; ─── Select + sort ───────────────────────────────────────────────────────────

(ert-deftest orgzly-query-select-sorts-by-priority ()
  (let* ((a (orgzly-query-test--entry :title "a" :state "TODO" :priority "C" :pos 1))
         (b (orgzly-query-test--entry :title "b" :state "TODO" :priority "A" :pos 2))
         (c (orgzly-query-test--entry :title "c" :state "TODO" :pos 3)) ; eff. B
         (q (orgzly-query-parse "i.todo o.p"))
         (out (orgzly-query-select (list a b c) q (orgzly-query-test--ctx))))
    (should (equal (mapcar (lambda (e) (alist-get 'title e)) out)
                   '("b" "c" "a"))))
  ;; Descending flips it
  (let* ((a (orgzly-query-test--entry :title "a" :priority "C" :state "TODO" :pos 1))
         (b (orgzly-query-test--entry :title "b" :priority "A" :state "TODO" :pos 2))
         (q (orgzly-query-parse "i.todo .o.p"))
         (out (orgzly-query-select (list a b) q (orgzly-query-test--ctx))))
    (should (equal (mapcar (lambda (e) (alist-get 'title e)) out)
                   '("a" "b")))))

(ert-deftest orgzly-query-select-default-scheduled-sort ()
  "A query with s. sorts by scheduled time by default; nils last."
  (let* ((late (orgzly-query-test--entry
                :title "late" :scheduled (orgzly-query-test--ts 2026 7 9) :pos 1))
         (early (orgzly-query-test--entry
                 :title "early" :scheduled (orgzly-query-test--ts 2026 7 1) :pos 2))
         (q (orgzly-query-parse "s.today"))
         (out (orgzly-query-select (list late early) q (orgzly-query-test--ctx))))
    (should (equal (mapcar (lambda (e) (alist-get 'title e)) out)
                   '("early" "late")))))

(ert-deftest orgzly-query-select-default-book-position ()
  (let* ((b2 (orgzly-query-test--entry :book "beta" :title "b2" :pos 5))
         (a9 (orgzly-query-test--entry :book "alpha" :title "a9" :pos 9))
         (a1 (orgzly-query-test--entry :book "alpha" :title "a1" :pos 1))
         (q (orgzly-query-parse ""))
         (out (orgzly-query-select (list b2 a9 a1) q (orgzly-query-test--ctx))))
    (should (equal (mapcar (lambda (e) (alist-get 'title e)) out)
                   '("a1" "a9" "b2")))))

(provide 'orgzly-query-test)
;;; orgzly-query-test.el ends here
