#lang sicp
;; From  https://www.inchmeal.io/sicp/ch-4/ex-4.78.html and it's comments

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (empty-conjunction? exps)
  (null? exps))
(define (first-conjunct exps)
  (car exps))
(define (rest-conjuncts exps)
  (cdr exps))

(define (empty-disjunction? exps)
  (null? exps))
(define (first-disjunct exps)
  (car exps))
(define (rest-disjuncts exps)
  (cdr exps))

(define (negated-query exps)
  (car exps))

(define (unique-query exps)
  (car exps))
(define (predicate exps)
  (car exps))
(define (args exps)
  (cdr exps))

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression contents")))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
  (car (contents exp)))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp)
  (symbol? exp))
(define (rule? statment)
  (tagged-list? statment 'rule))
(define (conclusion rule)
  (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule)) '(always-true) (caddr rule)))

(define rule-counter 0)
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter)))
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (conjoin conjuncts frame)
  (if (empty-conjunction? conjuncts)
      frame
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame))))

(define (disjoin disjuncts frame)
  (if (empty-disjunction? disjuncts)
      (amb)
      (amb (qeval (first-disjunct disjuncts) frame)
           (disjoin (rest-disjuncts disjuncts) frame))))

(define (negate operands frame)
  (if (eq? (if-fail (qeval (negated-query operands) frame)
                    'failed)
           'failed)
      frame
      (amb)))

(define (lisp-value call frame)
  (if (execute (instantiate call frame
                 [lambda
                     (v f)
                   (error "Unknown pat var: LISP-VALUE" v)]))
      frame
      (amb)))

(define (instantiate exp frame
          unbound-var-handler)
  (define (copy exp)
    (cond
      [(var? exp)
       (let ([binding (binding-in-frame exp frame)])
         (if binding
             (copy (binding-value binding))
             (unbound-var-handler exp frame)))]
      [(pair? exp) (cons (copy (car exp)) (copy (cdr exp)))]
      [else exp]))
  (copy exp))

(define (execute exp)
  (apply (eval (predicate exp)
               (scheme-report-environment 5))
         (args exp)))

(define (always-true ignore frame)
  frame)

(define THE-ASSERTIONS '())
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions)
  THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-list 'assertion-list (index-key-of pattern)))

(define THE-RULES '())
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules)
  THE-RULES)
(define (get-indexed-rules pattern)
  (append (get-list 'rule-list (index-key-of pattern))
          (get-list 'rule-list '?)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ([old-assertions THE-ASSERTIONS])
    (set! THE-ASSERTIONS (cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ([old-rules THE-RULES])
    (set! THE-RULES (cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ([key (index-key-of assertion)])
        (let ([current-assertion-list
               (get-list 'assertion-list key)])
          (put 'assertion-list
               key
               (cons assertion current-assertion-list))))))

(define (store-rule-in-index rule)
  (let ([pattern (conclusion rule)])
    (if (indexable? pattern)
        (let ([key (index-key-of pattern)])
          (let ([current-rule-list (get-list 'rule-list
                                             key)])
            (put 'rule-list
                 key
                 (cons rule current-rule-list)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat)) (var? (car pat))))

(define (index-key-of pat)
  (let ([key (car pat)]) (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (get-list key1 key2)
  (let ([s (get key1 key2)]) (if s s '())))

(define (pattern-match pat dat frame)
  (cond
    [(equal? pat dat) frame]
    [(var? pat) (extend-if-consistent pat dat frame)]
    [(and (pair? pat) (pair? dat))
     (pattern-match
      (cdr pat)
      (cdr dat)
      (pattern-match (car pat) (car dat) frame))]
    [else (amb)]))

(define (extend-if-consistent var dat frame)
  (let ([binding (binding-in-frame var frame)])
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (unify-match p1 p2 frame)
  (cond
    [(equal? p1 p2) frame]
    [(var? p1) (extend-if-possible p1 p2 frame)]
    [(var? p2) (extend-if-possible p2 p1 frame)]
    [(and (pair? p1) (pair? p2))
     (unify-match (cdr p1)
                  (cdr p2)
                  (unify-match (car p1) (car p2) frame))]
    [else (amb)]))

(define (extend-if-possible var val frame)
  (let ([binding (binding-in-frame var frame)])
    (cond
      [binding
       (unify-match (binding-value binding) val frame)]
      [(var? val)
       (let ([binding (binding-in-frame val frame)])
         (if binding
             (unify-match var (binding-value binding) frame)
             (extend var val frame)))]
      [(depends-on? val var frame) (amb)]
      [else (extend var val frame)])))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond
      [(var? e)
       (if (equal? var e)
           true
           (let ([b (binding-in-frame e frame)])
             (if b (tree-walk (binding-value b)) false)))]
      [(pair? e)
       (or (tree-walk (car e)) (tree-walk (cdr e)))]
      [else false]))
  (tree-walk exp))

(define (qeval query frame)
  (let ([qproc (get 'qeval (type query))])
    (if qproc
        (qproc (contents query) frame)
        (simple-query query frame))))

(define (simple-query query-pattern frame)
  (amb (find-assertions query-pattern frame)
       (apply-rules query-pattern frame)))

(define (find-assertions pattern frame)
  (pattern-match pattern
                 (an-element-of (fetch-assertions pattern frame))
                 frame))

(define (apply-rules pattern frame)
  (apply-a-rule (an-element-of (fetch-rules pattern frame))
                pattern
                frame))

(define (apply-a-rule rule query-pattern query-frame)
  (let ([clean-rule (rename-variables-in rule)])
    (let ([unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)])
      (qeval (rule-body clean-rule) unify-result))))

(define (rename-variables-in rule)
  (let ([rule-application-id (new-rule-application-id)])
    (define (tree-walk exp)
      (cond
        [(var? exp)
         (make-new-variable exp rule-application-id)]
        [(pair? exp)
         (cons (tree-walk (car exp)) (tree-walk (cdr exp)))]
        [else exp]))
    (tree-walk rule)))

(define (assoc key records)
  (cond
    [(null? records) false]
    [(equal? key (caar records)) (car records)]
    [else (assoc key (cdr records))]))

(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (make-record key value subtable)
  (make-tree (list key value subtable) '() '()))
(define (record-key record)
  (caar record))
(define (record-value record)
  (cadar record))
(define (record-subtable record)
  (cddar record))

(define (set-record-value record value)
  (set-car! (cdar record) value))
(define (set-record-subtable record subtable)
  (set-cdr! (cdar record) subtable))

(define (make-table)
  (let ([local-table (list '*table*)])
    (define (assoc key records)
      (cond
        [(null? records) false]
        [(equal? key (caar records)) (car records)]
        [else (assoc key (cdr records))]))

    (define (lookup key-1 key-2)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record (cdr record) false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ([subtable (assoc key-1 (cdr local-table))])
        (if subtable
            (let ([record (assoc key-2 (cdr subtable))])
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond
        [(eq? m 'lookup-proc) lookup]
        [(eq? m 'insert-proc!) insert!]
        [else (error "Unknown operation -- TABLE" m)]))
    dispatch))

(define my-table (make-table))

(define get (my-table 'lookup-proc))
(define put (my-table 'insert-proc!))

(define (tagged-list? exp tag)
  (if (pair? exp) (eq? (car exp) tag) false))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (init-compound-query)
  (put 'qeval 'and conjoin)
  (put 'qeval 'or disjoin)
  (put 'qeval 'not negate)
  (put 'qeval 'lisp-value lisp-value)
  (put 'qeval 'always-true always-true))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond
    [(pair? exp)
     (cons (map-over-symbols proc (car exp))
           (map-over-symbols proc (cdr exp)))]
    [(symbol? exp) (proc exp)]
    [else exp]))

(define (expand-question-mark symbol)
  (let ([chars (symbol->string symbol)])
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))

(define (lp query)
  (let ([q (query-syntax-process query)])
    (if (assertion-to-be-added? q)
        (begin
          (add-rule-or-assertion! (add-assertion-body q))
          (display "Assertion added to data base."))
        (begin
          (newline)
          (display "Query Results:")
          (instantiate q (qeval q '())
            [lambda
                (v f)
              (contract-question-mark v)])))))

(init-compound-query)

(lp '(assert! (address (Bitdiddle Ben)
                       (Slumerville (Ridge Road) 10))))
(lp '(assert! (job (Bitdiddle Ben) (computer wizard))))
(lp '(assert! (salary (Bitdiddle Ben) 60000)))
(lp '(assert! (address (Hacker Alyssa P)
                       (Cambridge (Mass Ave) 78))))

(lp '(assert! (job (Hacker Alyssa P)
                   (computer programmer))))
(lp '(assert! (salary (Hacker Alyssa P) 40000)))
(lp '(assert! (supervisor (Hacker Alyssa P)
                          (Bitdiddle Ben))))
(lp '(assert! (address (Fect Cy D)
                       (Cambridge (Ames Street) 3))))
(lp '(assert! (job (Fect Cy D) (computer programmer))))
(lp '(assert! (salary (Fect Cy D) 35000)))
(lp '(assert! (supervisor (Fect Cy D) (Bitdiddle Ben))))
(lp '(assert! (job (Tweakit Lem E) (computer technician))))
(lp '(assert! (salary (Tweakit Lem E) 25000)))
(lp '(assert! (supervisor (Tweakit lem e) (Bitdiddle Ben))))
(lp '(assert! (address (Reasoner Louis)
                       (Slumerville (Pine Tree Road) 80))))

(lp '(assert! (job (Reasoner Louis)
                   (computer programmer trainee))))
(lp '(assert! (salary (Reasoner Louis) 30000)))
(lp '(assert! (supervisor (Reasoner Louis)
                          (Hacker Alyssa P))))
(lp '(assert! (supervisor (Bitdiddle Ben)
                          (Warbucks Oliver))))
(lp '(assert! (address (Warbucks Oliver)
                       (Swellesley (Top Heap Road)))))
(lp '(assert! (job (Warbucks Oliver)
                   (administration big wheel))))
(lp '(assert! (salary (Warbucks Oliver) 150000)))
(lp '(assert! (address (Scrooge Eben)
                       (Weston (Shady Lane) 10))))
(lp '(assert! (job (Scrooge Eben)
                   (accounting chief accountant))))
(lp '(assert! (salary (Scrooge Eben) 75000)))
(lp '(assert! (supervisor (Scrooge Eben)
                          (Warbucks Oliver))))
(lp '(assert! (address (Cratchet Robert)
                       (Allston (N Harvard Street) 16))))
(lp '(assert! (job (Cratchet Robert)
                   (accounting scrivener))))
(lp '(assert! (salary (Cratchet Robert) 18000)))
(lp '(assert! (supervisor (Cratchet Robert)
                          (Scrooge Eben))))
(lp '(assert! (address (Aull DeWitt)
                       (Slumerville (Onion Square) 5))))
(lp '(assert! (job (Aull DeWitt)
                   (administration secretary))))
(lp '(assert! (salary (Aull DeWitt) 25000)))
(lp '(assert! (supervisor (Aull DeWitt) (Warbucks Oliver))))
(lp '(assert! (can-do-job (computer wizard)
                          (computer programmer))))
(lp '(assert! (can-do-job (computer wizard)
                          (computer technician))))
(lp '(assert! (can-do-job (computer programmer)
                          (computer programmer trainee))))
(lp '(assert! (can-do-job (administration secretary)
                          (administration big wheel))))
