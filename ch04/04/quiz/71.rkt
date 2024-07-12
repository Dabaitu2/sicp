#lang sicp

;; why simple-query and disjoin procedures are explicitly using delay operation
;; rather than being defined as follows?
;; if we use delay, while infinite loops happened, there's still some meaningful message can be left
;; but if not, nothing could be printed out, only maximum recursion depth exceed error printed and do not help
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append (find-assertions query-pattern frame)
                    (apply-rules query-pattern frame)))
   frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave
       (qeval (first-disjunct disjuncts) frame-stream)
       (disjoin (rest-disjuncts disjuncts) frame-stream))))

;; with this difinition, infinte loop will happened in simple query
(assert! (home a b))
(assert! (rule (home ?a ?b) (home ?a ?b)))

;; with this difinition, infinte loop will happend in disjoin
(assert! (rule (house ?a ?b)
               (or (home ?a ?b) (house ?b ?c))))
