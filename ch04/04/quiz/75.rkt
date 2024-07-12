#lang sicp

;; Achieve a new special form called unique
;; unique should succeed if there is precisely one item in the data base satisfying a specified query.
;;
;; For example,
;; Input:
;; (unique (job ?x (computer wizard)))
;; Output:
;; (unique (job (Bitdiddle Ben) (computer wizard)))

;; Input:
;; (unique (job ?x (computer programmer)))
;; Output:
;; <Empty Stream> cuz there is more than one computer programmer

;; Moreover:
;; (and (job ?x ?j) (unique (job ?anyone ?j)))
;; should list all the jobs that are filled by only one person

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(and (supervisor ?person ?supervisor)
     (unique (supervisor ?anyone ?supervisor)))


(define (uniquely-asserted operands frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ([result-frame (qeval (unique-query operands)
                                (singleton-stream frame))])
       (if (stream-single? result-frame)
           result-frame
           the-empty-stream)))
   frame-stream))

(define (init-compound-query)
  (put conjoin 'qeval 'and)
  (put disjoin 'qeval 'or)
  (put negate 'qeval 'not)
  (put lisp-value 'qeval 'lisp-value)
  (put always-true 'qeval 'always-true)
  (put uniquely-asserted 'qeval 'unique))


;; usecase
(and (supervisor ?a ?boss)
     (unique (supervisor ?anyone ?boss)))
