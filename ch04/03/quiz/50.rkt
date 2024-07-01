#lang sicp

;; see amb-evaluator
(define (analyze-ramb exp)
  (let ([cprocs (map analyze (amb-choices exp))])
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ;; 随机尝试执行某一个可能值的求值
            (let* ([choices-len (length choices)]
                   [next-idx (random choices-len)]
                   [next-choice (list-ref choices next-idx)]
                   [rest-choices (remove next-choice
                                         choices)])
              (next-choice env
                           succeed
                           (lambda ()
                             (try-next rest-choices))))))
      (try-next cprocs))))
