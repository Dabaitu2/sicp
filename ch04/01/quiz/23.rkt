#lang sicp


;; Alyssa's way to achieve analyze-sequence
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond
      [(null? (cdr procs)) ((car procs) env)]
      [else
       ((car procs) env)
       (execute-sequence (cdr procs) env)]))
  (let ([procs (map analyze exps)])
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

;; 她的方案存在的潜在可优化点为
;; 由于她通过递归的调用反复去 execute-sequence
;; 从而去执行被 analyze 过的 (lambda (env) ...)
;; 相较于正文给出的解法
;; 每一个 exp 都存在额外的 cond 分情况控制的开销
;; 正文将 cond 分情况分析在外界就处理了，而 Alyssa 的做法却需要在后续接着执行

