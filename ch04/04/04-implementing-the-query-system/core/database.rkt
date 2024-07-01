#lang sicp
(#%require "../../../../common/data/stream.rkt")
(#%require "../registry.rkt")
(#%require "../predicates.rkt")
(#%require "./elements.rkt")

;; ======================= read =========================
;; assertions, it's a stream
(define THE-ASSERTIONS the-empty-stream)
(define (fetch-assertions pattern frame)
  ;; 是否使用索引
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-all-assertions)
  THE-ASSERTIONS)
(define (get-indexed-assertions pattern)
  (get-stream 'assertion-stream (index-key-of pattern)))

;; rules
(define THE-RULES the-empty-stream)
(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))
(define (get-all-rules)
  THE-RULES)
;; rules 会同时检查两种情况
;; 1 conclusion 开头对应的就是查询 pattern 的 car
;; 2 conclusion 开头是变量, 从而有可能需要被检查
;; 因此将两者共同 append
(define (get-indexed-rules pattern)
  (stream-append (get-stream 'rule-stream
                             (index-key-of pattern))
                 (get-stream 'rule-stream '?)))

;; ===================== write =========================
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  ;; 插入索引
  (store-assertion-in-index assertion)
  ;; 插入全局
  (let ([old-assertions THE-ASSERTIONS])
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ([old-rules THE-RULES])
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ([key (index-key-of assertion)])
        (let ([current-assertion-stream
               (get-stream 'assertion-stream key)])
          (put (cons-stream assertion
                            current-assertion-stream)
               'assertion-stream
               key)))))

(define (store-rule-in-index rule)
  ;; rule 和函数有一点不同在于它不需要有一个所谓的 "名字"
  ;; 例如 (live-near ?a ?b) 是一个 rule 的 conclusion
  ;; 而 (?a next-to ?b) 同样也是一个 rule 的 conclusion
  (let ([pattern (conclusion rule)])
    (if (indexable? pattern)
        (let ([key (index-key-of pattern)])
          (let ([current-rule-stream
                 (get-stream 'rule-stream key)])
            (put (cons-stream rule current-rule-stream)
                 'rule-stream
                 key))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat)) (var? (car pat))))

;; 求一个模式的索引 key
(define (index-key-of pat)
  (let ([key (car pat)]) (if (var? key) '? key)))

;; 如果一个模式的开头是 constant 则使用索引
;; 对于 rule 而言, 其实变量开头也是 constant （"?" 符号)
(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (get-stream key1 key2)
  (let ([s (get key1 key2)]) (if s s the-empty-stream)))

(#%provide fetch-assertions
           fetch-rules
           add-rule-or-assertion!)
