#lang sicp
(#%require "../../../../common/data/stream.rkt")
(#%require "./predicates.rkt")
(#%require "./core/database.rkt")
(#%require "./core/instantiate.rkt")
(#%require "./core/binding.rkt")
(#%require "./qeval.rkt")

(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results: ")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ([q (query-syntax-process (read))])
    (cond
      [(assertion-to-be-added? q)
       (add-rule-or-assertion! (add-assertion-body q))
       (newline)
       (display "Assertion added to database.")
       (query-driver-loop)]
      [else
       (newline)
       (display output-prompt)
       (display-stream
        ;; qeval 结果还是一个 stream，将每一个 frame 都实例化
        (stream-map
         (lambda (frame)
           (instantiate q frame
             [lambda
                 (v f)
               ;; 将在读入阶段被拆分成 ('? var) 的 ?var 重新以字符串形式显示
               (contract-question-mark v)]))
         ;; 在执行完内部的 qeval 后，将 stream 中每一个 frame 的可能残存的 promise 都 fullfil 掉
         ;; 结果会被内部的 simple-stream-flatmap 扁平化
         (stream-fullfil-promises
          #t
          (qeval q
                 (singleton-stream (make-frame '() '())))
          )))
       (query-driver-loop)])))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

;; 递归处理一个 pair 中的 symbol
(define (map-over-symbols proc exp)
  (cond
    [(pair? exp)
     (cons (map-over-symbols proc (car exp))
           (map-over-symbols proc (cdr exp)))]
    [(symbol? exp) (proc exp)]
    [else exp]))

;; 展开 question mark
;; 经此函数处理后，?var 会变成 (list '? var)
;; 一般 symbol 则保持不动
(define (expand-question-mark symbol)
  (let ([chars (symbol->string symbol)])
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

;; 收缩 question mark
;; 将 expand 展开的形式还原用于展示
(define (contract-question-mark variable)
  (string->symbol
   (string-append
    "?"
    ;; 在 apply rule 时，rule 内的变量会被改名 (增加一个全局 id)
    ;; 例如 ('? . 'x) => ('? . (1 . 'x))
    ;; @see ./core/element.rkt
    ;; 因此这里要判断一下 var 部分第一位是否是 number, 如果是则说明此变量是被改名过的
    ;; 展示的时候，我们将其写成 ?var-id 的形式
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        ;; 否则说明不是 rule apply 过程中完成变量查找和扩充的 frame
        ;; 直接展示成 ?var 即可
        (symbol->string (cadr variable))))))

(#%provide query-driver-loop)
