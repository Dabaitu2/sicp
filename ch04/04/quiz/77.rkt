#lang sicp

;; 完整实现见 dir 77 / playground.rkt 
;; 实现更优的 not 和 lisp-value 过程，
;; 使得其在完成过滤之前先等待某些必要条件

;; 也就是说， 由于依赖 simple-stream-flatmap
;; 至少传入的 stream 不应该默认就是空
;; 简单的办法是，把 not 和 lisp 安排到 and 子查询列表的后面, 保证它们不会第一个被使用， 但题目中提到不希望这样做

;; 按照题目中给出的办法，增加一个 promise 来解决这些问题

(define (make-frame bindings promises)
  (cons bindings promises))

(define (make-binding variable value)
  (cons variable value))

(define (first-binding bindings)
  (car bindings))

(define (rest-binding bindings)
  (cdr bindings))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (make-promise vars then)
  (cons vars then))

(define (promise-vars promise)
  (car promise))

(define (promise-then promise)
  (cdr promise))

(define (frame-bindings frame)
  (car frame))

(define (frame-promises frame)
  (cdr frame))

(define (first-promise promises)
  (car promises))

(define (rest-promises promises)
  (cdr promises))

;; 为 frame 扩展一个 promise
(define (extend-promises frame promise)
  (make-frame (frame-bindings frame)
              (cons promise (frame-promises frame))))

;; 扩展 binding 后，尝试将产生的新 frame 中的 promise 进行 fullfil
(define (extend-binding variable value frame)
  (fullfil-promises
   (make-frame
    (cons (make-binding variable value)
          (frame-bindings frame))
    ;; extend binding 的情况下， 仅判断 variable 满足条件后执行 promise
    (frame-promises #f frame))))

(define (fullfil-promises force? frame)
  (let ([promises (frame-promises frame)])
    (if (null? promises)
        frame
        (execute-promises force? promises frame))))

;; frame 可能随着 execute-promise 会在迭代中被动态改变
(define (execute-promises force? promises frame)
  (if (null? promises)
      frame
      (execute-promises
       (rest-promises promises)
       (execute-promise force?
                        (first-promise promises)
                        frame))))

;; 如果 force 强制执行，则跳过 variable 检查
;; 什么时候强制执行？print 的时候!
(define (execute-promise force? promise frame)
  (let ([then (promise-then promise)])
    (if (eq? force? #t)
        (then frame)
        (if (enough-variable-bound? promise frame)
            (then frame)
            frame))))

;; 判断一个变量 是否在 frame 中被绑定
;; 需要递归检测直到遇到值 / 或者没有对应值的绑定
;; 这个和 extend-if-consistent 很相似
(define (bounded? pattern frame)
  (cond
    [(var? pattern)
     (let ([binding (binding-in-frame var frame)])
       (and binding
            (bounded? (binding-variable binding) frame)))]
    [(pair? pattern)
     (and (bounded? (car pattern))
          (bounded? (cdr pattern)))]
    ;; else 就对应值的情况
    [else #t]))

;; 判断是否所有的 variable 都有 绑定了
(define (enough-variable-bound? promise frame)
  (define (iter vars)
    (if (null? vars)
        #t
        (and (bounded? (car vars) frame)
             (enough-variable-bound? (cdr vars) frame))))
  (iter (promise-vars promise) frame))

;; 从 (not xxx) 的 xxx pattern 中获取变量
(define (variables pattern)
  (cond
    [(null? pattern) '()]
    [(var? pattern) (cons pattern '())]
    [(pair? pattern)
     (append (variables (car pattern))
             (variables (cdr pattern)))]
    [else '()]))

;; 针对  stream 中 的 frame 挨个挨个的 fullfil promise, 相当于做清理
;; fullfil 结果是 frame  / failed , 根据其表现转化为 singleton-stream / empty-stream
;; 最后的结果再整合成一个大的 stream
(define (stream-fullfil-promises force? frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (let ([fullfil-result (fullfil-promises force? frame)])
       (if (eq fullfil-result 'failed)
           the-empty-stream
           (singleton-stream fullfil-result))))
   frame-stream))

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
                 (singleton-stream (make-frame '() '()))))))
       (query-driver-loop)])))

;; negate / lisp-value 会对 frame-stream 做过滤， 可能减少 frame 数量
;; 因此需要被 promise 处理， 而 and, or 不会减少 frame 的量， 因此不用
;; negate 不是要扩充 frame， 而是从现有 frame 中剔除一些, 因此最多也就是返回原始的 frame
(define (negate operands frame-stream)
  (let ([negate-promise
         (make-promise
          (variables operands)
          ;; 这个 filter 会在适当的时机被调用， 并对 frame 做处理,
          ;; 产生一个新的 frame 或者 'failed
          (lambda (frame)
            (if (stream-null?
                 (qeval (negated-query operands)
                        (singleton-stream frame)))
                frame
                'failed)))])
    ;; 对于每一个 frame,  把 negate 对应的 promise 扩展进去, 构造成一个新的 frame 并用 map 重新链接
    (stream-map (lambda (frame)
                  (extend-promises frame negate-promise))
                frame-stream)))

(define (lisp-value call frame-stream)
  (let ([lisp-value-promise
         (make-promise
          (variables operands)
          (lambda (frame)
            (if (execute
                 (instantiate call frame
                   [lambda
                       (v f)
                     (error "Unknown pat var: LISP-VALUE"
                            v)]))
                frame
                'failed)))])
    (stream-map (lambda (frame)
                  (extend-promises frame negate-promise)))))

(define (uniquely-asserted operands frame-stream)
  (let ([unique-promise
         (make-promise
          (variables operands)
          (lambda (frame)
            (let ([result-frame
                   (qeval (unique-query operands)
                          (singleton-stream frame))])
              ;; 和 negate 不同, unique 执行的结果本身可能会扩展 frame 的 binding,
              ;; 因此我们需要返回 uniq 执行过的 frame, 而不是传入的 frame
              (if (stream-single? result-frame)
                  (stream-car result-frame)
                  'failed))))])
    (stream-map (lambda (frame)
                  (extend-promises frame unique-promise)))))
