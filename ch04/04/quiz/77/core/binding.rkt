#lang sicp
(#%require "../predicates.rkt")
(#%require "../../../../../common/data/stream.rkt")

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

(define (binding-in-frame variable frame)
  (assoc variable (frame-bindings frame)))

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
       force?
       (rest-promises promises)
       (execute-promise force?
                        (first-promise promises)
                        (rest-promises promises)
                        frame))))

;; 如果 force 强制执行，则跳过 variable 检查
;; 什么时候强制执行？print 的时候!
;; 注意 promise 执行完成之后要从 frame 中删除
(define (execute-promise force?
                         cur-promise
                         rest-promises
                         frame)
  (let ([then (promise-then cur-promise)])
    (let ([new-promise (make-frame (frame-bindings frame)
                                   rest-promises)])
      (if (eq? force? #t)
          (then new-promise)
          (if (enough-variable-bound? cur-promise frame)
              (then new-promise)
              frame)))))

;; 扩展 binding 后，尝试将产生的新 frame 中的 promise 进行 fullfil
(define (extend-binding variable value frame)
  (fullfil-promises
   #f
   (make-frame
    (cons (make-binding variable value)
          (frame-bindings frame))
    ;; extend binding 的情况下， 仅判断 variable 满足条件后执行 promise
    (frame-promises frame))))

;; 判断一个变量 是否在 frame 中被绑定
;; 需要递归检测直到遇到值 / 或者没有对应值的绑定
;; 这个和 extend-if-consistent 很相似
(define (bounded? pattern frame)
  (cond
    [(var? pattern)
     (let ([binding (binding-in-frame pattern frame)])
       (and binding
            (bounded? (binding-value binding) frame)))]
    [(pair? pattern)
     (and (bounded? (car pattern) frame)
          (bounded? (cdr pattern) frame))]
    ;; else 就对应值的情况
    [else #t]))

;; 判断是否所有的 variable 都有 绑定了
(define (enough-variable-bound? promise frame)
  (define (iter vars frame)
    (if (null? vars)
        #t
        (and (bounded? (car vars) frame)
             (iter (cdr vars) frame))))
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
       (if (eq? fullfil-result 'failed)
           the-empty-stream
           (singleton-stream fullfil-result))))
   frame-stream))

(#%provide make-binding
           binding-variable
           binding-value
           binding-in-frame
           extend-binding
           extend-promises
           stream-fullfil-promises
           variables
           enough-variable-bound?
           make-frame
           frame-promises
           frame-bindings
           make-promise
           first-binding
           rest-binding)
