#lang sicp

;; 封装一下 constraint unit 的使用, 这样就不用显式传递字符串 req 了
;; 不过这个并不保证 constraint 真的有新值了，只是一个通知
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constriant)
  ((connector 'connect) new-constriant))
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond
      [(null? items) 'done]
      [(eq? (car items) exception) (loop (cdr items))]
      [else
       (procedure (car items))
       (loop (cdr items))]))
  (loop list))

(define (make-connector)
  (let ([value false]
        ;; 用于校验后面取消值的 retractor 和 informant 是否是一个
        ;; 如果不是就跳过, 就是个校验逻辑
        [informant false]
        [constraints '()])
    (define (set-my-value newval setter)
      (cond
        [(not (has-value? me))
         (set! value newval)
         (set! informant setter)
         (for-each-except setter
                          inform-about-value
                          constraints)]
        [(not (= value newval))
         (error "Contradiction" (list value newval))]
        [else 'ignored]))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          ;; 删除 informant
          (begin
            (set! informant false)
            ;; 由于是触发者 constraints 触发的 inform-about-no-value
            ;; 如果这里不排除它会导致无限递归
            (for-each-except retractor
                             inform-about-no-value
                             constraints))
          'ignored))

    ;; 连接的 constriant 这里没有设置断开的 api
    (define (connect new-constriant)
      (if (not (memq new-constriant constraints))
          (set! constraints
                (cons new-constriant constraints)))
      (if (has-value? me)
          (inform-about-value new-constriant))
      'done)

    ;; message passing
    (define (me request)
      (cond
        ;; 持有值的本质就是有 informant (因为值是 informant 发起的)
        [(eq? request 'has-value?)
         (if informant true false)]
        [(eq? request 'value) value]
        [(eq? request 'set-value!) set-my-value]
        [(eq? request 'forget) forget-my-value]
        [(eq? request 'connect) connect]
        [else
         (error "Unknown operation: CONNECTOR" request)]))
    me))

(#%provide has-value?
           get-value
           set-value!
           forget-value!
           connect
           make-connector)
