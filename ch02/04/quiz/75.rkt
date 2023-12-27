#lang sicp
(#%require "../../../common/math/num-theory.rkt")

;; 我们之前所提到的 数据导向 的设计方案
;; 是将操作通用化, 智能化，通过为不同的数据类型设计不同的实现来完成类型分派
;; 这类似于泛型? 或者是注册表, 这是一种中心化的通用操作方式

;; 而实际上，与之对应的是，我们可以把数据本身作为一个"智能数据",
;; 它表现为一个过程
;; 我们只是向这个数据过程中传入操作名, 就可以由这个过程返回给我我们需要的结果
;; 这更像是我们常见的运行时对象(面向对象? 带有一些方法，自身维护数据, 这是一种去中心化的通用操作方式
;; 在下面的 constructor 中, 返回的就是一个过程

(define (make-from-real-imag x y)
  ;; 产生的不是一个数据实体，而是一个过程, 不过这个过程通过闭包存储了数据
  (define (dispatch op)
    (cond [(eq? op 'real-part) x]
          [(eq? op 'imag-part) y]
          [(eq? op 'magnitude)
           (sqrt (+ (square x) (square y)))]
          [(eq? op 'angle)
           (atan y x)]
          [else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op)]))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)]))
  dispatch)

;; 接下来，我们在调用方法的时候，实际上就是把 op 传给 dispatch
;; 然后获得实际的数据，这就是 “消息传递”
;; 这个名称来自于一个数据对象作为接收“消息”的实体的形象。
;; 我们已经在2.1.3节中看到了消息传递的一个例子
;; 我们看到如何只用过程而不是数据对象来定义cons、car和cdr。
;; (define (cons x y)
;;  (define (dispatch m)
;;     (cond ((= m 0) x))
;;           ((= m 1) y))
;;           (else (error "Argument not 0 or 1: cons" m))
;;   dispatch)
;;
;; (define (car z) (z 0))
;; (define (cdr z) (z 1))
;; 消息传递也是组织通用型操作系统的好办法
(define (apply-generic op arg) (arg op))
