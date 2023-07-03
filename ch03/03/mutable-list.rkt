#lang sicp

;; get-new-pair 在第五章才会实现
#| (define (cons x y) |#
#|   (let ((new (get-new-pair))) |#
#|     (set-car! new x) |#
#|     (set-cdr! new y) |#
#|     new)) |#

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)


;; 下面就是引入赋值操作在数据存在 "共享" 时会破坏同一性的例子
;;  如果我们无法修改 z1 z2, 那么即使 z1 中 car cdr 共享了 x
;;  z1 z2 仍然可以看作是 “同一” 个东西，这是因为他们应用在任何过程中都会产生相同结果
;; 然而一旦可以修改共享数据，情况就变了，它们就不可以再看成同样的东西了，比如下面的例子
(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))
z1
z2

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)


;; lisp 提供的判断两个东西是否 同一 使用的就是 eq? 过程
;; 这将比较两者的指针。
