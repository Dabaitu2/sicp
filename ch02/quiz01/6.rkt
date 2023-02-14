#lang sicp

;; church 计数
;; 这个数没有利用任何一个 “数” 来表示
;; 而是使用了一个过程来表示数字
;; 0 就是对 lambda (x) -> x 这个过程应用了 0 次 f
(define zero (lambda (f) (lambda (x) x)))
;; ((zero f) x) -> x

;; +1 就是为一个邱奇数（一个过程) 多应用一次 f (包装在最外面)
;; 因为 n 是一个形如 zero 的邱奇数, 它需要送入一个 f (但这个 f 是什么不重要)
;; 然后就可以产生对 f 进行 一定次嵌套调用的过程，表示此数的大小
;; +1 正是在外面又套了一层 f 所以表示出了 +1
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

;; 进而我们可以体会到
;; 加法就是把 ab 对应的内部过程进行嵌套的 过程
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
