#lang sicp

;; ========================================================================================================================
;; 2.2.3 用作约定 Interface 的 list
;; 下面的两个函数看起来彼此没有什么关系，但他们都是可以抽象成一些约定接口的组合的
;;
;;  sum-odd-squares
;;  +-------------+      +-------------+       +-------------+      +---------------+
;;  |             |      |             |       |             |      |               |
;;  | enumerate:  |      |   filter    |       |   map:      |      |  accumulate:  |
;;  | tree leaves |----->|   odd?      |------>|   square    |----->|  +, 0         |
;;  |             |      |             |       |             |      |               |
;;  +-------------+      +-------------+       +-------------+      +---------------+
;;
;;
;;  even-fibs
;;  +---------------+      +-------------+       +-------------+       +--------------+
;;  |               |      |             |       |             |       |              |
;;  | enumerate:    |      |  map:       |       |  filter:    |       |  accumulate: |
;;  | integers(0-N) |----> |  fib        |------>|  even?      |------>|  cons, ()    |
;;  |               |      |             |       |             |       |              |
;;  +---------------+      +-------------+       +-------------+       +--------------+
;;
(define (square x)
  (* x x))
(define (sum-odd-squares tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) (if (odd? tree) (square tree) 0)]
    [else
     (+ (sum-odd-squares (car tree))
        (sum-odd-squares (cdr tree)))]))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;; 构造出所有偶数的 fibonacci 数的一个 list
(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ([f (fib k)])
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; (even-fibs 10)

;; 序列 (Sequence) 操作
;; 实现 filter
(define (filter predicate sequence)
  (cond [(null? sequence) nil]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]
        ))

;; 实现 accumulate (reduce), 此乃线性递归
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(accumulate / 1 (list 1 2 3))

(accumulate + 0 (list 1 2 3))

;; 通过这些抽象的 介面 可以将一开始较为复杂的逻辑转化为 signal 的流转
;; 定义一个枚举整数接口
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;; 定义一个枚举 leaves 接口
(define (enumerate-tree tree)
  (cond [(null? tree) nil]
        [(not (pair? tree)) (list tree)]
        [else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree)))]))

;; 重写过程
(define (sum-odd-squares-2 tree)
  (accumulate
   + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs-2 n)
  (accumulate
   cons nil (filter even? (map fib (enumerate-interval 0 n)))))

;; 我们可以观察到，针对其他需求我们依然可以复用许多代码
(define (list-fib-squares n)
  (accumulate
   cons nil (map square (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)


(define (product-of-squares-of-odd-elements sequence)
  (accumulate
   * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))

;; Nested Mappings 嵌套映射
;; 考虑下面的问题:给定了自然数n，找出所有不同的有序对和 i & j，其中1 <= j < i <= n ，使得 i+j 是素数。
;; 例如，假定 n 是 6，满足条件的序对就是:
;;
;;    i | 2 3 4 4 5 6 6
;;    j | 1 2 1 3 2 1 5
;;  ----|----------------
;;  i+j | 3 5 5 7 7 7 11
;;
;;  要想做到这个, 我们需要先 (enumerate 1 n)
;;  对于产生的每一个 i, 再 (enumerate 1 (- i 1))
;;  对于产生的每一个 j, 再 (list i j)
;;  最后将这些序列组合到一起 (accumulate + append 组合多个二级 list)
;;  就可以获得结果

(define (gen-all-i-j n)
  (accumulate append nil (map (lambda (i)
                                (map (lambda (j) (list i j))
                                     (enumerate-interval 1 (- i 1))))
                              (enumerate-interval 1 n))))
(gen-all-i-j 6)

;; 我们通常可以把这样的操作抽象为 flatmap
;; 它用来 append 累积映射 map 结果
;; (由于映射结果是 list, append 这些 list 会消掉这些 list 的括号, 就像是将嵌套 list 打平了, 故命名 flatmap)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; 这样我们就可以结合对 sum 进行 prime? 检测的函数，组合出需要的结果了
;; (cadr n) = (car (cdr n))
;; 所以这一步就是取 list 的第一个和第二个
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; 生成 (i, j, i+j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


;; 将所有序对过滤之后生成需要的结果
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))

                (enumerate-interval 1 6)
                ))))

(prime-sum-pairs 6)

;; 使用 flatmap 完成全排列问题
;; remove 就是反向 filter
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; permutations 算法也是一种 dp;
;; 要想解决 S 的子问题, 需要对于 S 中的每一个元素 x
;; 递归的生成 S-x 的所有排列的序列 (子问题)
;; 然后将这个元素 x 组合到序列的最前面
;; P([1, 2, 3])
;; 1 -> 1, P([2, 3])
;;      1, 2, P(3)
;;      1, 3, P(2)
;; 2 -> 2, P([1, 3])
;;      2, 1, P(3)
;;      2, 3, P(1)
;; 3 -> 3, P([1, 2])
;;      3, 1, P(2)
;;      3, 2, P(1)
(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(permutations (list 1 2 3))

