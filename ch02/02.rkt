#lang sicp
(#%require "../common/math.rkt")
(#%require "../common/painter/wave.rkt")

;; ========================================================================================================================
;; 2.2.1 序列
;; list 是 cons (1 cons (2 cons (..))) 这种嵌套写法的便捷表示
;; lisp 中 cons 这种 数据组合方式 具有闭包性质
;; 就是表明他组合其数据对象得到的结果本身还可以通过同样的操作进行组合
;; 这也是抽象代数中的定义
;; lisp 中闭包还有另一层定义，表示了一种为表示自由的变量的过程使用的计数
;; 这也就是 JS 中所说的闭包
;; (JS 的函数捕获了外部的变量使得其不能被垃圾回收, 而这个变量并不是 JS 函数定义的约束变量)
;; 我们可以把序列近似看成链表或数组
(define one-through-four (list 1 2 3 4))
;; 以上的结果会得到一个 list (1, 2, 3, 4), 这个表并不等于 (list 1 2 3 4)
;; 这个过程，如果直接调用 （1，2，3，4） 1 会被认为是过程中的一部分导致出错
;; 我们把这个表理解成一个美化输出就行了

;; 和 cons 一样，我们可以用 car / cdr 去取出第一项的数据和后面的 cons
;; 从这里也可以看出 list 本身就是 cons 的反复组合
(car one-through-four) ;; 1
(cdr one-through-four) ;; (2,3,4)
(cons 10 one-through-four) ;;(10, 1, 2, 3, 4)

;; 实现类似于求数组某一项的操作
;; 还是递归, 但是是尾递归，所以性能应该没问题
(define (list-ref items n)
  (if (= n 0) (car items) (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3) ;; 16

;; 实现求数组长度的操作
;; null 是 scheme 提供的，用于检查参数是不是空 list
(define (length items)
  (if (null? items) 0 (+ 1 (length (cdr items)))))
(length squares)

;; 使用迭代方案（本质上就是尾递归
(define (length2 items)
  (define (length-iter a count)
    (if (null? a) count (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))
(length2 squares)

;; 数组的append 操作
;; 组合两个 list
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(append squares squares)

;; list 的高阶过程
;; 正常情况下需要缩放一个 list 需要这么做
(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(scale-list (list 1 2 3 4 5) 10)

;; 这样的操作可以用 map 来抽象
;; 针对 list 中的每一个元素依次应用 proc 过程
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items)) (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(define (scale-list-2 items factor)
  (map (lambda (x) (* x factor)) items))

(scale-list-2 (list 2 3 4 5 6) 10)




;; ========================================================================================================================
;; 2.2.2 层次型结构
;; (cons
;;    (cons 1 (cons 2 ))
;;    (cons 3 (cons 4 ))
;; )
;; 这里打印的是 ((1,2), 3, 4)
;; 表示 cons 有一种特殊表示法叫做 dotted-pair
;; 即 (cons a b) -> (a . b)
;; 而对于 (list a b) ->  (cons a (cons b nil)) -> (a . (b . nil))
;; 而由于 lisp 具有一个化简特性
;; (a . (b ...)) -> (a b ...)
;; 因此 代换到这里是 ((1 2) . (3 4)) -> ((1 2) 3 4)
(cons (list 1 2) (list 3 4))

;; 而对于最外层是 list 的情形
;; 由于多了一层 list, cons 套 cons 变成了这样
;; ((1 2) . ((3 4) . nil)) (nil 可以直接省略)
;; ((1 2) . ((3 4))) -> ((1 2) (3 4)) 只化简一次，导致结果仍然包含括号
(list (list 1 2) (list 3 4))

;; x 是一种多个 list 构成的结构，这种叫做层次型结构
;; 和数据结构中的 tree 很相似
(define x (cons (list 1 2) (list 3 4)))
(length x) ;;  作为 list 而言, 结果为 3

;; 求叶子节点的数
(define (count-leaves x)
  (cond
    [(null? x) 0]
    [(not (pair? x))
     1] ;; pair 用于判断是不是 cons, 而树中非 pair 的就是叶子
    [else
     (+ (count-leaves (car x)) (count-leaves (cdr x)))]))

(count-leaves x) ;; 作为树而言, leaves = 4

;; 与 map 处理序列这种强有力的高阶抽象类似
;; map + 递归同样也是处理树的一种强有力的抽象
;; 下面是一种普通递归实现的 scale-tree
(define (scale-tree tree factor)
  (cond
    [(null? tree) nil]
    [(not (pair? tree)) (* tree factor)]
    [else
     (cons (scale-tree (car tree) factor)
           (scale-tree (cdr tree) factor))]))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

;; 通过结合 map, 我们可以进一步抽象它
(define (scale-tree-2 tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* subtree factor)))
       tree))

(scale-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)




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

;; 实现 accumulate (reduce)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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



;; ========================================================================================================================
;; 2.2.4 实例: 一种图形语言
;; 这个例子更加清晰的展现了抽象和组合的强大之处
;; 使用 procedure 而非 list 来构造数据对象
;; 在这里，我们仍然使用抽象屏障的方式来做，也就是说，我们假设已经有了 wave
;; 实现基于其上的高阶过程，然后再实现底层 wave

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define wave4_alter (flipped-pairs wave))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; 逐渐抽象出复杂的图形生成模式
(define (sqaure-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; flipped-pairs 和 square-limit 都是将一个 painter 的四个 copy 安排在一个正方形的模式中
;; 差异仅仅在于这些 copy 的旋转角度
;; 我们可以把这种模式也抽象出来, 先 beside 再 below
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; 抽象出模式后再定义一次 flipped-pairs
(define (flipped-pairs_2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

;; 同样也可以抽象 square-limit
(define (square-limit-2 painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))
    ))

;; ((flipped-pairs_2 wave) test-frame)
;; ((sqaure-limit wave 4) test-frame)
;;
;; 实现 Frame
;; origin vector specify the offset of the frame's origin from some absolute origin in the plane (translate X/Y)
;; edge vectors specify the offsets of the fframe's corner from its origin. (rotateZ + scaleX/Y)
;;
;; 在这里，我们依旧使用抽象屏障，首先定义
;; 一个constructor make-frame
;; 他利用 三个向量(1 origin vector + 2 edge vectors) 产生一个 Frame
;; originVector 代表了原点位置
;; edgeVector   代表了缩放大小
;; 需要定义 selector: origin-frame edge1-frame edge2-frame
;;
;; 接下来: 我们使用单位正方形的坐标 (0 ≤ x, y ≤ 1) 去描述图像
;; 也就是说我们只是只在 0-1 范围内去指定一些点，线，面，而这个 0-1 的单位正方形会被映射到 frame 中
;; 
;; 对于每个 Frame 我们要为他关联一个 Frame Coordinate map 来实现这个事情
;; 借助其完成图像的位移和伸缩
;;
;; 这个映射会将单位正方形变换到对应的 frame 中 
;; 针对单位正方形中的任意一个点，我们都以向量 v = (x, y) 表示，那么这样的映射关系便为这个公式
;; Origin(Frame) + x * Edge1(Frame) + y * Edge2(Frame)
;;
;; x * Edge1(Frame) + y * Edge2(Frame) 的几何意义
;;
;; 单位正方形中的点 (x, y) ,  将其看作一个向量 v1
;; v1 放到 Edge1， Edge2 这两条边形成的矩形中，假设映射结果为向量 v2 (newX, newY)
;; 我们要求这个 k1 和 k2, 我们又知道 v1 存在于单位正方形中，它的 x,y 刚好就是代表它占 1 这个单位的比例
;; 那么就可以直接代换到 k1 k2, 也就是 v2 = x * Edge1(frame) + y * Edge2(frame) = (newX, newY)
;; 
;; Origin(Frame) + v2 对应的就是这个点相对于原点需要在 origin-frame 的 x，y 方向进行平移， 最终获得了这样的映射关系
;;
;; 在这样的映射下，在单位正方形坐标中
;; (0, 0) 被映射到 frame 的原点 Origin(Frame) + 0 * edge1(frame) + 0 * edge2(frame) = origin(frame)
;; (1，1) 被映射到与原点对角的那个点，
;; (0.5, 0.5) 被映射到给定框架的中心点。
;;
;; 我们可以通过下面过程建立起框架的坐标映射
;; xcor-vect 是获得 v 向量的 x
;; ycor-vect 是获得 v 向量的 y
;; scale-vect 将 edge Vector 进行放缩
;; add-vect 对向量进行加法运算
;; 所以这个函数其实就是 上面的向量表达式的代码表示
;; 这个过程的结果依然是一个过程
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; ((frame-coord-map a-frame) (make-vect 0 0)) -> (origin-frame a-frame)

;;
;; 实现 Painter
;; Painter 其实是一个绘图过程, 可以理解为我们在 canvas 上面
;; 调用了一大堆命令所做的事情
;; 而 frame 其实类似于 canvas 画布本身, 我们建立画布就是要指定一个相对于屏幕
;; 坐标系原点的位置，然后指定 x, y 和旋转角度 (在这里使用向量)
;; 而 painter 本身是基于一个 单位正方形绘图，被放缩到这个画布上的
;; 同样，还是使用抽象屏障
;; 我们在这里实现一个画折线的画家
;;
;; segment-list 是单位正方形上的线段序列
;; segments->painter 将其转化画到 frame 上
;;
;; draw-line 接受 frame 上的两个点，在 frame 上画出一条直线
;; start-segment 获得单位正方形线段的起点
;; end-segment 获得单位正方形线段的终点
(define (segements->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


