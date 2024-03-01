#lang racket

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

;; 考虑如下表达式
;; (let ([a 1])
;;   (define (f x)
;;     (define b (+ a x))
;;     (define a 5)
;;     (+ a b))
;;   (f 10))

;; 这个表达式按照我们正文的解释方法无法正常执行
;; 因为 b 的 set 操作也先于 a
;; 导致 a 还没有赋值就被使用

;; 为了使得 define 的顺序不再重要，我们可以如下设计:
;; 对 define 的顺序, 在 scan 时进行拓扑排序，保证没有依赖的变量先 define
;; 例如对于上面的例子，我们分析出 a 不依赖任何变量，而 b 依赖 a
;; 因此可以构建 DAG a->b, 再按照拓扑排序将结果输出，针对每一个结果完成赋值
;; 从而保证没有变量会在依赖变量未赋值前被赋值
;; 1. 如果 (define var primitive) var 创建一个 node
;; 2. 如果 (define var other var2) 创建一个 edge var -> var2
;; 3. 如果 (define var (expression))
;;   3.1 如果是 procedure => 创建 edge 将 var 指向 procedure 的实际参数
;;   3.2 如果不是 procedure => 解析表达式，递归的获取 operands, 直到最后读取到原始值或者某个变量
;;     例如 (define x (+ a (- b (add d e))))
;;     -> 获取 a，解析 (- b (add d e))
;;     -> 获取 b, 解析 (add d e)
;;     -> 获取 d, e
;;     最终建立 x->a, x->b, x->d, x->e, b->a, d->a, e->a 的 边，加入 DAG
;;   扫描完成后，通过拓扑排序输出依赖序列，再将赋值按照依赖序列创建到 (let body) 中

;; 这个实现针对我们要学习的东西来说过分复杂了，所以就略过～



;; P.S
;; 下面是 拓扑排序 的 racket 实现
;; constructor
(define (make-graph)
  '())
(define (graph-nodes graph)
  (map car graph))

;; api
(define (add-node graph node)
  (if (assoc node graph)
      graph
      ;; 邻接表
      (cons (cons node '()) graph)))
(define (add-edge graph node1 node2)
  (let ([nodes-pair1 (assoc node1 graph)]
        [nodes-pair2 (assoc node2 graph)])
    ;; 确保两个节点都存在于图中
    (if (and nodes-pair1 nodes-pair2)
        ;; 如果两者都存在，则为每个节点更新其相邻列表
        (map
         (lambda (pair)
           ;; 对于每个节点对(pair)，检查它是否是我们要连接的两个节点之一
           ;; 如果是，则将另一个节点添加到邻接列表中(如果尚未添加)
           ;; 否则，只返回原始对(pair)不变
           (if (eq? pair nodes-pair1) ; 检查是否为第一个节点
               ; 更新第一个节点的邻接列表，添加第二个节点(如果尚未存在)
               (cons
                node1
                (cons node2
                      ; 确保不会重复添加相同的边缘
                      ; 如果node2已经在node1的列表里了就不加了。
                      (remove*
                       '(node2)
                       (cdr pair)
                       ; 这里使用cdr获取现有邻接列表部分
                       ; 不更改其余部分(即car部分即key=node1部分)。
                       ; 即: cdr pair 返回现有邻接列表。
                       ;     car pair 返回key=node1。
                       ;     cons ... 将新元素和旧list结合成新list。
                       ;     cons ... 将key和新list结合成pair形式。
                       )))
               pair))
         ;; 对整个图graph进行映射(map)，从而产生更新后带有新边缘的新图graph版本。
         graph)
        ;; 如果至少有一个点不存在，则返回原始图形不变
        graph)))

;; 使用示例

(define empty-graph (make-graph))

; 这里使用let*顺序执行各步骤临时绑定g0->g3到最终结果g，
; 因为Racket里没有像common lisp 那样progn/prog1可以多步骤顺序执行操作，
; 所以用let*来确保操作按顺序执行并传递给下一步操作。

;; 拓扑排序 Helpers
(define (in-degree graph node)
  ;; 用 apply 是因为需要参数是一个 list
  (let ([edges (apply append (map cdr graph))])
    ;; 获得符合条件的数量, 这本质上就是边中指向 node 的数量，也就是入度
    (count (lambda (x) (equal? x node)) edges)))

;; 找到入度为 0 的节点
(define (find-zero-in-degree-nodes graph)
  (filter (lambda (node) (= 0 (in-degree graph node)))
          (graph-nodes graph)))

;; 删除指向某个节点的所有边, 同时从图中删除这个节点
;; 会和入度一起判断，当删除的时候这个节点就该被输出了
(define (remove-node-and-edges graph node)
  (let ([graph-without-node
         (remove* (list (assoc node graph)) graph)])
    (map (lambda (pair)
           (if (not (eq? node (car pair)))
               (cons (car pair)
                     (remove* (list node) (cdr pair)))
               pair))
         graph-without-node)))

(define (topological-sort-helper graph sorted)
  ; Find all nodes with in-degree of zero.
  ; These nodes do not have any dependencies and can be processed.
  (let ([zero-in-degree-nodes (find-zero-in-degree-nodes
                               graph)])
    (if (null? zero-in-degree-nodes)
        ; If there are no such nodes but the graph still has nodes,
        ; that means there are cycles in the graph, and we cannot perform a topological sort.
        (if (not (null? (graph-nodes graph)))
            (error
             "Graph has cycles, cannot perform topological sort")
            ; Topological sort is complete, return reversed list of sorted elements.
            (display (reverse sorted)))
        (for-each
         (lambda (node)

           (let ([new-graph (remove-node-and-edges graph
                                                   node)])

             (topological-sort-helper new-graph
                                      (cons node sorted))))
         zero-in-degree-nodes))))

;; 函数式解法
(define nodes '(a b c))
(define edges '((b c) (b a) (c a)))
(define (run g)
  (topological-sort-helper
   (fold-left
    (lambda (graph edge) (apply add-edge (cons graph edge)))
    (fold-left (lambda (graph node) (add-node graph node))
               g
               nodes)
    edges)
   '()))

(run empty-graph)
(newline)

;; 过程式解法
(let* ([g0 empty-graph]
       [g1 (add-node g0 'a)]
       [g2 (add-node g1 'b)]
       [g3 (add-node g2 'c)]
       [g4 (add-edge g3 'b 'c)]
       [g5 (add-edge g4 'b 'a)]
       [g6 (add-edge g5 'c 'a)])
  (topological-sort-helper g6 '()))
