#lang racket

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

(let* ([g0 empty-graph]
       [g1 (add-node g0 'a)]
       [g2 (add-node g1 'b)]
       [g3 (add-node g2 'c)]
       [g4 (add-edge g3 'a 'b)]
       [g5 (add-edge g4 'a 'c)]
       [g6 (add-edge g5 'b 'c)])
  (topological-sort-helper g6 '()))
