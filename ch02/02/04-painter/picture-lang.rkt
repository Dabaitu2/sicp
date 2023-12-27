#lang sicp

;; ========================================================================================================================
;; 2.2.4 实例: 一种图形语言
;; 这个例子更加清晰的展现了抽象和组合的强大之处
;; 使用 procedure 而非 list 来构造数据对象
;; 在这里，我们仍然使用抽象屏障的方式来做，也就是说，我们假设已经有了 wave
;; 实现基于其上的高阶过程，然后再实现底层 wave

(define (split transform1 transform2)
  (define (inner painter n)
    (if (= n 0)
        painter
        (let ([smaller (up-split painter (- n 1))])
          (transform1 painter
                      (transform2 smaller smaller)))))
  (inner))

(define right-split (split beside below))
(define up-split (split below beside))

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


;; flipped-pairs 和 square-limit 都是将一个 painter 的四个 copy 安排在一个正方形的模式中
;; 差异仅仅在于这些 copy 的旋转角度
;; 我们可以把这种模式也抽象出来, 先 beside 再 below
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; 抽象出模式后再定义一次 flipped-pairs
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

;; 同样也可以抽象 square-limit
(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))
    ))
