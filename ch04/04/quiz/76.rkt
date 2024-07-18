#lang sicp

;; parallel conjoin operation
;; however, here's a issue:
;; as chapter 4.4.3 said, if the operands `not` accept an empty-stream without any frame in it
;; it might return false result, cause the empty stream input to `not` will directly return empty
;; and furtherly make `not` return empty-stream, which is not correct
;; TODO:  we will try to fix this after quiz 4.67
(define (new-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (unify-stream (qeval (first-disjunct conjuncts)
                           frame-stream)
                    (new-conjoin (rest-conjuncts conjuncts)
                                 frame-stream))))

(define (unify-stream s1 s2)
  (cond
    [(stream-null? s1) s2]
    [(stream-null? s2) s1]
    [else
     (stream-flatmap
      (lambda (frame1)
        (stream-flatmap (lambda (frame2)
                          (unify-frame frame1 frame2))
                        s2))
      s1)]))

;; unify 两个 frame ，任何一个 binding 不满足条件就直接返回 the-empty-stream
;; 否则走到递归的终点 (frame1 遍历完毕), 返回被 extend 完成的 frame2
(define (unify-frame frame1 frame2)
  ;; 递归遍历 frame1 (cdr), 因此一定是 frame1 会没有
  (if (null? frame1)
      (singleton-stream frame2)
      (let ([binding (car frame1)])
        (let ([var (binding-variable binding)]
              [val (binding-value binding)])
          (let ([unify-result (unify-match var val frame2)])
            (if (eq? unify-result 'failed)
                the-empty-stream
                (unify-frame (cdr frame1) frame2)))))))
