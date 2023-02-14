#lang racket

(define nil '())

;; helpers
;; 实现 accumulate (reduce / fold)
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; 取出 seqs 的第一项 map 为新的 list 做 accumulate 得到结果后，递归计算剩下的
      (cons (accumulate op init (map (lambda (seq) (car seq)) seqs))
            (accumulate-n op init (map (lambda (seq) (cdr seq)) seqs)))))

;; 构造线性代数的抽象
;; 用 i 表示行, j 表示列
;; 我们将向量 v = (vi) 表示为数的序列
;;     将矩阵 m = (mij) 表示为向量(矩阵行)的序列
;; 例如，对于如下矩阵，
;;
;;  1 2 3 4
;;  4 5 6 6
;;  6 7 8 9
;;
;; 我们将其表示为
;; ((1 2 3 4),
;;  (4 5 6 6),
;;  (6 7 8 9))


;; 下面是针对线性代数的常见计算方法
;; 向量点积 -> returns the sum Σ_iv_iw_i;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; 矩阵乘向量
;; 返回向量 t, 其中 ti =  Σjmijvj
;; 每一个向量元素计算中 i 不动 v 动
;;
;; ((1 2 3 4),  1    a m11v1 + m12v2 + m13v3 + m14v4
;;  (4 5 6 6),  2 -> b m21v1 + m21v2 + m23v3 + m24v4 -> ti = Σj(mij)*vj
;;  (6 7 8 9))  3    c m31v1 + m31v2 + m33v3 + m34v4
;;              4
(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

;; 转置矩阵
;; 返回矩阵 n, where nij =mji.
(define (transpose mat)
  (accumulate-n cons nil mat))

;; 矩阵乘矩阵
;; returns the matrix p, where pij =Σk(m_{ik})n_{kj};
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;; (define matrix (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
;; (define v (list 1 2 3 4))
;; (define w (list 4 3 2 1))
;; 
;; (dot-product v w)
;; (define transposed (transpose matrix))
;; (display transpose)
;; (matrix-*-vector matrix v)
;; (matrix-*-matrix matrix transposed)

