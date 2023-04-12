#lang racket
(#%require "./normal-form.rkt")

;; 复数的直角坐标系表示更适合加减法
;; 极坐标系表示更适合乘除法
;; 我们的目标是，在使用这些加减乘除的时候，对于 both normal form or polar form 都可以使用
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; 很明显我们可以通过两种方式来完成任务
;; 1. 使用 pair 来表示实部和虚部, 并通过变换得到模长和幅角 (normal-form.rkt)
;; 2. 使用 pair 来表示模长和辐角, 并通过变换得到实部和虚部 (poler-form.rkt)
;; 不管是使用极坐标系还是直角坐标系，通过数据抽象都可以通过统一的表示进行操作

;; 在这里我们使用最小承诺原则 (Principle of Least Commitment)
;; 指在设计系统或编写代码时选择尽可能少的假设和限制。
;; 这意味着在设计和编写代码时，应该尽可能地保持灵活性和可扩展性，以便能够适应未来的变化。
;; 通过遵守最小承诺原则，可以减少对系统的约束，并提高其适应性和可维护性。
;; 在这里，我们没有对 complex 的表示做出太多的假设，而是提供了许多比较细的 selector 设计来
;; 方便用户进一步地为这类 api 实现多种数据表示, 而实际上如何实现这些的操作被推迟到了最后一刻

;; 不过, 如果我们同时在一个系统中使用了两类不同的表示法表示相同的数据。那么我们还需要找到进一步的办法
;; 去区分它们，否则我们就不知道用户需要的结果到底是极坐标还是直角坐标下的表示了, 因此我们需要 Tagged Data 来帮我们了
