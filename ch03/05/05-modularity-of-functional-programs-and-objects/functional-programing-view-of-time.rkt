#lang sicp

(#%require "../../../common/data/stream.rkt")

;; 使用流的思路去避免使用时间和状态概念以重新实现 "取款" 的逻辑 
(define (stream-withdraw balance amount-stream)
  (cons-stream balance
               (stream-withdraw
                (- balance (stream-car amount-stream))
                (stream-cdr amount-stream))))


(stream-ref (stream-withdraw 25 (cons-stream 20 (cons-stream 10 nil))) 2)


;; 使用流在解决并发问题时通常是很有效的, 它在保持了优雅的代换模型时, 消解了"对象"模拟的引入。又实现了使用内部状态一般的对外表现
;; 但是依然存在着需要解决的问题
;; 也就是，当我们的并发系统涉及到现实中多个独立对象之间的交互时, 它可能还是需要引入时间的概念
;; 假设我们存在之前所提到的 "关联账户"
;; 使用流来解决并发的访问账户时可能的顺序问题，实际需要对两个存取款操作的流进行 "merge"
;; 然而，这个 merge 本身就是依赖现实世界的 时间顺序，我们不可以简单的让这两个流的操作一前一后的发生就好了
;; 因为我们无法保证这两个用户真的就这样操作。比如，有个账户的访问频率可能极低。我们不可能让另一个账户始终等待前一个账户
;; 存在结果再去执行。
;; 我们往往还是需要根据现实操作的发生顺序来决定 merge 的顺序，这意味着我们还是不得不引入明确的 "时间" 的概念
