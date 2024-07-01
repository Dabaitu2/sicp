#lang sicp


;; old rule
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;; 而对于旧的规则, 由于 and 本质上是按照从上到下的顺序执行的
;; suprvisor 是合法的断言, 会产生一些 input streams, 产出 output stream 或使得查询终止
;; 从而不会陷入无限循环。


(rule (outranked-by ?staff-person ?boss) 
      (or (supervisor ?staff-person ?boss)
      (and (outranked-by ?middle-manager ?boss) 
           (supervisor ?staff-person ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)
;; 而新的规则交换了 and 里的顺序, 
;; (outranked-by ?staff-person ?boss) 的规则每次调用都是一样的输入，使得这个查询无法终止 








