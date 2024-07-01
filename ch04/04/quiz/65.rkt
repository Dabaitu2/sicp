#lang sicp

;; Big wheel 是那种领导领导的人
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))


(wheel ?who)

;;; Query results:
(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))


;; 为啥 Warbucks Oliver 会出现 4 次呢？
;; 因为 Bitdiddle 同时领导了 3 个人, Eben 也领导了 1 个人
;; 参考 instance.rkt
;; wheel 在做查询的时候, 针对 (supervisor ?middle-manager ?person)
;; 遇到 middle-manager 是 Ben 的时候，下一步的查询
;; 会产出 3 个可能的结果, 他们最终产出的 binding frame
;; 会是三个可能的 ?x 和同样的 ?middle-manager 和同样的 ?person

;; 对于 Eden 也是同样, 这也会产生一个 (?x ?Eden ?Warbucks)
;; 因此最终的结果会出现4次
