#lang sicp

;; Peter: (set! balance (+ balance 10))
;; Paul: (set! balance (- balance 20))
;; Mary: (set! balance (- balance (/ balance 2)))
;;
;;
;; a. sequence possibility
;; Peter -> Paul -> Mary   
;; Peter -> Mary -> Paul
;; Paul -> Peter -> Mary
;; Paul -> Mary -> Peter
;; Mary -> Peter -> Paul
;; Mary -> Paul -> Peter


;; if processes can interleave each other
;; cause both lookup value, set value operations can be seperated
;; so we can seperate peter into peter-get peter-set, for paul and mary the situation is the same
;; then for peter we will have peter-get peter-plus10 peter-set 3 variants
;; Paul is almost the same, has 3 variants
;; for marry, acutally while running (- balance (/ balance 2)) would not change the balance
;;
;; 这个问题实际上是一个经典的计数问题，涉及到组合数学。我们可以通过以下步骤来解决：
;; 1. 首先，我们要明确这里有多少个"槽位"可以放置元素。由于每个元素都必须出现在序列中，因此总共有9个槽位。
;; 2. 其次，我们需要考虑的是每种元素（A、B、C）在给定条件下能占据多少不同的槽位。A1必须在A2和A3之前，所以A的三个元素（A1,A2,A3）必须按照特定的顺序出现。同样的规则也适用于B和C。也就是说，我们可以把每种元素看作一个整体来处理。
;; 3. 于是问题就变成了：有9个位置可供三个整体（每个整体包含三个元素）占据，他们一共有多少种可能的排列？
;; 4. 这正好就是一个组合问题：从9个位置中选取3个位置给"A"，那么有C(9,3)种方式；接着剩下6个位置中选取3个位置给"B"，那么有C(6,3)种方式；最后剩下3个位置全部给"C"。
;; 5. 因此总共的可能性就是 C(9,3) * C(6,3) * C(3,3) = 84 * 20 * 1 = 1680种。
;; c(m,n) = m!/((m-n)!* n!

;; so generally, it should has 1680 ways
;; and sorry I can't draw so many diagrams, so skip

