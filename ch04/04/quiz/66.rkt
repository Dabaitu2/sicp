#lang sicp

;; according to quiz 65
;; the result could be duplicate
;; which leads to the wrong result


;; if we want to salvage this situation
;; (accumulation-function ⟨variable⟩ ⟨query pattern⟩)

(sum ?amount 
     (and (job ?x (computer programmer)) 
          (salary ?x ?amount)))
;; there should be a filter functor to filter out the duplicate items by keys

