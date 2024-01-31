#lang sicp


;; a. if sequential evalutation
;; (lambda <vars>)
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>
;; )
;;
;; while evaluating <e3>
;;  ┌──────────────────┐
;;  │ global frame     │
;;  │ ...              │
;;  └──────────────────┘
;; 
;;  ┌──────────────────┐
;;  │ lambda           │
;;  │------------------│
;;  │ frame            │
;;  │                  │
;;  │ vars -> args     │ 
;;  │ u -> e1          │
;;  │ v -> e2          │
;;  └──────────────────┘
;; 
;; b. with variable hoisting
;;
;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>
;;
;;
;;  ┌──────────────────┐
;;  │ global frame     │
;;  │ ...              │
;;  └──────────────────┘
;; 
;;  ┌──────────────────┐
;;  │ lambda           │
;;  │------------------│
;;  │ frame            │
;;  │ vars -> args     │ 
;;  └──────────────────┘
;;
;;  ┌──────────────────┐
;;  │ lambda           │
;;  │------------------│
;;  │ frame            │
;;  │ u -> e1          │ 
;;  │ v -> e2          │ 
;;  └──────────────────┘
;; cause we introduced an extra lambda, which will be evaluated as a new procedure then
;; which would introduce a new env with a new frame
;; but the new lambda has no other elements, by which it could keep the procedure running as usual

;; to achieve the same effect without introduce new env & new frame
;; we can modify the original procedure and construct a new one
;; use define to replace let
;;
;; (lambda <vars>
;;   (define u '*unassigned*)
;;   (define v '*unassigned*)
;;   (set! u <e1>)
;;   (set! v <e2>)
;;   <e3>)
