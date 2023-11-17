#lang sicp

(#%require "./handle-multiple-shared-resources.rkt")

;; even use serialized-exchange we might still meet deadlock issues
;; presume we have two guys who both want to exchange there accounts



;; (define (serialized-exchange account1 account2)
;;   (let ([serializer1 (account1 'serializer)]
;;         [serializer2 (account2 'serializer)])
;;     ((serializer1 (serializer2 exchange)) account1
;;                                           account2)))

;; for person 1, the serialized-exchange will be a1, a2 he get into serializer-a1 first
;; for person 2, the serialized-exchange will between a2, a1, he/she got into serializer-a2 first
;; now, for person1, he start to request serializer-a2 to give it space to execute task
;; but the serializer-a2 has been requested by person2
;; meanwhle, person2 still need wait serializer-a1 to do the same things, that's how deadlock constructed

;; a possible way to solve deadlock problems partially is to mark every resource an identification number
;; and rewrite serialized-exchange so that a process will always attempt to enter a procedure protecting the lowest-numbered account first

;; but this method can't solve all issues
