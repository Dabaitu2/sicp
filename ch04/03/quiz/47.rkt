#lang sicp

;; Louis suggest that we can use a more simple way to
;; create that parser
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))


;; but it can not work, cuz it's obviously will
;; cause infinite loop



;; if we switch the order of the two amb
;; it will still cause infinite loop
;; and the infinite loop can happen even faster ;)
(define (parse-verb-phrase)
  (amb
   (list 'verb-phrase
         (parse-verb-phrase)
         (parse-prepositional-phrase))
   (parse-word verbs)
   ))
