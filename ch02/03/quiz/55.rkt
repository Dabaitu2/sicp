#lang racket

;; (car ''something) is treated by the interpreter as:
;; (car (quote (quote something)))
;; The first occurrence of 'quote' quotes the next entity
;; (quote something),which is actually a list with two elements, so
;; caring this list yields 'quote. However, this is just a quoted
;; symbol, not a procedure, typing quote in the interpreter prints:
;; =>(#@keyword . #<primitive-macro! #<primitive-procedure quote>>)
;; whereas typing 'quote just yielded it literally.

(car ''abracadabra)
