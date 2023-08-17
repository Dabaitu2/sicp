#lang sicp
;; if we don't invoke this proc specially
;; the initial output of the wire might be error
;; take and-gate as an example
;; if we use some wire input 1, input 2 with signal 1, output 1 with signal 0
;; We hope that we can get 1 of the output, but the final result will be 0
;; because the initial procedures wasn't be triggered
;; we still need take the initial signal of wires into account
;; and the actions' execution will happened at the moment add-action! specified (+ delay current-time)
