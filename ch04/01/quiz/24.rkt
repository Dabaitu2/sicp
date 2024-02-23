#lang sicp

;; use belowing code
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ([input (read)])
    (define stime (runtime))
    (let ([output (eval input the-global-environment)])
      (newline)
      (display (list "Time Taken: " (- (runtime) stime)))
      (newline)
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
