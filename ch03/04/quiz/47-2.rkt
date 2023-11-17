#lang sicp

;; semaphore which support n processors achieved by atomic test-and-set!
(define (make-semaphore n)
  (define (accquire)
    ;; in multiple processors' supposition
    ;; if test-and-set! return true, means now we don't have space for new task running, need recursive run accquire (hang) to wait
    (if (test-and-set! n)
        (accquire)
        'ok))

  (define (release)
    (set! n (+ n 1))
    'ok)
  (define (dispatch m)
    (cond
      [(eq? m 'accquire) (accquire)]
      [(eq? m 'release) (release)]
      [else (error "Unknown op --MAKE-SEMAPHORE" m)]))
  dispatch)


;; it's just a example implementation of atomic test-and-set! operation
(define (test-and-set! n)
  (if (= n 0)
      true
      (begin
        (set! n (- n 1))
        false)))
