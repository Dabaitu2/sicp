#lang sicp

;; semaphore which support n processors achieved by mutex
(define (make-semaphore n)
  (let ([current-count 0]
        [mutex
         (make-mutex)]) ;; for keeping current-count modification atomicly
    (define (accquire)
      ;; try accquire the lock for preventing other process try to modify semaphore
      (mutex 'accquire)
      ;; if still has capacity for accquiring semaphore, update current-count and return ok
      (if (< current-count n)
          (begin
            (set! current-count (+ current-count 1))
            (mutex 'release)
            'ok)
          ;; if not, release inner lock and try re-accquire it
          (begin
            (mutex 'release)
            (accquire))))

    (define (release)
      (mutex 'accquire)
      (if (< 0 current-count)
          (begin
            (set! current-count (- current-count 1))
            (mutex 'release)
            'ok)
          (begin
            (mutex 'release)
            (error
             "can't subtract current-count cuz it has been 0 -- RELEASE"))))
    (define (dispatch m)
      (cond
        [(eq? m 'accquire) (accquire)]
        [(eq? m 'release) (release)]
        [else (error "Unknown op --MAKE-SEMAPHORE" m)]))
    dispatch))

;; mutex will create a cell and put it into false by default
(define (make-mutex)
  (let ([cell (list false)])
    (define (the-mutex m)
      (cond
        [(eq? m 'accquire)
         ;; if cell is true, which means some other procedure has acquired the lock, need to do it
         ;; recursively
         (if (test-and-set! cell)
             (the-mutex 'accquire))] ;retry
        [(eq? m 'release) (clear! cell)]))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

;; try to get mutex, if ok, set the mutex status into true
;; here's a potential issue: this procedure should be guaranteed that executed atomicly
;; or this procedure might be interleaved by other processes..
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))
