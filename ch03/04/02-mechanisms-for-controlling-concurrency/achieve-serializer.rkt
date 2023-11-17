#lang sicp

;; achieve serializer by mutex (mutual exclusion)
;; it provide two basic operation: a mutex can be *accquired* or *released*

;; make-serializer return a function that receive a procedure and wrap it with mutex invokaction
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        ;; any other operation that require the mutext resource has to wait this mutex has been released
        (mutex 'accquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))


;; mutex will create a cell and put it into false by default
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'accquire)
             ;; if cell is true, which means some other procedure has acquired the lock, need to do it
             ;; recursively
             (if (test-and-set! cell)
                 (the-mutex 'accquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

;; try to get mutex, if ok, set the mutex status into true
;; here's a potential issue: this procedure should be guaranteed that executed atomicly
;; or this procedure might be interleaved by other processes..
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

(#%provide make-serializer)
