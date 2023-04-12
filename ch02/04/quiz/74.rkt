#lang racket

(#%require "../../../common/generic/tag.rkt")
(#%require "../../../common/generic/apply.rkt")

(define (get-record key file)
  (apply-generic 'get-record key file))

(define (get-salary key file)
  (apply-generic 'get-salary key file))

(define (install-division-1-plugin)
  (define (tag x)
    (attach-tag 'A x))

  (define (get-name record)
    (car record))
  (define (get-salary record)
    (cadr record))

  (define (get-record key file)
    (cond [(null? file) (error "no result")]
          [(eq? key (get-name (car file)))
           (car file)]
          [else (get-record name (cdr file))]))

  (define (get-salary key file)
    (let ((record (get-record key file)))
      ;; real implementation
      (get-salary record)))

  (put 'get-name 'division-1 get-name)
  (put 'get-record 'division-1 get-record)
  (put 'get-salary 'division-1 get-salary)
  'done)

(define (fine-employee-record key files)
  (if (null? files)
      #f
      (let ((current-file (car file-list)))
        (if (get-record key current-file)
            (get-record key current-file)
            (fine-employee-record key (cdr files))))))
