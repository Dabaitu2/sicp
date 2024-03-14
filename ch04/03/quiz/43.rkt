#lang sicp
(#%require
 "../../../common/data/conventional-interface.rkt")

;; solve the quiz:
;; Mary Ann Moore’s father has a yacht and so has each of his four friends:
;;
;; [Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker]
;;
;; Each of the five also has one daughter
;;  and each has named his yacht after a daughter of one of the others.
;; (每个人都用别人女儿的名字给自己的游艇命名)
;;
;; 1. Sir [Barnacle]’s yacht is the [Gabrielle],
;; 2. Mr. [Moore] owns the [Lorna];
;; 3. Mr. [Hall] the [Rosalind].
;; 4. The [Melissa], owned by [Colonel Downing], is named after Sir [Barnacle]’s daughter.
;; 5. [Gabrielle]’s father owns the yacht that is named after [Dr. Parker]’s daughter.
;;
;; Question: Who is Lorna’s father?

;; a. 非确定性解法
(define (distinct? items)
  (cond
    [(null? items) true]
    [(null? (cdr items)) true]
    [(member (car items) (cdr items)) false]
    [else (distinct? (cdr items))]))

(define (require p)
  (if (not p) (amb)))

(define (make-father father-name yacht daughter-name)
  (list father-name yacht daughter-name))
(define (father-name father)
  (car father))
(define (father-yacht father)
  (cadr father))
(define (father-daugher father)
  (caddr father))

;; general but might slow way
(define (yacht-puzzle)
  (let ([fathers
         (list
          (make-father
           'Barnacle
           'Gabrielle
           (amb 'Lorna 'Rosalind 'Melissa 'Mary))
          (make-father
           'Moore
           'Lorna
           (amb 'Gabrielle 'Rosalind 'Melissa 'Mary))
          (make-father
           'Hall
           'Rosalind
           (amb 'Gabrielle 'Lorna 'Melissa 'Mary))
          (make-father
           'Downing
           'Melissa
           (amb 'Gabrielle 'Lorna 'Rosalind 'Mary))
          (make-father
           'Parker
           'Mary
           (amb 'Gabrielle 'Lorna 'Rosalind 'Melissa)))])
    (define (get-father-from-father-name father-name)
      (car (filter (lambda (item)
                     (eq? (car item) father-name))
                   fathers)))
    (define (get-father-from-daugher-name daughter-name)
      (car (filter (lambda (item)
                     (eq? (caddr item) daughter-name))
                   fathers)))
    (require (distinct? (map (lambda (item)
                               (father-daugher item))
                             fathers)))
    (require (eq? (father-daugher
                   (get-father-from-father-name 'Moore))
                  'Mary))
    (require (eq? (father-yacht (get-father-from-father-name
                                 'Barnacle))
                  'Gabrielle))
    (require (eq? (father-yacht (get-father-from-father-name
                                 'Moore))
                  'Lorna))
    (require (eq? (father-yacht (get-father-from-father-name
                                 'Hall))
                  'Rosalind))
    (require (eq? (father-yacht (get-father-from-father-name
                                 'Downing))
                  'Melissa))
    (require (eq? (father-name (get-father-from-daugher-name
                                'Melissa))
                  'Barnacle))
    (require (eq?
              (father-yacht (get-father-from-daugher-name
                             'Gabrielle))
              (father-daugher (get-father-from-father-name
                               'Parker))))
    fathers))

(define stime (runtime))
(yacht-puzzle)
(display (list "Time Taken: " (- (runtime) stime)))

;; b. if we are not told that Mary Ann’s last name is Moore
;; that means skip the require
;; (require (eq? (father-daugher (get-father-from-father-name 'Moore)) 'Mary))

;; and we can get 2 posiible answers



;; c.
;; Faster way with some logic position arrangement
(define (get-father-from-daugher-name daughter-name fathers)
  (car (filter (lambda (item)
                 (eq? (caddr item) daughter-name))
               fathers)))

(define (yacht-puzzle-2)
  (let ([Moore-rel
         (make-father
          'Moore
          'Lorna
          (amb 'Gabrielle 'Rosalind 'Melissa 'Mary))])
    (require (eq? (father-daugher Moore-rel) 'Mary))
    (let ([Barnacle-rel
           (make-father
            'Barnacle
            'Gabrielle
            (amb 'Lorna 'Rosalind 'Melissa 'Mary))])
      (let ([Hall-rel
             (make-father
              'Hall
              'Rosalind
              (amb 'Gabrielle 'Lorna 'Melissa 'Mary))])
        (let ([Downing-rel
               (make-father
                'Downing
                'Melissa
                (amb 'Gabrielle 'Lorna 'Rosalind 'Mary))])
          (let ([Parker-rel (make-father 'Parker
                                         'Mary
                                         (amb 'Gabrielle
                                              'Lorna
                                              'Rosalind
                                              'Melissa))])
            (let ([fathers (list Barnacle-rel
                                 Moore-rel
                                 Downing-rel
                                 Hall-rel
                                 Parker-rel)])
              (require (distinct?
                        (map (lambda (item)
                               (father-daugher item))
                             fathers)))
              (require (eq? (father-name
                             (get-father-from-daugher-name
                              'Melissa
                              fathers))
                            'Barnacle))
              (require (eq? (father-yacht
                             (get-father-from-daugher-name
                              'Gabrielle
                              fathers))
                            (father-daugher Parker-rel)))

              fathers)))))))

(newline)
(define stime2 (runtime))
(yacht-puzzle-2)
(display (list "Time Taken: " (- (runtime) stime2)))
