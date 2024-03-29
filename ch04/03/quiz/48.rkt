#lang sicp

;; from http://community.schemewiki.org/?sicp-ex-4.48

(define (require p)
  (if (not p) (amb)))

;; changed
(define adjectives '(adj good bad cool awful nice))
(define adverbs '(adv independently carefully))
(define conjunctions '(conj and or but yet when while))

(define prepositions '(prep for to in by with))
(define nouns '(noun student professor cat class apple boy))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;; added
(define (parse-simple-verb)
  (newline)
  (display "parse-simple-verb unparsed: ")
  (display *unparsed*)
  (newline)
  (amb (list 'simple-verb
             (parse-word adverbs)
             (parse-word verbs))
       (parse-word verbs)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (newline)
    (display "parse-verb-phrase maybe-extend unparsed: ")
    (display *unparsed*)
    (newline)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                ;; changed
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; added
(define (parse-simple-noun-phrase)
  (newline)
  (display "parse-simple-noun-phrase unparsed: ")
  (display *unparsed*)
  (newline)
  (amb
   (list 'simple-noun-phrase
         ;; 冠词 + 形容词 + 名词
         (parse-word articles)
         (parse-word adjectives)
         (parse-word nouns))
   (list 'simple-noun-phrase
         ;; 冠词 + 名词
         (parse-word articles)
         (parse-word nouns))
   ))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (car *unparsed*)])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-sentence)
  (define (maybe-extend sentence)
    (newline)
    (display "parse-sentence maybe-extend unparsed: ")
    (display *unparsed*)
    (newline)
    (amb sentence
         (maybe-extend (list 'compound-sentence
                             sentence
                             (parse-word conjunctions)
                             (parse-sentence)))))
  (maybe-extend (parse-simple-sentence)))

(define (parse-simple-sentence)
  (list 'simple-sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ([sent (parse-sentence)])
    (require (null? *unparsed*))
    sent))


;; TODO: 由于 sicp 给出的 amb 实现均不会撤销 set, 44-49 的题目都暂时无法完美运行
;; 等待 amb 解释器完成后再试
;; compound sentences are sentences like
;; the cat eats the mouse when the sun sets
;; 我们目前的实现还没有支持宾语，所以 a verb b 这种东西不会被认识, 必须在动词后面加介词
;; (parse '(the good professor carefully lectures to the student with the cat))
(parse '(the nice boy eats to the good apple))
