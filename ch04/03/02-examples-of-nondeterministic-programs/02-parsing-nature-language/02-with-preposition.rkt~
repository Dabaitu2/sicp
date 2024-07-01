#lang sicp

;; amb 实例2: 自然语言解析
(define (require p)
  (if (not p) (amb)))

;; 到目前为止，amb 的主要作用是能够构成 require 从而方便的描述约束条件
;; 但如果我们的句子结构变得更加复杂，它就可以处理更多内容, 因为我们往往需要进行 "回溯"
;; 去尝试另一种句法结构判断

;; 增加介词 & 介词短语判断
;; 介词短语 = 介词 + 名词短语
(define prepositions '(prep for to in by with))
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

;; 一个动词短语
;; 可以是一个简单的动词 ('verb xxx) we [love] money
;; 也可以是一个动词短语 + 一个介词短语 ('verb-phrase verb-phrase prepositional-phrase) -> we [wait for the cat] / we [fight for the future]
;; (不过，我们可以想见，如果句子合法，最终动词短语的第一个被解析的单词将一定是一个简单动词，不然递归不会终止)
;; 在这里，amb 就会发挥"回溯"的作用, 当我们遇到一个 verb 的时候，我们可能会检测它是否能够构成 verb + 介词短语
;; 还是只是一个简单动词，这就通过 amb 来实现
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (display "try-extend verb ")
    (display verb-phrase)
    (newline)
    (define v-answer
      (amb verb-phrase
           (maybe-extend
            (list 'verb-phrase
                  verb-phrase
                  (parse-prepositional-phrase)))))

    (display "selected-verbs ")
    (display v-answer)
    (newline)
    v-answer)
  (maybe-extend (parse-word verbs)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; 我们可以进一步去细化名词短语的定义
;; a cat in the class => 简单名词短语
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        ;; 冠词 + 名词
        (parse-word articles)
        (parse-word nouns)))

;; 名词短语则是
;; 1. 简单名词短语 => a cat
;; 2. 一个名词短语 + 一个介词短语 => a cat in the class
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    ;; 产生它自身 (noun-phrase simple-noun-phrase)
    ;; 或者一个包含自身的名词短语 (noun-phrase (simple-noun-phrase xxx) (prepositional-phrase xxx))
    (display "try-extend noun: ")
    (display noun-phrase)
    (newline)
    (define n-answer
      (amb noun-phrase
           (maybe-extend
            (list 'noun-phrase
                  noun-phrase
                  (parse-prepositional-phrase)))))
    (display "select-noun: ")
    (display n-answer)
    (newline)
    n-answer)
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ([found-word (car *unparsed*)])
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ([sent (parse-sentence)])
    (require (null? *unparsed*))
    sent))

;; (parse '(the student with the cat sleeps in the class))
;; (sentence
;;  (noun-phrase
;;   (simple-noun-phrase (article the) (noun student))
;;   (prep-phrase
;; (prep with)
;; (simple-noun-phrase (article the) (noun cat))))
;;  (verb-phrase
;;   (verb sleeps)
;;   (prep-phrase
;;    (prep in)
;;    (simple-noun-phrase (article the) (noun class)))))

(parse
 '(the professor lectures to the student with the cat))
(newline)
(display "try again")
(newline)
(amb)
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

;; 理论上多次调用应该能够返回多个值，但是这里的 amb 实现并不能撤销 set!
;; 所以这里 tree 会 exausted, 替换为我们自己的 eval 实现后，此处应该就能正常运行了
;; (sentence
;;  (simple-noun-phrase (article the) (noun professor))
;;  (verb-phrase
;;   (verb lectures)
;;   (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student)) (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
