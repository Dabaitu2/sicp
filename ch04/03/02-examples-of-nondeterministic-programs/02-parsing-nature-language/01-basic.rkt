#lang sicp

;; amb 实例2: 自然语言解析
;; 解析输入的句子，解析其中的语法部分
(define (require p)
  (if (not p) (amb)))

;; 首先需要定义一些单词所属的词类别
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

;; 假设我们有一个简单的句子
;; The cat eats
;; 它理论上就可以分析为：
;; (sentence (noun-phrase (article the) (noun cat))
;; (verb eats))
;; 代表这个句子由 冠词 the, 名词 cat 组成的名词短语 和 动词 eats 组成


;; 分析这个句子
;; 实际上是要产生 sentence 这个结构
;; 这个结构分别由两个部分构成
;; (parse-noun-phrase) (parse-word)
;; 会分别被调用从而求得这两个部分
;; 而 (parse-noun-phrase) 又是由 article 和 noun 构成的
(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-word verbs)))

(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

;; parse-word 则是最终的实现
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

(parse '(the cat eats))
;; Output:
;; (sentence (noun-phrase (article the) (noun cat)) (verb eats))

;; 到目前为止，amb 的主要作用是能够构成 require 从而方便的描述约束条件
;; 但如果我们的句子结构变得更加复杂，它就可以处理更多内容, 因为我们往往需要进行 "回溯"
;; 去尝试另一种句法结构判断

;; 增加介词 & 介词短语判断
;; 介词短语 = 介词 + 名词短语
(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))




