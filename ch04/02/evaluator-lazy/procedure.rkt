#lang sicp

(#%require "./utils.rkt")
(#%require "./special-forms/definition.rkt")
(#%require "./derived-forms/let.rkt")
(#%require
 "../../../common/data/conventional-interface.rkt")

(define (scan-out-defines proc-body)
  (let ([result
         (fold-right
          (lambda (cur acc)
            (if (definition? cur)
                ;; 构造变量提升kk
                (let ([def-var
                        (list (definition-variable cur)
                              ;; 变量会被 evaluated 一次，因此要多加一层 quote
                              ''*unassigned)]
                      ;; 构造变量赋值用于占位
                      [def-set
                        (list 'set!
                              (definition-variable cur)
                              (definition-value cur))])
                  ;; 这里没有把顺序倒回去，因为对于 binding / set! 定义而言这应该不重要
                  (list (cons def-var (car acc))
                        (cons def-set (cadr acc))))
                ;; 如果非内部定义，就直接原样放回去
                (list (car acc) (cons cur (cadr acc)))))
          (list '() '())
          proc-body)])
    (let ([bindings (car result)] [body (cadr result)])
      (if (null? bindings)
          body
          ;; 如果被处理成了 let, 那么原来的 list body 就可能变成一个 let 过程，
          ;; 而这就不符合一般 body 的定义了(list of exp)
          ;; 无法被 eval-sequence 正常的求值, 因此要包一层
          (list (make-let bindings body))))))

;; 处理一般过程
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (procedure-parameters p)
  (cadr p))
(define (procedure-body p)
  (caddr p))
(define (procedure-environment p)
  (cadddr p))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (primitive-procedure? procedure)
  (tagged-list? procedure 'primitive))
(define (primitive-implementation proc)
  (cadr proc))

;; 注册基本过程
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'remainder remainder)
        (list 'quotient quotient)
        ;; (list 'imap map)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        ;; and so on..
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; 这里用的是底层语言的 apply, 而不是我们实际的那个 apply
;; 这好像有一点投机取巧的意思，似乎我们绕过了 "如何将参数应用给一个过程"  这样一个关键问题
;; 但是，回到本章的定义:
;; 我们这里做的是元语言抽象，是基于一门语言去实现另一门语言
;; 而不是要从 0 - 1 去设计一个语言，实际上，即使是 c/c++ 这类语言，本质上也是
;; 基于底层的汇编语言做的元语言抽象，只是他们的抽象层次更低，执行 apply 这种操作
;; 被转化为了汇编指令而已。
;; 我们在这一章节的目标, 就是利用 lisp 的底层能力去实现另一个语言，只是另一个语言恰好也是 lisp 方言而已
(define apply-in-underlying-scheme apply)
(define (apply-primitive-procedure procedure arguments)
  (apply-in-underlying-scheme
   (primitive-implementation procedure)
   arguments))

(#%provide procedure-parameters
           procedure-environment
           procedure-body
           compound-procedure?
           primitive-procedure?
           primitive-implementation
           primitive-procedures
           primitive-procedure-names
           primitive-procedure-objects
           apply-primitive-procedure
           make-procedure)
