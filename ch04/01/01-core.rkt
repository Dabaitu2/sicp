#lang sicp

;; eval的参数是一个表达式和一个环境。
;; eval 对表达式进行分类，依此引导自己的求值工作。
;; Primitive expression 基本表达式:
;;  1. 对于自求值表达式，比如各种数，直接返回本身
;;  2. eval 从环境中寻找 variable 对应的值
;; Special forms 特殊形式:
;;  1. expression with quote, 返回被引用的表达式
;;  2. 对于赋值或者定义变量, eval 需要被递归的调用去计算出关联到这个变量的值，并且需要修改环境，以建立或者改变对应变量的 bindings
;;  3. if 表达式根据谓词的值的 true & false 去求值其不同的部分
;;  4. lambda 会被转化为一个过程
;;  5. begin 表达式会按照其中的一系列表达式按照顺序求值
;;  6. cond 会被变化为一组嵌套的 if 表达式再求值
;; Combinations 组合式
;;  1. 对于一个过程的 application, eval 会递归的求值其运算符部分 operator 和运算对象部分 operand，然后将这样得到的过程和参数送给 apply 去处理实际的过程应用
;;  过程的 application 就是传入这样一个东西 (eval (add 1 2) env)
;;  (add 1 2) 就是 procedure application
(define (eval exp env)
  (cond
    [(self-evaluating? exp) exp]
    [(variable? exp) (lookup-variable-value exp env)]
    [(quoted? exp) (text-of-quotation exp)]
    [(assignment? exp) (eval-assignment exp env)]
    [(definition? exp) (eval-definition exp env)]
    [(if? exp) (eval-if exp env)]
    [(lambda? exp)
     (make-procedure (lambda-parameters exp)
                     (lambda-body exp)
                     env)]
    [(begin? exp) (eval-sequence (begin-actions exp) env)]
    [(cond? exp) (eval (cond->if exp) env)]
    [(application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env))]
    [else (error "Unknown expression type: EVAL" exp)]))

;; apply 接受两个参数
;; 1. 一个过程, 2 过程的实参
;; 对于基本过程，直接求解基本过程
;; 否则，则挨个求组成过程体的那些表达式，并且利用该过程的实参和形参结合扩展该过程的环境
;; 这里什么情况下会被传递 procedure 进来呢？通常是遇到 eval application 时
;; application 的 operator 会被 eval 成对应的过程体 (lookup variable 获得)
;; 这就是 apply 的 procedure
(define (apply procedure arguments)
  (cond
    [(primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments)]
    [(compound-procedure? procedure)
     (eval-sequence (procedure-body procedure)
                    (extend-environment
                     (procedure-parameters procedure)
                     arguments
                     (procedure-environment procedure)))]
    [else
     error
     "Unknown procedure type -- APPLY"
     procedure]))

;; 用于求解过程应用 Procedure Application 的参数表
;; 以 combinations 的运算对象 operands 为参数，递归的求值并返回这些值的 list
;; (operand exp) -> exps
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 这里使用的是语言无关的 true?, 那么实际上这里的真值就可以是别的，比如 #t
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; eval 传给 apply 的过程体所包含的表达式列表
(define (eval-sequence exps env)
  (cond
    [(last-exp? exps) (eval (first-exp exps) env)]
    [else
     (eval (first-exp exps) env)
     (eval-sequence (rest-exps exps) env)]))

;; 处理赋值和定义
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
