#lang sicp

;; a
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))

;; b, 如果要能够实现，则我们的 and 操作符应该也是从左到右执行的
(and (salary (Bitdiddle Ben) ?ben-salary)
     ;; 其他人的名称和工资需要另开变量
     (and (salary ?person ?amount)
          (lisp-value < ?amount ?ben-salary)))

;; c. 依然是从左到右（上到下）计算
;; 1. 获取所有 supervisor 记录中的 ?boss => 找到是老板的那些人
;; 2. 将 boss 传进 第二个条件，找到 ?boss 中符合条件的那些人
;; 3. 找到 job 中第一个选项是 2 中的 boss 的那些，并且获得 他们的 job
(and (supervisor ?person ?boss)
     (not (job ?boss (computer . ?computer-jobs)))
     (job ?boss ?job))
