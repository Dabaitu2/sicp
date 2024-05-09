#lang sicp

(rule (big-shot ?person ?division)
      (and (job ?person (?division ?job))
           ;; 1. 任何人都不是这个人的上级
           ;; (supervisor ?person ?manager) 如果没有匹配, 才会对于 not 返回 true
           (or (not (supervisor ?person ?manager))
               ;; 2. 任何是他上级的人都不在一个部门
               (and (supervisor ?person ?manager)
                    (not (job ?manager (?division .?manager-job)))))))
