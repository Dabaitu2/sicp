#lang sicp

;; actually it's practicing your skill to write recursive code
(rule (last-pair (?x) (?x)))
(rule (last-pair (?y . ?z) (?x))
      (last-pair ?z (?x)))
