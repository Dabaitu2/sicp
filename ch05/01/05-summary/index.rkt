#lang sicp


(assign ⟨register-name⟩ (reg ⟨register-name⟩)) 
(assign ⟨register-name⟩ (const ⟨constant-value⟩)) 
(assign ⟨register-name⟩ 
        (op ⟨operation-name⟩)
        ⟨input1⟩ ... ⟨inputn⟩)
(perform (op ⟨operation-name⟩) ⟨input1⟩ ... ⟨inputn⟩) 
(test (op ⟨operation-name⟩) ⟨input1⟩ ... ⟨inputn⟩) 
(branch (label ⟨label-name⟩))
(goto (label ⟨label-name⟩))

;; registers can hold labels 
(assign ⟨register-name⟩ (label ⟨label-name⟩))
(goto (reg ⟨register-name⟩))

;; stack operations
(save ⟨register-name⟩) 
(restore ⟨register-name⟩)
