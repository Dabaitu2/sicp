#lang sicp

(#%require "./special-forms.rkt")
(#%require "./derived-forms.rkt")
(#%require "./setup.rkt")

(install-special-form-package)
(install-derived-form-package)

(driver-loop)
