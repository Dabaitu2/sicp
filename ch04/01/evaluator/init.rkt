#lang sicp

(#%require "./special-forms/init.rkt")
(#%require "./derived-forms/init.rkt")
(#%require "./setup.rkt")

(install-special-form-package)
(install-derived-form-package)

(driver-loop)
