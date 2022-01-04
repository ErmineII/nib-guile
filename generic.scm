(define-module (language nib generic)
  #:use-module (oop goops)
  #:export (call))

(define-generic call)
(define-method (call (fn <procedure>) args)
  (fn args))
