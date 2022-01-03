(define-module (language nib call)
  #:export (call))

(define (call fn args)
  (cons* 'call fn args))
