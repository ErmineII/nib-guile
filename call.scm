(define-module (language nib call)
  #:export (call))

(define (call fn . args)
  (list 'call fn args))
