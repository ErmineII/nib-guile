(define-module (language nib io-state)
  #:use-module (srfi srfi-9)
  #:export (io-state? io-state next-io-state with-state-change))

(define-record-type <io-state>
  (make-io-state number)
  io-state?
  (number io-state-number))

(define initial-io-state (make-io-state 0))
(define (next-io-state s) (make-io-state (1+ (io-state-number s))))

(define io-state (make-parameter initial-io-state))

(define-syntax-rule (with-state-change prior body ...)
  (if (eqv? prior (io-state))
    (let ((result (begin body ...)))
      (io-state (next-io-state (io-state)))
      (values (io-state) result))
    (error "Tried to use an old version of the world")))
