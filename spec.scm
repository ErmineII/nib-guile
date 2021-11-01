(define-module (language nib spec)
  ; #:use-module (language nib compile-tree-il)
  #:use-module (language nib parser)
  #:use-module (system base language)
  #:export (nib))

(define-language nib
  #:title	"Nib"
  #:reader (lambda (port env) (read-nib port))
  ; #:compilers   `((tree-il . ,compile-tree-il))
  #:evaluator	(lambda (x module) x)
  #:printer	write)
