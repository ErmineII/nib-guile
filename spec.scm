(define-module (language nib spec)
  #:use-module (language nib compile-tree-il)
  #:use-module (language nib lexer)
  #:use-module (language nib parser)
  #:use-module (system base language)
  #:export (nib))

(define-language nib
  #:title	"Nib"
  #:reader (lambda (port env) (lex-nib port))
  #:parser parse-nib

  ;; for development
  #:evaluator	(lambda (x module) x)

  ; #:compilers   `((tree-il . ,compile-tree-il))

  #:printer	write)
