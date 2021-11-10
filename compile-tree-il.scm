(define-module (language nib compile-tree-il)
  #:use-module (ice-9 match)
  #:use-module (language tree-il)
  #:export (compile-tree-il))

;; lexicals: alist of (name . gensym)
(define (compile-expr expr lexicals)
  (match
    expr
    (_ (error (list "invalid parse tree" expr)))
    ))

(define (compile-tree-il expr env opts)
  (parse-tree-il (compile-expr expr '())))
