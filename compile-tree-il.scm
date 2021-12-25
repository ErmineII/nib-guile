(define-module (language nib compile-tree-il)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (language tree-il)
  #:use-module (language nib parser)
  #:use-module (language nib call)

  #:export (compile-item compile-tree-il yes))

;; lexicals: alist of (name . gensym)
(define (compile-item expr lexicals)
  (match
    expr
    (('sub-expr start-value (('application functions arg-lists) ...))
     (fold (lambda (func args current-value)
             `(call (@ (language nib call) call)
                    ,(compile-item func lexicals)
                    ,current-value
                    ,@(map (lambda (arg) (compile-item arg lexicals)) args)))
           (compile-item start-value lexicals)
           functions
           arg-lists))
    (('assign value name scope)
     (let ((name-gensym (gensym)))
       `(let (,name) (,name-gensym) (,(compile-item value lexicals))
          ,(compile-item scope (acons name name-gensym lexicals)))))
    (('parameters (names ..1) body)
     (let* ((names (cons implicit-value names))
            (gensyms (map (lambda (n) (gensym (symbol->string n))) names)))
       `(letrec*
          ,names
          ,gensyms
          ,(unfold ;; state value: (n . (cdr (cdr ... implicit-value ...)))
             (lambda (x) (= (car x) 0))
             (lambda (x) `(call (@@ (guile) car) ,(cdr x)))
             (lambda (x) (cons (- (car x) 1)
                               `(call (@@ (guile) cdr) ,(cdr x))))
             (cons (length names)
                   (compile-item `(name () ,implicit-value) lexicals)))
          ,(compile-item body (append (map cons names gensyms) lexicals)))))
    (('application-argument function)
     `(call (const ,call)
            ,(compile-item function lexicals)
           ,(compile-item `(name () ,implicit-value) lexicals)))
    (('name (qualifiers ...) name)
     ;; TODO: handle qualifiers
     (let ((the-gensym (assq-ref lexicals name)))
       (if the-gensym
         `(lexical ,name ,the-gensym)
         `(toplevel ,name))))
    (('delay subexpr)
     (if (and (pair? subexpr) (eq? (car subexpr) 'name))
       (compile-item subexpr lexicals)
       (let ((the-gensym (gensym "initial-value")))
         `(lambda ()
            (lambda-case (((,implicit-value) #f #f #f () (,the-gensym))
                          ,(compile-item subexpr (acons implicit-value
                                                        the-gensym
                                                        lexicals))))))))
    ((or (? char?) (? string?) (? number?))
     `(const ,expr))
    (_ (error "invalid parse tree" expr))))

(define (compile-tree-il expr env opts)
  (values (parse-tree-il (compile-item expr '())) env env))
