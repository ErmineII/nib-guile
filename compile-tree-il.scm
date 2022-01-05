(define-module (language nib compile-tree-il)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)

  #:use-module (language tree-il)
  #:use-module (language nib implicit-value)
  #:use-module (language nib generic)

  #:export (compile-item compile-tree-il yes))

;; lexicals: alist of (name . gensym)
(define (compile-item expr lexicals)
  (match
    expr
    (('sub-expr start-value (('application functions arg-lists) ...))
     (fold
       (lambda (func args current-value)
         (let ((compiled-args
                  (map (lambda (arg) (compile-item arg lexicals)) args)))
           (if (eq? (car func) 'name)
             `(call (@ (language nib generic) call)
                    ,(compile-item func lexicals)
                    (call (@ (guile) cons*)
                          ,current-value
                          ,@compiled-args))
             (let ((the-gensym (gensym " ")))
               `(let (,implicit-value) (,the-gensym)
                     ((call (@ (guile) cons*) ,current-value ,@compiled-args))
                     ,(compile-item func (acons implicit-value
                                                the-gensym
                                                lexicals)))))))
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
             (lambda (x)
               (if (= (car x) 1)
                 (cdr x)
                 `(call (@@ (guile) car) ,(cdr x))))
             (lambda (x) (cons (- (car x) 1)
                               `(call (@ (guile) cdr) ,(cdr x))))
             (cons (length names)
                   (compile-item `(name (,implicit-value)) lexicals)))
          ,(compile-item body (append (map cons names gensyms) lexicals)))))
    (('name (variable fields ...))
     (fold
       (lambda (field module)
         `(call (@ (guile) module-ref)
                ,module
                (const ,field)))
       (let ((the-gensym (assq-ref lexicals variable)))
         (if the-gensym
           `(lexical ,variable ,the-gensym)
           `(toplevel ,variable)))
       fields))
    (('delay subexpr)
     (if (and (pair? subexpr) (eq? (car subexpr) 'name))
       (compile-item subexpr lexicals)
       (let ((the-gensym (gensym " ")))
         `(lambda ()
            (lambda-case (((,implicit-value) #f #f #f () (,the-gensym))
                          ,(compile-item subexpr (acons implicit-value
                                                        the-gensym
                                                        lexicals))))))))
    (('module (names ...))
     (let ((gensym-gensym (gensym))
           (module-gensym (gensym)))
     `(let (the-gensym) (,gensym-gensym) ((call (@ (guile) gensym)))
        (let (the-module) (,module-gensym)
             ((call (@ (guile) resolve-module)
                    (call (@ (guile) list)
                          (const language)
                          (const nib)
                          (lexical the-gensym ,gensym-gensym))))
          ,(fold
             (lambda (name code) 
               `(seq
                 (call (@ (guile) module-define!)
                       (lexical the-module ,module-gensym)
                       (const ,(caddr name))
                       ,(compile-item name lexicals))
                 ,code))
            `(lexical the-module ,module-gensym)
            names)))))
    ((or (? char?) (? string?) (? number?))
     `(const ,expr))
    (_ (error "invalid parse tree" expr))))

(define (with-prelude body)
  `(seq
    (call (@ (guile) set-current-module)
      (call (@ (guile) resolve-module)
        (const (language nib modules default))))
    (seq
      (define ,implicit-value
        (call (@ (language nib io-state) io-state)))
      ,body)))

(define (compile-tree-il expr env opts)
  (values (parse-tree-il (with-prelude (compile-item expr '()))) env env))
