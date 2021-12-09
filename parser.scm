(define-module (language nib parser)
  #:use-module (language nib lexer)
  #:use-module (system base lalr)
  #:export (parse-nib implicit-value))

(define implicit-value (make-symbol "implicit-value"))

(define (parse-nib tokens)
  (let ((parser (make-parser)))
    (parser (lambda ()  ;; lexical analyzer
              (if (null? tokens)
                  '*eoi*
                  (let ((tok (car tokens)))
                    (set! tokens (cdr tokens))
                    (if (not (pair? tok))
                        (make-lexical-token tok #f #f)
                        (transform-lexical-token tok)))))
            error)))

(define (transform-lexical-token token)
  (make-lexical-token (car token) #f  (case (car token)
    ((name-token) (string->symbol (cadr token)))
    ((character) (string-ref (cadr token) 0))
    ((string-token) (cadr token))
    ;; Isn't it so nice that guile supports all the number syntax that nib does?
    ((number) (string->number (cadr token))))))

(define (make-parser)
  (lalr-parser
   ;; terminal token types
   (open-paren close-paren name-token assign double-colon colon character dot
               string-token number)
   (expression (parameters assignments subexpression)
               : `(expr ,$1 ,$2 ,$3))
   (parameters (colon name-token parameters) : (cons $2 $3)
               () : '())
   (assignments (assignments assignment) : (cons $2 $1)
                () : '())
   (assignment (subexpression assign name-token) : (list $1 $3))
   (subexpression (value applications) : `(sub-expr ,$1 ,$2)
                  (applications) : `(sub-expr (delay (name () ,implicit-value)) ,$1))
   (applications (application applications) : (cons $1 $2)
                 () : '())
   (application (function arguments) : `(application ,$1 ,$2))
   (function (parenthesis) : $1
             (name) : $1)
   (parenthesis (open-paren expression close-paren) : $2)
   (arguments (argument arguments) : (cons $1 $2)
              () : '())
   (argument (colon function) : `(application $2 ())
             (colon value) : $2
             (value) : $1)
   (name (qualifiers name-token) : `(name ,$1 ,$2))
   (qualifiers (qualifiers name-token double-colon) : (cons $2 $1)
               () : '())
   (value (number) : $1
          (string-token) : $1
          (dot function) : `(delay ,$2)
          (character) : $1)))
