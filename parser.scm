(define-module (language nib parser)
  #:use-module (language nib lexer)
  #:use-module (language nib implicit-value)
  #:use-module (system base lalr)
  #:export (parse-nib))

(define implicit-value (gensym " "))

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
   ;; conflict caused by "$ expression"
   (expect: 1)
   ;; terminal token types
   (open-paren close-paren name-token assign double-colon colon character dot
    string-token number dollar-sign)
   (expression
     (double-colon names) : `(module ,$2)
     (parameters scope) : (if (null? $1) $2 `(parameters ,$1 ,$2)))
   (names
     (name colon names) : (cons $1 $3)
     (name) : (list $1))
   (parameters
     (colon name-token parameters) : (cons $2 $3)
     () : '())
   (scope
     (subexpression assign name-token scope) : `(assign ,$1 ,$3 ,$4)
     (subexpression) : $1)
   (subexpression
     (value applications) : `(sub-expr ,$1 ,$2)
     (applications) : `(sub-expr (delay (name (,implicit-value))) ,$1))
   (applications
     (application applications) : (cons $1 $2)
     () : '())
   (application
     (function arguments) : `(application ,$1 ,$2))
   (function
     (open-paren expression close-paren) : $2
     (dollar-sign expression) : $2
     (name) : $1)
   (arguments
     (argument arguments) : (cons $1 $2)
     () : '())
   (argument
     (colon function) : `(sub-expr (delay (name (,implicit-value)))
                                   ((application ,$2 ())))
     (colon value) : $2
     (value) : $1)
   (name
     (path) : `(name ,$1))
   (path
     (name-token double-colon path) : (cons $1 $3)
     (name-token) : (list $1))
   (value
     (number) : $1
     (string-token) : $1
     (dot function) : `(delay ,$2)
     (character) : $1)))
