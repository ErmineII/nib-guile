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
                        (make-lexical-token (car tok) #f (cadr tok))))))
            error)))

(define (make-parser)
  (lalr-parser
   ;; terminal token types
   (open-paren close-paren name-token assign double-colon colon character dot
               string-token number semicolon)
   (expression (assignments subexpression) : (if (null? $1)
                                               $2
                                               `(assign ,(reverse $1) ,$2)))
   (assignments (assignments assignment) : (cons $2 $1)
                () : '())
   (assignment (subexpression assign target maybe-semicolon) : (list $1 $3 $4))
   (maybe-semicolon (semicolon) : #t () : #f)
   (target (name-token colon target) : (cons (string->symbol $1) $3)
           (name-token) : (list (string->symbol $1)))
   (subexpression (value applications) : `(expr ,$1 ,$2)
                  (applications) : `(expr (delay (name () ,implicit-value)) ,$1))
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
   (name (qualifiers name-token) : `(name ,$1 ,(string->symbol $2)))
   (qualifiers (qualifiers name-token double-colon) : (cons (string->symbol $2) $1)
               () : '())
   (value (number) : (string->number $1)
          (string-token) : $1
          (dot function) : `(delay ,$2)
          (character) : (string-ref $1 0))))
