(define-module (language nib parser)
  #:use-module (language nib lexer)
  #:use-module (system base lalr)
  #:export (parse-nib))

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
   ; (expect: 5) ;; I don't know what the best way to resolve these Shift/Reduce
   ;             ;; conflicts is
   ;; terminal token types
   (open-paren close-paren name-token assign double-colon colon character dot
               string-token number semicolon)
   (expression (subexpression) : $1
               (assignments subexpression) : `(assign ,$1 ,$2))
   (assignments (subexpression assign target semicolon assignments) : (cons (list $1 $3 #t) $5)
                (subexpression assign target assignments) : (cons (list $1 $3 #f) $4)
                (subexpression assign name) : (list (cons $1 $3)))
   (subexpression (value applications) : `(expr ,$1 ,$2)
                  (applications) : `(expr #f ,$1))
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
