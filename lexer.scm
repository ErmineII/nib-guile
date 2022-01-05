(define-module (language nib lexer)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:export (lex-nib lex-nib-string))

(define-peg-pattern ws none (peg "([ \t\n] / '`' (!'`' .)* '`')*"))

(define-peg-pattern digit body (range #\0 #\9))
(define-peg-pattern integer body (and (? "-") (+ digit)))
(define-peg-pattern fraction body (and "." (+ digit)))
(define-peg-pattern exponent body (and (or "E" "e") (? (or "-" "+")) (+ digit)))
(define-peg-pattern number all (and integer (? fraction) (? exponent)))

(define-peg-pattern name-token all (peg "ws !number (![:()$`\". \t\n] .)+"))
(define-peg-pattern double-colon all (ignore "::"))

(define-peg-pattern ign-quote none "\"")
(define-peg-pattern escaped-char body (peg "!'\"' . / '\"' ign-quote"))
(define-peg-pattern string-token all
                    (and ign-quote (* escaped-char) ign-quote))
(define-peg-pattern character all
                    (and (ignore ".") ws ign-quote escaped-char ign-quote))

(define-peg-pattern dot all (ignore "."))
(define-peg-pattern open-paren all (ignore "("))
(define-peg-pattern close-paren all (ignore ")"))
(define-peg-pattern dollar-sign all (ignore "$"))
(define-peg-pattern colon all (ignore ":"))
(define-peg-pattern assign all (ignore (and dot colon)))

(define-peg-pattern token body
                    (or open-paren
                        close-paren
                        name-token
                        assign
                        double-colon
                        colon
                        character
                        dot
                        string-token
                        number
                        dollar-sign))
(define-peg-pattern tokens all (* (and ws token)))

(define (fix tree)
  (let ((flat
          (keyword-flatten '(name-token character string-token number) tree)))
    (if (pair? flat)
      (cdr flat)
      '())))

(define (lex-nib-string s)
  (fix (peg:tree (match-pattern tokens s))))

(define (lex-nib port)
  (let ((contents (get-string-all port)))
    (if (string-null? contents)
      (read-char port) ;; eof-object
      (lex-nib-string contents))))
