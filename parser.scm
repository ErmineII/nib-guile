(define-module (language nib parser)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:export (read-nib))

(define-peg-pattern ws none (peg "([ \t\n] / '`' (!'`' .)* '`')*"))

(define-peg-pattern digit body (range #\0 #\9))
(define-peg-pattern integer body (and (? "-") (+ digit)))
(define-peg-pattern fraction body (and "." (+ digit)))
(define-peg-pattern exponent body (and (or "E" "e") (? (or "-" "+")) (+ digit)))
(define-peg-pattern number all (and ws integer (? fraction) (? exponent)))

(define-peg-pattern name-token body (peg "ws !number (![:;()`\". \t\n] .)+"))
(define-peg-pattern qualifier all (and name-token ws (ignore "::")))
(define-peg-pattern name all (and (* qualifier) name-token))

(define-peg-pattern ign-quote none "\"")
(define-peg-pattern escaped-char body (peg "!'\"' . / '\"' ign-quote"))
(define-peg-pattern string-token all
                    (and ws ign-quote (* escaped-char) ign-quote))

(define-peg-pattern dot none (and ws "."))
(define-peg-pattern character all (and dot ws ign-quote escaped-char ign-quote))

(define-peg-pattern delayed all (and dot function))
(define-peg-pattern value body (or number string-token character delayed))

(define-peg-pattern function all (or parenthesis name))
(define-peg-pattern parenthesis all
                    (and ws (ignore "(") (or expression array) ws (ignore ")")))
(define-peg-pattern array all (+ (and expression ws ";")))

(define-peg-pattern argument body
                    (or (and ws (ignore ":") (or function value)) value))
(define-peg-pattern application all (and function (* argument)))
(define-peg-pattern subexpression body (and (? value) (* application)))
(define-peg-pattern assignment all (and subexpression ws (ignore ".:") name))
(define-peg-pattern expression all (and (* assignment) subexpression))

(define (fix tree)
  (match
    tree
    ;; TODO: add more cases when encountered
    (_ tree)))

(define (read-nib port)
  (let* ((contents (get-string-all port))
         (code (if (eof-object? contents) "" contents))
         (matched (peg:tree (match-pattern expression code)))
         (fixed (fix matched)))
    fixed))
