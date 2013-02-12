#lang racket

(require srfi/41) ; Stream library
(require test-engine/racket-tests)
(require racket/pretty)

(require "derivative-parsers.rkt")


;;;; Python grammar

(define (mk-tag? tag-name)
  (lambda (token)
    (eq? (car token) tag-name)))

(define (mk-value? predicate)
  (lambda (token)
    (predicate (cadr token))))

(define NAME      (lang (@--> (token (mk-tag? 'ID) 'NAME)
                              (λ (_ id) (string->symbol id)))))
(define NUMBER    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? number?)) 'NUMBER)
                              (λ (_ num) num))))
(define STRING    (lang (@--> (token (and/c (mk-tag? 'LIT) (mk-value? string?)) 'STRING)
                              (λ (_ str) str))))

(define NEWLINE   (token (mk-tag? 'NEWLINE) 'NEWLINE))
(define ENDMARKER (token (mk-tag? 'ENDMARKER) 'ENDMARKER))
(define INDENT    (token (mk-tag? 'INDENT) 'INDENT))
(define DEDENT    (token (mk-tag? 'DEDENT) 'DEDENT))


(define python-literal->language
 (λ (x)
   (match x
     [(or "+" "-" "*" "**" "/" "//" "%"
          "<<" ">>" "&" "|" "^" "~"
          "<" ">" "<=" ">=" "==" "!="
          "(" ")" "[" "]" "{" "}"
          "," ":" "." ";" "@" "="
          "+=" "-=" "*=" "/=" "//=" "%="
          "&=" "|=" "^=" ">>=" "<<=" "**="
          "..." ; not in lexical spec
          "<>" ; not in lexical spec
          "->" ; not in lexical spec
          )
      (lang (>--> (token (and/c (mk-tag? 'PUNCT) (mk-value? (λ (v) (equal? v x)))) x)
                  [`(PUNCT ,k) k]))]
     
     [(or "False" "None" "True" "and" "as" "assert" "break"
          "class" "continue" "def" "del" "elif" "else" "except" 
          "finally" "for" "from" "global" "if" "import" "in"
          "is" "lambda" "nonlocal" "not" "or" "pass" "raise" 
          "return" "try" "while" "with" "yield")
      (lang (>--> (token (and/c (mk-tag? 'KEYWORD) (mk-value? (λ (v) (equal? x (symbol->string v))))) x)
                  [`(KEYWORD ,k) k]))]
     
     [else
      (printf "unknown literal: ~s~n" x)
      (error "unknown literal")])))

(set-literal->language! python-literal->language)
 
(define py-lang
  (grammar-from-file file_input "python-ast.grm.sx"))

(define args (current-command-line-arguments))

(define (python-parse-file filename)
  (python-parse-port (open-input-file filename))) 

(define (python-parse-port port)
  (define lines (read-all port))
  (define forest (parse #:compact compact py-lang (list->stream lines)))
  (cond
    [(= (set-size forest) 1)
     (pretty-write (set-choose forest))
     (newline)]
    
    [(= (set-size forest) 0)
     (pretty-write #f) 
     (newline)]
    
    [else
     (printf "; ambiguous grammar:~n"
     (write forest)
     (newline))]))
 

;;; newly added to transform the class to funtion and always return the record;
;
;(define (class-to-func parsed-res)
;  (match parsed-res
;      [`(program) `(program) ]
;      [`(program ,stmts ...) (match-stmt-onebyone stmts) 
;                                     ])
;      [else (display "FAILED match-parsed result. \n")])
;
;(define (match-stmt-onebyone stmts)
;  (if (= 0 (length stmts))
;      (list)
;      (let* ((s (first stmts))
;             (s-res (match-stmt s indef symtbl)))
;        (if (and (list? s) (equal? 'begin (first s)))
;            (append s-res (match-stmt-onebyone (drop stmts 1) ))
;            (append (list s-res) (match-stmt-onebyone (drop stmts 1) ))))))
;
;(define (match-stmt s)
;  (match s
;     ; <funcdef> ::= (def (<NAME> <NAME>*) <suite>)
;    ;(funcdef         (seq "def" NAME parameters ":" suite))
;    
;    ;<classdef> ::= (class <NAME> ([ <arglist> ]) : <suite>)
;
;;(classdef        (seq "class" NAME inherit ":" suite)
;;(inherit         (seq "(" (opt arglist) ")")
;    
;    [`(class ,names ,suite) ]))
;
;(define (extract s)
  
      
    
    

; (python-parse-file "lexed.py") 
(match args
  [(vector filename)
   ; =>
   (python-parse-file filename) 
    
   (exit)]
  
  [else
   ; =>
   (python-parse-port (current-input-port))])
