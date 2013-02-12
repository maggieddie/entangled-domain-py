#! /usr/bin/env racket
#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 1: Lexer for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;
; The lexer program, it can accept a filename
; argument from command line as input file.
; Without command line arguments, it
; reads from STDIN directly.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Import the lexer generators.
(require parser-tools/lex racket/control
         (prefix-in : parser-tools/lex-sre))


(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

; stack operations for inden/dedent
(define (stack-new)
  (list))

(define (stack-size ss)
  (length ss))

(define-syntax stack-push
  (lambda (stx)
    (syntax-case stx ()
      [(stack-push ss e) #'(set! ss (cons e ss))])))


(define-syntax stack-pop
  (lambda (stx)
    (syntax-case stx ()
      [(stack-pop ss) #'                 
                      (let ([tmp (car ss)]) 
                        (set! ss (cdr ss))
                        (car (list tmp)))])))

(define (stack-top ss)
  (when (not (equal? (stack-size ss) 0))
    (car ss))) 
;
(define s0 (stack-new))
(stack-push s0 0)



(define (change-indent len)
  (cond
    [(> len (stack-top s0)) (begin 
                              (stack-push s0 len)
                              (display "(")
                              (display (token-INDENT))
                              (display ")\n"))]
    [(< len (stack-top s0))     
     (let conti ([temp (stack-top s0)]) 
       (when  (not (equal? len temp))  
         (begin 
            (stack-pop s0)
            (if (equal? 0 (stack-size s0))
                (begin
                  (display "(ERROR \"misindented program\")")
                  (abort))
                (begin 
                  (display "(")
                  (display (token-DEDENT))
                  (display ")\n") 
                  (set! temp (stack-top s0))
                  (conti temp))))))]))  


(define token-lst '())

(define (error-handler err-msg)
  (append-token-lst (token-ERROR err-msg))
  (post-proc #f)
  (abort)
  )
; short version for appending
(define (append-token-lst res)
  (set! token-lst (append token-lst (list (cons (token-name res) (token-value res))))) 
  (cons (token-name res) (token-value res)))

(define-tokens value-tokens (KEYWORD PUNCT ID SPACE LIT ERROR))
(define-empty-tokens op-tokens (NEWLINE ENDMARKER INDENT DEDENT CONCATLINE))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (keyword (:or "False" "class" "finally" "is" "return" "None" "continue" "for" "lambda" "try" "True" "def" "from" "nonlocal" "while" "and" "del" "global" "not" "with" "as" "elif" "or" "if" "yield" "assert" "else" "import" "pass" "break" "except" "in" "raise"))
  ;; "." is deleted to deal with object ops
  (delimiter (:or "(" ")" "[" "]" "{" "}" "," ":" "." ";" "@" "=" "+=" "-=" "*=" "//=" "/=" "%=" "&=" "|=" "^=" ">>=" "<<=" "**=" "->" "<>" "..."))
  (operator (:or #\+ #\- #\* "**" "/" "//" "%" "<<" ">>" "&" "|" "^" "~" ">" "<" "<=" ">=" "==" "!=" ))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9"))
  (integer (:or decimalinteger octinteger hexinteger bininteger))
  (decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ "0")))
  (nonzerodigit (:/ "1" "9"))
  (octinteger (:: "0" (:or "o" "O") (:+ octdigit) ))
  (hexinteger (:: "0" (:or "x" "X") (:+ hexdigit) ))
  (bininteger (:: "0" (:or "b" "B") (:+ bindigit) ))
  (octdigit (:/ "0" "7"))
  (hexdigit (:or digit (:/ "a" "f") (:/ "A" "F" )))
  (bindigit (:or "0" "1"))
  
  (floatnumber (:or pointfloat exponentfloat))
  (pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
  (exponentfloat (:: (:or intpart pointfloat) exponent))
  (intpart (:+ digit))
  (fraction (:: "." (:+ digit)))
  (exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit)))
  
  (imagnumber (:: (:or floatnumber intpart) (:or "j" "J")))
  )

(define escaped-chars (list #\\ #\' #\" #\a #\b #\f #\n #\r #\t #\v #\x #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))


(define (process-quote clst pos)
  (if (< pos (length clst))
      (if (and (equal? #\" (list-ref clst pos)) (even? (count-backslash (take clst pos))))
          (string-append (string #\\) (string (list-ref clst pos)) (process-quote clst (add1 pos)))
          (string-append (string (list-ref clst pos)) (process-quote clst (add1 pos))))
      ""))

(define (in-list? e lst)
  (if (empty? lst)
      #f
      (if (equal? (first lst) e)
          #t
          (in-list? e (drop lst 1)))))


(define (process-esc clst pos)
  (if (< pos (length clst))
      (if (and (not (in-list? (list-ref clst pos) escaped-chars)) (odd? (count-backslash (take clst pos))))
          (string-append "\\" (string (list-ref clst pos)) (process-esc clst (add1 pos)))
          (string-append (string (list-ref clst pos)) (process-esc clst (add1 pos))))
      ""))

(define (read-string-syntax str)
  (let* ([qstr (process-quote (string->list str) 0)]
         [cstr (process-esc (string->list qstr) 0)])    
  (define s (read-syntax "l" (open-input-string (string-append "\"" cstr "\""))))
  (eval s ns)))


(define (count-backslash lst)
  (if (> (length lst) 0)
      (if (equal? (last lst) #\\)
          (+ 1 (count-backslash (drop-right lst 1)))
          0)
      0))

(define (has-two-quotes? lst termchar)
  (if (> (length lst) 1)
      (if (equal? (list->string (list termchar termchar)) (list->string (take-right lst 2)))
          (even? (count-backslash (drop-right lst 2)))
          #f)
      #f))


(define (eat-triple-quoted-string inp charlst termchar)
  (begin
    (let ([c (read-char inp)])
      (cond
        [(equal? termchar c) (if (has-two-quotes? charlst termchar)
                                 (drop-right charlst 2)
                                 (eat-triple-quoted-string inp (append charlst (list c)) termchar))]
        [(equal? #\newline c) (if (odd? (count-backslash charlst))
                                  (eat-triple-quoted-string inp (drop-right charlst 1) termchar)
                                  (eat-triple-quoted-string inp (append charlst (list #\\ #\n)) termchar))]
        [(eof-object? c) (error-handler "ERROR: EOF in string!\n")]
        [else (eat-triple-quoted-string inp (append charlst (list c)) termchar)]))))


(define (eat-quoted-string inp termchar raw?)
  (begin
    (let ([c (read-char inp)])
      (if (equal? termchar c)
          (begin
            (let ([c (read-char inp)])
              (if (equal? termchar c)
                  (let ([str (list->string (eat-triple-quoted-string inp (list) termchar))])
                    (if raw?
                        str 
                        (begin
                          (read-string-syntax str))))
                  (let ([pos (file-position inp)])
                    (when (not (eof-object? c))
                      (file-position inp (- pos 1)))
                    ""))))
          (let ([pos (file-position inp)]
                [charlst (list)])
            (when (not (eof-object? c))
              (file-position inp (- pos 1)))
            (let ([str (list->string (aux-eat-quoted-string inp charlst termchar raw?))])
                    (if raw?
                        str 
                        (read-string-syntax str)))
            )))))

(define (aux-eat-quoted-string inp charlst termchar raw?)
  (begin
    (let ([c (read-char inp)])
      (cond
        [(equal? termchar c) (if (odd? (count-backslash charlst))
                                 (aux-eat-quoted-string inp (append charlst (list c)) termchar raw?)
                                 charlst)]
        [(equal? #\newline c) (if (odd? (count-backslash charlst))
                                  (aux-eat-quoted-string inp (drop-right charlst 1) termchar raw?)
                                  (begin
                                    (error-handler "ERROR: newline in quoted string!")))]
        [(eof-object? c) (begin
                                   (error-handler "ERROR: EOF in string!\n"))]
        [else (aux-eat-quoted-string inp (append charlst (list c)) termchar raw?)]))))



(define (eat-comment inp)
  (begin
    (let ([c (read-string 1 inp)])
      (if (eof-object? c)          
          #f
          (if (equal? c "\n")
              #t
              (eat-comment inp))))))

(define (aux-compute-indent-len clst len)
  (if (empty? clst)
      len
      (cond
        [(equal? (first clst) #\space) (aux-compute-indent-len (drop clst 1) (add1 len))]
        [(equal? (first clst) #\tab) (aux-compute-indent-len (drop clst 1) 
                                                             (if (equal? (remainder len 8) 0)
                                                                 (+ len 8)
                                                                 (* 8 (add1 (quotient len 8)))
                                                             ))]
        [else 0])))

(define (compute-indent-length wss)
  (aux-compute-indent-len (string->list wss) 0))

(define (gen-lexer delim?)
  (lexer
   [(eof) (token-ENDMARKER)] 
   
   [(:+ (:or #\space #\tab)) 
    (let ([res (token-SPACE (compute-indent-length lexeme))])
      ;(when (not delim?)
        (if (> (length token-lst) 0)
            (begin 
              (let ([temp (caar (take-right token-lst 1))])
                (when (in-list? temp (list 'CONCATLINE 'NEWLINE))
                  (append-token-lst res)
                  )))
            ( append-token-lst res));)
      (cons 'SPACE (compute-indent-length lexeme)))
    ] 
   
   [(:: (:or (:? (:or "r" "R")) (:or "b" "B" "br" "Br" "bR" "BR")) (:or #\" #\')) 
    (append-token-lst (token-LIT (if (> (string-length lexeme) 1)
                                     (let ([rawprefix (first (take-right (string->list lexeme) 2))])
                                       (if (or (eq? rawprefix #\r) (eq? rawprefix #\R))
                                           (begin
                                             (eat-quoted-string input-port (last (string->list lexeme)) #t))
                                           (begin
                                             (eat-quoted-string input-port (last (string->list lexeme)) #f))))
                                     (eat-quoted-string input-port (last (string->list lexeme)) #f))))]
                                
   
   [(:: (:? "\r") #\newline) 
    (let ([res (token-NEWLINE)])
      ;(when (not delim?)
        (when (> (length token-lst) 0)
          (begin 
            (let ([temp  (caar (take-right token-lst 1))])
              (if (and (not (equal? temp 'CONCATLINE)) (not (equal? temp 'NEWLINE)))
                  (begin
                    (if (< (length token-lst) 2)
                        (let ([temp (take-right token-lst 1)])
                          (when (and (not (equal? (caar temp) 'SPACE)) (not delim?)) 
                            (set! token-lst (append token-lst (list (cons (token-name res) (token-value res)))))))
                        
                        (begin
                          (let ([temp (take-right token-lst 2)])
                            (if (and (equal? (caar temp) 'NEWLINE) (equal? (caadr temp) 'SPACE))
                                (set! token-lst (drop-right token-lst 1))
                                (when (not delim?)
                                           (append-token-lst res)))))
                            
                        ))
                  ;prev is concatline or newline (do nothing)
                  (begin
                    (when (equal? temp 'CONCATLINE)  (set! token-lst (drop-right token-lst 1)))
                    (when (and (equal? temp 'NEWLINE) delim?)
                      (set! token-lst (drop-right token-lst 1)))
                      )))));)
      (cons 'NEWLINE #f))]
   
   [#\\ (begin 
          (let ([res (token-CONCATLINE)])
            ;(when (not delim?)
              (append-token-lst res);)
            ;(cons 'CONCATLINE #f)
            )) ]    
   
   [keyword (begin 
              (let ([res (token-KEYWORD lexeme)])
                (append-token-lst res)                
                ))]
   
   [#\# (begin
          (let ([has-newline (eat-comment input-port)])
            
            (when (> (length token-lst) 0)
              (let ([temp (caar (take-right token-lst 1))])
                (if (equal? temp 'SPACE)
                    (set! token-lst (drop-right token-lst 1))  
                    (when (not delim?) 
                      (when has-newline
                        (when (not (equal? temp 'NEWLINE))
                          (set! token-lst (append token-lst (list  (cons 'NEWLINE #f))))
                          ))))))
            
            (if has-newline
                (cons 'NEWLINE #f)
                'ENDMARKER)))]
   
   [(:or delimiter operator)  (begin 
                                (let ([res (token-PUNCT lexeme)])                                 
                                  (append-token-lst res)
                                  
                                  ))]
  ; "." is added to deal with objct ops 
   [(:: (:or #\_ alphabetic) (:* (:or #\_ alphabetic digit ) ) ) (begin 
                                                                 (let ([res (token-ID lexeme)])
                                                                   (append-token-lst res)
                                                                   ))]
   
   [(:or integer floatnumber) (begin 
                                (match lexeme
                                  [(regexp #rx"0(x|X|o|O|b|B).+") 
                                   (append-token-lst (token-LIT (string->symbol (regexp-replace "0" lexeme "#"))))]
                                  [(regexp #rx"0(0)+") (append-token-lst (token-LIT (string->symbol lexeme)))]
                                  [else (append-token-lst (token-LIT (string->number lexeme)))]))]

   [imagnumber (begin                     
                 (let ([res (token-LIT (string->number (string-append "+" (regexp-replace "j|J" lexeme "i"))))])
                   (append-token-lst res)
                   
                   )) ]
   [any-char (error-handler (string-append "Unknown Character: " lexeme))]
   ))


(define (res-to-string res)
  (cond
    [(pair? res) (cond
                   [(symbol? (cdr res)) (symbol->string (cdr res))]
                   [else (cdr res)]
                   )]
    [else (cond
            [(symbol? (token-value res)) (symbol->string (token-value res))]
            [else (token-value res)])]
    )
  )

(define (build-token-lst lex ip nendmarkers)
  (let ([res (lex ip)]
        [has-endmarkers? (> nendmarkers 0)])
    (if (equal? res 'ENDMARKER)
        (post-proc has-endmarkers?)
        (if (eq? 'PUNCT (car res))              
                (cond
                  [(or (equal? "(" (res-to-string res)) (equal? "[" (res-to-string res)) (equal? "{" (res-to-string res)))
                   (build-token-lst (if has-endmarkers? lex (gen-lexer #t)) ip (+ nendmarkers 1))]
                  [(or (equal? ")" (res-to-string res)) (equal? "]" (res-to-string res)) (equal? "}" (res-to-string res)))
                   (build-token-lst (if (> nendmarkers 1) lex (gen-lexer #f)) ip 
                                    (if (> nendmarkers 0) (sub1 nendmarkers) 0))]
                [else (build-token-lst lex ip nendmarkers)])
                (build-token-lst lex ip nendmarkers)))))

(define (show-sexp s)
  (display "(")
  (display (car s))
  (when (not (equal? (car s) 'NEWLINE))      
        (display " ")
    (cond 
      [(or (equal? (car s) 'KEYWORD)
           (and (equal? (car s) 'LIT) (symbol? (cdr s)) ))
           (display (cdr s))]
      [else (print (cdr s))]))          
  (display ")")
  (newline))

(define (post-proc has-delim?)
  (begin              
    (set! token-lst (append token-lst (list 'ENDMARKER)))           
    (let out ([temp (car token-lst)] [cnt (length token-lst)])
      (if (not (equal? temp 'ENDMARKER))                 
          (begin                 
            (if (not (equal? (car temp) 'SPACE)) 
                (begin
                   (if (equal? (car temp) 'CONCATLINE)
                      (begin
                        (display "(ERROR \"Concated line symbol is not the last char in a line\")\n")
                        (abort))
                      (begin
                        (when (> (length token-lst) cnt)
                          (when (equal? 'NEWLINE (caar (take-right token-lst (+ cnt 1))))
                            (change-indent 0)))
                          (show-sexp temp))))
                (begin                   
                  (change-indent (cdr temp))))
            (set! cnt (sub1 cnt))
            (set! temp (car (take-right token-lst cnt)))
            (out temp cnt))
          (when (not has-delim?)
            (change-indent 0))))  
    (display "(ENDMARKER)\n")))


;;; for adding defined globals
(define (read-ip ip)
  (let ([next (read-string 10000 ip)])
    (if (eof-object? next)
        '()
        (cons next (read-ip ip)))))


(define (add-defined-globals ip)
  (string-append  "__name__ = \"__main__\" \n" (car (read-ip ip))))   


;;;;;;;;;;;;

(define args (current-command-line-arguments))
(define in (current-input-port))
(when (> (vector-length args) 0)
    (set! in (open-input-file (vector-ref args 0) #:mode 'binary)))

;(set! in (open-input-string (add-defined-globals in)))

(port-count-lines! in)
(build-token-lst (gen-lexer #f) in 0) 
(close-input-port in) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 1: Lexer for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
