#lang racket

;; helper function for readable gensym
 (define gensym
    (let ([counter 0])
      (lambda ([x 'b])
        (if (number? x)
            (set! counter x)
            (begin0 (string->unreadable-symbol
                     (format "~a~a" x counter))
                    (set! counter (add1 counter)))))))
  
  
;(define (atomic? exp)
;  (match exp
;    [`(quote ,_)         #t]
;    [(? number?)         #t]
;    [(? boolean?)        #t]
;    [(? string?)         #t]
;    [(? char?)           #t]
;    [(? symbol?)         #t]
;    [(or '+ '- '* '/ '=) #t]
;    [else                #f]))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [(? symbol?)   #t]
    
    ['(void)       #t]
    ['None         #t]
    ['Ellipsis     #t]
    ;[(or '+ '- '* '/ '= ) #t] ; not sure about this line
    [`(list v ...) #t]
;    [`(dict (k v) ...) 
;     
;     (displayln "the dict is an atomic!") 
;     #t]
    ;[(quasiquote ((unquote _) ... ))    #t]
    ;[(()   (display "stupid atomic???(): ") (displayln exp) #t]
   ; [`()   #t]
    [else          #f]))

; primitive-operations is a set of symbols
(define primitive-operations
  (apply set '(for-container ;get-field set-field! remove-field! 
                             equal? py-list-set! dict-set! < >  >= <= not-equal? in? not-in? eq? not-eq? >> << + - * / quotient modulo expt assert2 bitwise-and bitwise-or bitwise-xor py-list-ref py-list-remove! tuple-ref  dict-ref dict-set! dict-remove! bitwise-not integer? string? tuple? dict? py-list? set? assert1 py-print)))




; primitive-operation? : symbol -> boolean
(define (primitive-operation? prim-op)
  (set-member? primitive-operations prim-op))


;; Expression normalization:
(define (normalize-term exp) (normalize exp (lambda (x) x)))

(define (normalize exp k)
  (match exp
    [`(lambda ,params ,body)  
     
      (k `(lambda ,params ,(normalize-term body)))]
    
    [`(let () ,exp)
      (normalize exp k)]

    [`(let ([,x ,exp1] . ,clause) ,exp2) 
      (normalize exp1 (lambda (aexp1) 
       `(let ([,x ,aexp1])
         ,(normalize `(let (,@clause) ,exp2) k))))]
    
    [`(begin ,exp) 
      (define tmp (gensym))
      (normalize exp (lambda (t)
                       `(let ((,tmp ,t))
                          ,(k tmp))))]
     ; (k (normalize-term exp ))]
    
     [`(begin ,exp ,rest ...)
     
     (normalize exp (lambda (t)
                  `(let ((,(gensym '_) ,t))
                         ,(normalize `(begin ,@rest) k))))
     
     ]
    
    [`(set! ,v ,exp)
      (normalize-name exp (lambda (t)
       `(let ([,(gensym '_) (set! ,v ,t)])
          ,(k '(void)))))]
    
    [`(if ,exp1 ,exp2 ,exp3) 
    
      (normalize-name exp1 (lambda (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3)))))]
    
    
    
    [`(,(and p (? primitive-operation?)) ,es ...) 
     ; =>
     
     (match p
       [(or `py-list-set! `dict-set!)
        (normalize-name* es (lambda ($es)
                              `(let ([,(gensym '_) ((anf ,p) ,@$es)])
                                 ,(k '(void)))))
        
        ]
       [else
     
        (normalize-name* es (lambda ($es)
                           ; (k  `(,p ,@$es))))]
                           (k `((anf ,p) ,@$es))))]
       )]
    
    [`(dict (,k* ,v*) ...) 
     (normalize-name* k* (lambda (key*)
                                  (normalize-name* v* (lambda (vals*)
                                                        (k `(dict ,@(map (lambda (x y) (list x y)) key* vals*)))))))]
 
    ;; left from the desugaring phrase while expecting to be dealt with 
    ;; in the CESK machine! 
    
    [`(call/ec ,exp)
      (k `(call/ec ,(normalize-term exp)))]
    
    
   ; |   (<triop> <exp> <exp> <exp>)
    ;|   (<binop> <exp> <exp>)
    ;|   (<unop> <exp>)
    
    
  
    ;; 
    
    [(? atomic?)            
     (k exp)]
    
    [`(,f . ,e*) 
      (normalize-name f (lambda (t) 
                          (normalize-name* e* (lambda (t*)
                                                (k `(,t . ,t*))))))]
    
    
    
    
    ))

(define (normalize-name exp k)
  (normalize exp (lambda (aexp) 
    (if (atomic? aexp)
       ;(begin
        ; (displayln aexp)
          (k aexp) ;)
        (let ([t (gensym)])
         
         `(let ([,t ,aexp]) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (lambda (t) 
       (normalize-name* (cdr exp*) (lambda (t*) 
        (k `(,t . ,t*))))))))


;;;;; for the top! ;;;;;
; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))
;
(define (anormal-program prog)
  (match  (car prog)
    [`(program ,stmts ...)
     (define-values (defs bodys) (partition define? stmts))
     `(program ,@defs ,@(normalize-program bodys))
     ]
    
    
    )) 

(define (normalize-program decs)
    
   (match decs
    ['() 
     '()]
    
    [(cons `(define . ,vs) rest)
     (cons `(define . ,vs);(normalize-define (car decs))
           (normalize-program rest))]
    
    [(cons exp rest)
     (cons (normalize-term exp)
           (normalize-program rest))]))

;;; test
;(displayln
; (pretty-format
;(anormal-program 
; `{ (program
;   (define f (void))
;   (define lst (void))
;   (set! lst `(1 2 3))
;   (set! f 
;           (lambda (n)
;             (if (= n 0)
;                 1 
;                 (* n (f (- n 1)))))))
;     })))


(define (read-all port)
    (let ((next (read port)))
      (if (eof-object? next)
          '()
          (cons next (read-all port)))))
  
  (define (pyanormal-file filename)
    (py-port (open-input-file filename)))
  
  (define (py-port port)
    (let* ([lines (read-all port)])
           (when (not (empty? lines))
             ;(pretty-write  lines)
             (pretty-write (anormal-program  lines)))))
  
  (define args (current-command-line-arguments))
  (match args
    [(vector filename)   (pyanormal-file filename) (exit)]  
    [else  (py-port (current-input-port))])
  
 ;(pyanormal-file "test-list-dict.py.desug")
; (pyanormal-file "./tests/tt8.py.desug")
