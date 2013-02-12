#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 4: Desugaring python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition-k pred (cdr lst) (λ (in out)
                                    (if (pred (car lst))
                                        (k (cons (car lst) in) out)
                                        (k in (cons (car lst) out)))))))

; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [`(,(or 'lambda 'λ) . ,_)     #t]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(,_ ...) #t]
    ['(void)       #t]
    ['None         #t]
    ['Ellipsis     #t]
    [(? symbol?)         #t]
    [else          #f]))

; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))

; global-name : symbol -> symbol

(define built-in-container-type '(List set tuple object gbl-dict len))
(define (built-in-container-type? cont) 
  (member  cont built-in-container-type))
(define (global-name name)
  (if (built-in-container-type? name)
      name
      (begin
     ; (displayln name)
      (string->symbol (string-append "g$" (symbol->string name))))))

; atomize-tops : top list -> top list
(define (atomize-tops tops)
  (match tops
    ['()  #;=>  '()]
    
    [(cons (and head (? atomic-define?)) tail)
     (cons head (atomize-tops tail))]
    
    [(cons `(define ,v ,exp) tail)
     `((define ,v (void))
       (set! ,v ,exp)
       ,@(atomize-tops tail))]
    
    [(cons head tail)
     (cons head (atomize-tops tail))]))




;; Desugaring.

; desugar-top : top -> top
(define (desugar-top top)
  (match top
    [`(define ,v ,exp)
     `(define ,(global-name v) ,(desugar-exp exp))] 
    
    [exp
     (desugar-exp exp)]))


; desugar : program -> program
(define (desugar-program program)
  (define prog 
    (match program [`(program . ,stmts) stmts]))
  ;(pretty-write prog)
  
  (set! prog (atomize-tops prog))
  
  (set! prog (map desugar-top prog))
  
  (set! prog (append (list
                      '(define break (void))
                      '(define return (void))
                      '(define continue (void))
                      '(define $current-handler (void)))
                     prog))
 ; (display "before the partition.....\n")
  ;(pretty-write prog)
  (set! prog
        (partition-k 
         atomic-define?
         prog
         (λ (atomic complex)
           (append atomic `((begin ,@complex)))))) 
  ;(display "After the partition.....\n")
  ;(pretty-write prog)
  
  `(program ,@prog))

(define (gen-for-each-helper $seq $loop else )
  (desugar-exp 
   `(lambda (,$seq  ,$loop)
      (begin
        (begin
;          (cond
;            ((set? ,$seq)     (for-set ,$seq ,$loop))
;            ((tuple? ,$seq)   (for-tuple ,$seq ,$loop))
;            ((py-list? ,$seq) (for-py-list ,$seq ,$loop))
;            ((dict? ,$seq)    (for-dict ,$seq ,$loop))) 
          (for-container ,$seq ,$loop)
              
           ,else
          )
       ))))     

  
  (define (gen-for-each var exp body else) 
    (define $seq (gensym '$seq))
    (define $loop (gensym '$loop))   
    `(call/ec
      (lambda (break)
          (,(gen-for-each-helper $seq $loop else)
           ,(desugar-exp exp)        
           (lambda (,var)
             (call/ec
              (lambda (continue)
               ,(desugar-exp body)))))
        )))
  
  
  (define (gen-while2 cond body else)
    `(call/ec (lambda (break)
                ,(desugar-exp `(letrec 
                                   ((loop (lambda ()                                 
                                            (if ,(desugar-exp cond)
                                                (begin
                                                  (call/ec (lambda (continue)
                                                             ,(desugar-exp body)))
                                                  (loop))
                                                (void)))))                  
                                 (loop)
                                 ,(desugar-exp else))))))
  
   
  ; desugar-exp : exp -> exp
  (define (desugar-exp exp)
    (match exp
      [(? symbol?)      exp]
      ;[`(quote ,_)       (error "quotes not allowed in hir")] 
      
      [`(letrec ((,vs ,es) ...) . ,body) 
       (desugar-exp
        `(let ,(for/list ([v vs])
                 (list v '(void)))
           ,@(map (λ (v e)
                    `(set! ,v ,e))
                  vs es)
           ,@body))]
      
       [`(let () . ,body)
        (desugar-exp `(begin ,@body) )]
      
;      
;       [`(let ((,vs ,es) ...) . ,body)
;       `((lambda ,vs ,(desugar-exp `(begin ,@body)))
;         ,@(map desugar-exp es))
;       ] 
      
       [`(let ((,vs ,es) ...) . ,body)
        (let* ((vs-lst vs)
               (es-lst  (map desugar-exp es))
               (rest-vs (drop vs 1))
               (rest-es (drop es 1))
               (rest-pairs (map (λ (x y) (list x y)) rest-vs rest-es))
               )
          `(let ((,(car vs-lst) ,(car es-lst)))
             ,(desugar-exp 
               `(let ,rest-pairs . ,body))))]
      
      [`(let* () ,body)
      (desugar-exp body) ] 
    
      [`(let* ((,v ,e) . ,rest) ,body)
       (desugar-exp 
        `(let (,(list v (desugar-exp e)))
                     ,(desugar-exp 
                       `(let* ,rest ,body))))]
      
      
      [`(,(or 'lambda 'λ) ,params ,body)
        `(lambda ,params ,(desugar-exp body))]
      
      [`(,(or 'lambda 'λ) ,params . ,body)
        `(lambda ,params ,(desugar-exp `(begin ,@body)))] 
      
      [`(call/ec ,exp)
       `(call/ec ,(desugar-exp exp))]
      
      [`(cond)  
       `(void)]
      
      [`(cond (else ,exp)) 
       (desugar-exp exp)]
      
      [`(cond (,test ,exp))        
       `(if ,(desugar-exp test) 
            ,(desugar-exp exp) 
            (void))]
      
      [`(cond (,test ,exp) ,rest ...)      
       `(if ,(desugar-exp test)
            ,(desugar-exp exp)
            ,(desugar-exp `(cond . ,rest)))]
      
      [`(and)   #t]
      [`(or)    #f]
      
      [`(or ,exp)
       (desugar-exp exp)]
      
      [`(and ,exp)
       (desugar-exp exp)]
      
      [`(or ,exp . ,rest)
       (define $t (gensym 't))
       (desugar-exp
        `(let ((,$t ,exp))
           (if ,$t ,$t (or . ,rest))))]
      
      [`(and ,exp . ,rest)
       `(if ,(desugar-exp exp)
            ,(desugar-exp `(and . ,rest))
            #f)]
      
      [`(if ,test ,exp) 
       `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
      
      [`(if ,test ,exp1 ,exp2)
       `(if ,(desugar-exp test) 
            ,(desugar-exp exp1) 
            ,(desugar-exp exp2))]
       
      [`(set! ,v ,exp)
       `(set! ,v ,(desugar-exp exp))]
      
      [`(assert ,test)
       `(assert1 ,(desugar-exp test))]
      
      [`(assert ,test ,kind)
       `(assert2 ,(desugar-exp test) ,(desugar-exp kind))]
      
      [`(get-global ,var)
      
       (global-name var)]
      
      [`(set-global! ,var ,exp) `(set! ,(global-name var) ,(desugar-exp exp))]
      
      [`(begin . ,exps)
       `(begin ,@(map desugar-exp exps))] 
      
      ;;;;;;;;;;;;;
      ['(return)
       exp]
      
      ['(break)
       exp]
      
      ['(continue)
       exp]
      
      [`(while ,cond ,body) 
       (gen-while2 cond body `(void))]  
      
      [`(while ,cond ,body ,else) 
       (gen-while2 cond body else)]
      
      [`(for-each ,var ,seq ,body ,else)
       (gen-for-each var seq body else)]
      
      [`(for-each ,var ,exp ,body)
       (gen-for-each var exp body `(void))]
      
      [`(dict (,keys ,values) ...)     
       `(dict ,@(map list keys (map desugar-exp values)))]
      
      [`(set . ,values)
       `(set ,@(map desugar-exp values))]
      
      [`(tuple . ,values)
       `(tuple ,@(map desugar-exp values))]
      
      [`(py-list* . ,values)
       `(py-list* ,@(map desugar-exp values))]
      
      [`(try ,body ,handler)
       (define $ec (gensym '$ec))
       (define $ex (gensym '$ex))
       (desugar-exp `(let (($old-handler $current-handler))
                       (let* ([$old-return   return]
                              [$old-continue continue]
                              [$old-break    break]
                              
                              [return   (lambda (rv) 
                                          (set! $current-handler $old-handler)
                                          (return rv))]
                              [continue (lambda ()
                                          (set! $current-handler  $old-handler)
                                          ($old-continue))]
                              [break    (lambda ()
                                          (set! $current-handler
                                                $old-handler)                               
                                               ($old-break))])                           
                         (call/ec                          
                          (lambda (,$ec)
                            (set! $current-handler
                             (lambda (,$ex) 
                               (set! $current-handler $old-handler)
                               (,$ec (,(desugar-exp handler) ,$ex))))
                            (let ((rv ,(desugar-exp body)))
                              (begin
                                (set! $current-handler $old-handler)
                                rv)))))
                         ))] 
      
      [`(throw ,exp)
       `($current-handler ,(desugar-exp exp))]
      
      [`(,f . ,args) 
       
       `(,(desugar-exp f) ,@(map desugar-exp args))]
      
      [(? atomic?) exp] 
      
      
      
      [else 
       (error (format "desugar fail: ~s~n" exp))]))   
  
  
  ;(pretty-write (desugar-program (read)))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (read-all port)
    (let ((next (read port)))
      (if (eof-object? next)
          '()
          (cons next (read-all port)))))
  
  (define (pydesug-file filename)
    (pytran-port (open-input-file filename)))
  
  (define (pytran-port port)
    (let* ([lines (read-all port)])
           (when (not (empty? lines))
             (pretty-write (desugar-program (car lines))))))
  
  (define args (current-command-line-arguments))
  (match args
    [(vector filename)   (pydesug-file filename) (exit)]  
    [else  (pytran-port (current-input-port))])
  
 ; (pydesug-file "func_class.py.trans")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 4: Desugaring python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  
  