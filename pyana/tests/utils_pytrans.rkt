;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 2: High level translation for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
; 
; This program provides all util funcitons for the 
; pytrans.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module utils
  racket/base
  
  (provide (all-defined-out))
  (require racket/match)
  (require racket/promise)
  (require racket/list)
  
  (define (DEBUG info) (display info) (newline))
  
  (define (force-prod p)
    (cond
      [(list? p) `(,@(map force-prod p))]
      [(promise? p) (force p)]
      [(pair? p) `(,(force-prod (car p)) ,(force-prod (cdr p)))]
      [else p]))
  
  (define global-symtbl (make-hash))
  (define global-dict-symtbl (make-hash))
 ; (define object (lambda () (void)))
  
  (define (gen-indexed-ids ids #:idx [idx 0])
    (if (empty? ids) (list)  (append (list (cons (first ids)  idx))  (gen-indexed-ids (drop ids 1) #:idx (+ idx 1))   )))
  
  (define opsmap (make-hash 
                  (list
                   (cons "+=" '(+)) (cons "-=" '(-)) (cons "*=" '(*)) (cons "/=" '(/)) (cons "%=" '(modulo))
                   (cons "&=" '(bitwise-and)) (cons "^=" '(bitwise-xor)) (cons "<<=" '(<<)) (cons ">>=" '(>>)) 
                   (cons "**=" '(expt)) (cons "|=" '(bitwise-or)) (cons "//=" '(quotient))
                   (cons "<" `<) (cons ">" `>) (cons "==" `equal?) (cons ">=" `>=) (cons "<=" `<=)
                   (cons "<>" `not-equal?) (cons "!=" `not-equal?) (cons 'in `in?) (cons 'not-in `not-in?)
                   (cons "in" `in?) (cons "not-in" `not-in?)
                   (cons "is" `eq?) (cons "is-not" `not-eq?) (cons "+" `+) (cons "-" `-) (cons "~" `bitwise-not)
                   (cons 'is `eq?) (cons 'is-not `not-eq?) (cons "*" `*)
                   (cons "/" `/) (cons "%" `modulo) (cons ">>" `>>) (cons "<<" `<<) (cons "//" `quotient))))
  
  (define (add-symbol name indef symtbl)
           
    (if indef 
        (when (not (hash-has-key? symtbl name))  (hash-set! symtbl name 'local))
        ;(begin
          ;(if (equal? name 'super)
             ; (hash-set! symtbl name 'local)
              (hash-set! symtbl name 'global))
          
          ;))
    )
  
  (define (declare-global name indef symtbl) (when (and indef (not (hash-has-key? symtbl name))) (hash-set! symtbl name 'global)))
  
  (define (declare-nonlocal name indef symtbl) (when indef 
                                                   (hash-set! symtbl name 'nonlocal)))
  
 
  
    ; for super call, built-in functions, they are not set as global. 
(define built-ins
    '(super len))

 (define (built-ins? fn) (member fn built-ins))

  (define (gen-get-res name indef symtbl)
;    (when (or (equal? name 'len) (equal? name 'super))
;    (displayln "---------")
;    (displayln name)
;    (displayln symtbl)
;    (displayln indef))
    (if indef
        (if (hash-has-key? symtbl name)
            (if (or (equal? 'local  (hash-ref symtbl name)) (equal? 'nonlocal (hash-ref symtbl name))) 
                name
                ;(if (equal? name 'super)
                  ;  name
                    `(get-global ,name)
                    ;)
            )
            (delay 
              (if (and (hash-has-key? symtbl name) (or (equal? 'local  (hash-ref symtbl name)) (equal? 'nonlocal  (hash-ref symtbl name))))
                  (begin
                    
                    name  )
                  (if (equal? 'print name) 
                      `py-print 
                      (if (built-ins? name)  ;(equal? 'super name)
                          (begin
                            
                            name)
                          (begin
                            
                            `(get-global ,name)
                            )))
                  )))
        (if (equal? name 'print)
            (delay (if (hash-has-key? symtbl name) `(get-global ,name) `py-print))
            (if (built-ins? name)
                    name
                    `(get-global ,name)
                    )
            )
        ))
  
  (define (gen-set-res name rhs indef symtbl)
    (if (or (not indef) (and indef (hash-has-key? symtbl name) (equal? 'global (hash-ref symtbl name))))
        `(set-global! ,name ,rhs) `(set! ,name ,rhs)))
  
  (define (gen-idxed-rhs-mm valsymb idx)  (let ((symbbase (gensym 'e))  (symbidx (gensym 'i)))
                                         `(let ((,symbbase ,valsymb))  
                                            (let ((,symbidx ,idx))
                                              (cond
                                                ((py-list? ,symbbase) (py-list-ref ,symbbase ,symbidx))
                                                ((tuple? ,symbbase) (tuple-ref ,symbbase ,symbidx))
                                                ((dict? ,symbbase) (dict-ref ,symbbase ,symbidx))
                                                ((string? ,symbbase) (string-ref ,symbbase ,symbidx))
                                                (else (error "cannot index object")))))))
  
 ; modified for better analysis
  (define (gen-idxed-rhs valsymb idx)  (let ((symbbase (gensym 'e))  (symbidx (gensym 'i)))
                                         ;`(let ((,symbbase ,valsymb))  
                                            `(let ((,symbidx ,idx))
                                              (cond
                                                ((py-list? ,valsymb) (py-list-ref ,valsymb ,symbidx))
                                                ((tuple? ,valsymb) (tuple-ref ,valsymb ,symbidx))
                                                ((dict? ,valsymb) (dict-ref ,valsymb ,symbidx))
                                                ;((string? ,symbbase) (string-ref ,symbbase ,symbidx))
                                                (else (error "cannot index object"))))))
  
  (define (gen-idxed-lhs-mm base idx rhs)
    (let ((symbbase (gensym 'b))  (symbidx (gensym 'i)))
      `(let ((,symbbase ,base))   (let ((,symbidx ,idx))
                                    (cond
                                      ;((tuple? ,symbbase)      (tuple-set! ,symbbase ,symbidx ,rhs))
                                      ((py-list? ,symbbase)    (py-list-set! ,symbbase ,symbidx ,rhs))
                                      ((dict? ,symbbase)       (dict-set! ,symbbase ,symbidx ,rhs))
                                      ((string? ,symbbase)     (error "cannot set string with non-char/string value"))
                                      )))))
  ;modified from the above for bettre analysis
  (define (gen-idxed-lhs base idx rhs)
    (let ((symbbase (gensym 'b))  (symbidx (gensym 'i)))
      ;`(let ((,symbbase ,base))   
         `(let ((,symbidx ,idx))
           (cond
             ;((tuple? ,symbbase)      (tuple-set! ,symbbase ,symbidx ,rhs))
             ((py-list? ,base)    (py-list-set! ,base ,symbidx ,rhs))
             ((dict? ,base)       (dict-set! ,base ,symbidx ,rhs))
             ;((string? ,symbbase)     (error "cannot set string with non-char/string value"))
             )))) 
  
  (define (not-done-match f s) (error (format "match not done in ~a for ~a\n" f s )))
  
  (define (gen-comps lhs cs)
    (if (= (length cs) 1)
        `(,(car (first cs)) ,lhs ,(cdr (first cs)))
        (let ((nlhs (gensym 'c)) (cp (first cs)))
          `(let ((,nlhs ,(cdr cp))) (if (,(car cp) ,lhs ,nlhs) ,(gen-comps nlhs (drop cs 1)) #f)))))
  
  (define (gen-chain-ops lhs ss)
    (if (= (length ss) 1)
        `(,(car (first ss)) ,lhs ,(cdr (first ss)))
        `(,(car (last ss)) ,(gen-chain-ops lhs (drop-right ss 1)) ,(cdr (last ss)))))
  
  (define (in-list? lst e)
    (cond
      ((empty? lst) #f)
      ((equal? e (first lst)) #t)
      (else (in-list? (drop lst 1) e))))
  
  (define (get-conflicted-symbs symtbl names)
    (map (λ (k) (cons k (hash-ref symtbl k))) (filter (λ (x) (in-list? names x)) (hash-keys symtbl))))
   


  ; newly added for making dict in case of class.
  ;;
  
(define (gen-class-dict attrs-vals class-name base class-dict)
  (let* ((var-attrs (list))
         (dict-var (map (λ (x) 
                         (match x 
                           [`(,set ,attr ,val) 
;                            (if (equal? attr `__init__) 
;                                (match val
;                                  [`(lambda ,formals ,body) 
;                                   (set! val 
;                                         `(lambda v (apply ,val ,(make-initial-obj-dict class-name) v)))]
;                                  [else val])
;                                val)
                                
                                  
;                                (set! var-attrs (extract-instance-fields val)) 
;                                var-attrs)
                            (list (symbol->string attr)   val)]
                           [else (error (format  "~a Failed make return-dict"  attrs-vals)) ]))
                        attrs-vals))
        
        (base-class 
         (if (empty? base) 
             ;`object
             `(list object)
             (if (not (has-object? base))
                 (begin 
                   `(list ,@(append base (list 'object))))
                 (begin
                   `(list ,@base))))
             )
        (res 
         (append (list `dict)
                 ;(list (cons "__base__"  (list base-class)))
                 (list (cons "__base__"  (list base-class)));(symbol->string base-class)
                                              ;))
                ; (list (cons "__mro__" (mro class-name)))
                 (list (cons "__class__" (list `(get-global ,class-name));(symbol-string class-name)
                                               )) 
                 (list (cons "__type__" "class"))
                 ;var-attrs
                 ;(display base-class)
                 dict-var
                 )))
    
    
    res))
  
  (define (has-object? base-lst)
    (if (empty? base-lst)
        #f
        (let ((cur-elem (car base-lst)))
          (if (equal? cur-elem 'object)
              #t
              (has-object? (drop base-lst 1))))))

(define (make-initial-obj-dict class-name)
  (append (list `dict)
          (list (cons "__class__" (list `(get-global ,class-name)))) ;class-name)
                                        ;(symbol->string class-name)
                                        
          (list (cons "__type__" (string-append "inst" (symbol->string class-name))))
          
          ))
  
;; extract instance field from init
;; list -> list of pair
(define (extract-instance-fields val)
  (match val
    [ `(lambda ,formals (call/ec (lambda (return) (let ,_ . ,let-body))))
      (for/fold ([res (list)])
        ([elem (in-list let-body)])
        (match elem 
          [`(set-field! self ,field ,value) (append (list (cons field value)) res)]
          [else (append res `())  ]))]))

; return a list of args except self.
(define (get-init-args suite)
  (for/fold ([res-args (list)])
    ([elem (in-list suite)])
    (match elem
      [`(,set ,attr ,val) 
       (if (equal? attr `__init__) 
           (append res-args 
                   (match val
                     [`(lambda ,formals ,body) (drop formals 1)] ;; we donot need the self argument
                     [else `()]))
           (append res-args `()))]
                           
      [else (append res-args `())])))
  
  
 (define (glb-class-name s)
   (string->symbol (string-append "classdict_"(symbol->string s))))
  
 (define (car-str listof-sym)
   (symbol->string (car listof-sym))) 
  
  ;; add the return 
 (define (keep-and-transform-init suite class-name)
   (for/fold ([res-args (list)])
    ([elem (in-list suite)])
    (match elem
      [`(,set ,attr ,val) 
       (if (equal? attr `__init__) 
           (match val
             [`(lambda ,formals (call/ec (lambda (return) (let ,let-content . ,let-body)))) 
              ;(list (append (list set)
                      ;(list attr)
;                      (let ((new-let-body (append let-body (list (list `return `self)))))
;                        (append res-args (list 
;                                          (list `lambda formals 
;                                                (list `call/ec 
;                                                      (list `lambda 
;                                                            (list `return) 
;                                                            (list `let let-content new-let-body)))))))
                      
                      (let (;(let-dict (list $localo (make-initial-obj-dict class-name)))
                            (init-args (get-init-args suite)))
                          (append res-args (list 
                                            (list `lambda formals 
                                                  (list `call/ec 
                                                      (list `lambda 
                                                            (list `return)
                                                            (list `let `(($localo ,(make-initial-obj-dict class-name)))
                                                                  `((get-field $localo "__init__") 
                                                                    ,@init-args)
                                                                  `(return $localo))))))))
                      ;))
                      ]
             
             [else (append res-args `())])
           (append res-args `()))]
                           
      [else (append res-args `())])))
 
  (define (make-init-call return-dict init-args)
    (append (list `__init__) (list return-dict) init-args))
  
  (define (has-init? rec-lst)
  (if (empty? rec-lst)
      #f
      (let ((cur-elem (car rec-lst)))
        (match cur-elem
          [`(,set ,attr ,val) 
           (if (equal? attr `__init__) 
               #t
               (has-init? (drop rec-lst 1)))]))))
  
  (define (gen-constr res-suite last2 class-name class-dict base localsymtbl return-dict init-args indef symtbl)
    (if (has-init? last2)
        (let* (
               (class-dict-val (gen-class-dict last2 class-name base class-dict))
               (final-suite (append (list) (list (first res-suite))
                                  `(,(let ((locals (list))) 
                                     (hash-map localsymtbl 
                                              (λ (k v) 
                                                (when (and (equal? v 'local) (not (in-list? base k)))
                                                  (set! locals (append locals (list `(,k (void))))))))
                                         locals))
;                                  (keep-and-transform-init last2) ;(drop res-suite 2)
;                                  (list `(return ,(make-init-call return-dict init-args)))
                                  ;(list `(return ,(make-initial-return-dict class-name)))
                                  (keep-and-transform-init last2 class-name)
                                  )))
         ;`(set! ,function-name (lambda ,params (call/ec (lambda (return) ,final-suite))))))]
         ;(gen-set-res class-dict (gen-class-dict last2 class-name base) indef symtbl) 
         ;(gen-set-res class-name  `(lambda () (call/ec (lambda (return) ,final-suite))) indef symtbl)))
;          (hash-set! global-dict-symtbl (symbol->string class-dict) class-dict)
;          (hash-set! global-dict-symtbl class-name
;                     ;`(lambda ,(get-init-args last2) (call/ec (lambda (return) ,final-suite)))
;                     class-dict)
          
                      
          `((lambda () 
              (begin
                
                ,(gen-set-res class-name  
                              `(lambda ,(get-init-args last2) (call/ec (lambda (return) 
                                                                         ;,final-suite
                                                                         (let (($localo  ,(make-initial-obj-dict class-name)))
                                                                           ((get-field $localo "__init__") 
                                                                            ,@init-args)
                                                                           (return $localo))
                                                                         ))) indef symtbl)
                (dict-set! gbl-dict (get-global ,class-name) ,class-dict-val)
                (dict-set! (dict-ref gbl-dict (get-global ,class-name)) "__mro__" (mro (get-global ,class-name)))
                ;(mro ,class-name) 
                )))
          )
        (let* (
               (class-dict-val (gen-class-dict last2 class-name base class-dict))
               ;(tmp-dict return-dict) stupid line!!!!!
               
               (final-suite (append 
                             (list) 
                             (list (first res-suite))
                             `(,(let 
                                    ((locals (list))
                                     ) 
                                  (hash-map localsymtbl 
                                            (λ (k v) 
                                              (when (and (equal? v 'local) (not (in-list? base k)))
                                                (set! locals (append locals (list `(,k (void))))))))
                                  locals))
                             ;(list `((get-field ,tmp-dict "__init__"))) ;; --stupid line!
                             ;(list `(return ,tmp-dict)) stupid line!!!!
                             ;;; The above two lines do not change tmp-dict, or say the return-dict! it just literally expand the value of tmp-dict
                             ;; the fill in the place. Even when executing, no change on the tmp-dict!!!!
                             
                             (list 
                              `(let (($localo ,(make-initial-obj-dict class-name)))
                                ((get-field $localo "__init__") 
                                 ,@init-args)
                                (return $localo)))
                             
                             )))
         ;(hash-set! global-dict-symtbl (symbol->string class-dict) class-dict)
;          (hash-set! global-dict-symtbl class-name
;                     ;`(lambda () (call/ec (lambda (return) ,final-suite)))
;                     class-dict)
          `((lambda ()
              (begin
               
                
                ,(gen-set-res class-name  `(lambda () (call/ec (lambda (return) ,final-suite))) indef symtbl)
                 (dict-set! gbl-dict (get-global ,class-name) ,class-dict-val)
                (dict-set! (dict-ref gbl-dict (get-global ,class-name)) "__mro__" (mro (get-global ,class-name)))
                ;(mro ,class-name) 
                )))
          )))

  ;;;;;;;;;;;;;;;;;;;;
  
  ; operations for container comprehension
  
    
  (define (compfor? lst)
  (match lst
    [`(for-each ,_ ,_) #t]
    [else #f]))
  
  
  (define (constr-comp-lst comp-lst res-lst)
   (match comp-lst
     [`(compfor ,name ,expr ,comp-iter) 
      (let ((tmp-lst 
             (append (list `(for-each ,name ,expr)) res-lst)))
        (constr-comp-lst comp-iter tmp-lst))
        ]
     [`(compif ,test ,comp-iter)
      (let ((tmp-lst
             (append res-lst  `(,test))))
        (constr-comp-lst comp-iter tmp-lst))
      ]
     [`(compfor ,name ,expr) 
      (let ((tmp-lst 
             (append (list `(for-each ,name ,expr)) res-lst)))
        tmp-lst)
        ]
     [`(compif ,test)
      (let ((tmp-lst
             (append res-lst  `(,test))))
        tmp-lst)
      ]
     [else  (displayln comp-lst )
            res-lst]
     ))
  
  ; get the comprehension body!
  
  (define (gen-compbody-lst for-expr)

    (match for-expr 
      [`(dict (,k ,v))       ;;;;; special dealing with the dict comprehension, 
                               ;;; generated from the pytrans2.rkt dict comprehension matching
       
;       (let* ((k-v (car (hash->list for-expr)))
;              (k (first k-v))
;              (v (second k-v)))
       `(dict-set! res ,k ,v)]
      
      [else   
        
        `(begin
       
           (dict-set! res $i ,for-expr)
           (set! $i (+ $i 1)))])
    
    )
  
  (define (gen-ifs comp-if for-expr)
  (if (empty? comp-if)
      (begin
       
          (gen-compbody-lst for-expr)
         
        )
      (if (not (list? (car comp-if)))
          (append  (list `if) 
                   (list comp-if)
                   (list (gen-compbody-lst for-expr))
                   (list `(void)))
      
          (begin
            (append 
             (list `if) 
             (list (append (list `and) comp-if)) 
             (list (gen-compbody-lst for-expr))
             (list `(void)))
            ))))
 
   ;comprehension, there is at least one for!
(define (gen-for-args fors )
  (if (empty? fors)
      (error (format "comprehension has no compfor" fors))
      (map (lambda (x) 
             (match x
               [`(for-each ,name ,seq) 
               ; (hash-set! symtbl name (if indef 'local 'global))     
                `(,name (void))]))
             fors)))
  
(define (desugar-compre comp-lst for-expr)
  (define-values (comp-for comp-if) (partition compfor? comp-lst))
  
  
  (define ifs 
    (gen-ifs comp-if for-expr)
    )
  
  (append `(let ((res (dict));(py-list*))
                 ($i 0)
                 ,@(gen-for-args comp-for ))) 
          (list
           (for/fold ([res  ifs])
             ([elem (in-list comp-for)])
             (append elem (list res))))
          (list `res)))
  
;;;; helpers for turning py-list to dict
 (define (constr-pylist-dict-pairs  elems)
  (let ((res (list))
        (i 0))
    (for ([elem elems])
      (set! res (append res (list (list i elem) )))
      (set! i (add1 i))
        )
    res)
  )
  
  (define (gen-list-append)
     (define self (gensym 'self))
    (define elem (gensym 'elem))
    
    (define max (gensym 'max))
    
    `(lambda (,self ,elem)
       (let ((,max (len ,self)))
         (dict-set! (dict-ref ,self "__containerdict__") ,max ,elem))))

  

  (define (gen-list-insert)
    
     (define self (gensym 'self))
    (define elem (gensym 'elem))
    (define container (gensym 'container))
    (define max (gensym 'max))
    
    `(lambda (,self index ,elem)
      (cond 
            [(= index 0) 
             (let ((i 0)
                   (,container (dict-ref ,self "__containerdict__"))
                   )
               (for-each e ,self
                         (begin
                           (set! i (+ i 1))
                           (dict-set! ,container i e)))
               (dict-set! ,container  0 ,elem))
             ]
            [(>= index (len ,self))
             (let ((tail (len ,self))
                   (,container (dict-ref ,self "__containerdict__")))
               (dict-set! ,container tail ,elem))
             
             ]
            [(and (> index 0) (< index (len ,self)))
             (let ((,max (len ,self))
                   (,container (dict-ref ,self "__containerdict__")))
               (let ((i ,max))
                 (while (and (>= ,max i) (> i index))
                        (begin
                          (dict-set! ,container i (dict-ref ,container (- i 1)))
                          (set! i (- i 1))
                          ))
                 (dict-set! ,container index ,elem)))
             ]
            
            )))
  
  (define (gen-list-index)
    
     (define self (gensym 'self))
    (define elem (gensym 'elem))
    (define container (gensym 'container))
    (define max (gensym 'max))
    
    `(lambda (,self ,elem)
      (let ((,container (dict-ref ,self "__containerdict__"))
            (i 0)
            (found #f)
            (,max (len ,self)))
        (while (and (<= 0 i) (< i ,max))
               (cond
                 [(equal? (dict-ref ,container i) ,elem) 
                  (begin
                    (if (not found)
                        (set! found i)
                        (void))
                    (set! i (+ i 1)))]
                 [else (set! i (+ i 1))]))
        (if (not found)
            (error (format "no such elem: ~a in list ~a" ,elem ,container))
            found)
        )))
  
  (define (gen-list-count)
    
    
      (define self (gensym 'self))
    (define elem (gensym 'elem))
    (define container (gensym 'container))
    (define max (gensym 'max))
    
    `(lambda (,self ,elem)
      (let ((,container (dict-ref ,self "__containerdict__"))
            (i 0)
            (cnt 0)
            (,max (len ,self)))
        (while (and (<= 0 i) (< i ,max))
               (cond
                 [(equal? (dict-ref ,container i) ,elem) (begin (set! cnt (+ cnt 1)) (set! i (+ i 1))) ]
                 [else (set! i (+ i 1))]))
        cnt
        )))
;  
  
 ; global-name : symbol -> symbol
(define (global-name name)
  (string->symbol (string-append "g$" (symbol->string name)))) 
;  (define (gen-list-init)
;    `(lambda (self tmp-list-dict )
;       (call/ec
;        (lambda (return)
;          (set-field! self "__containerdict__" tmp-list-dict);(dict-ref tmp-list-dict "__containerdict__"))
;          ))))
  
  (define (gen-list-init)
    
    (define self (gensym 'self))
    (define tmp-dict (gensym 'tmpdict))
    
    `(lambda (,self ,tmp-dict )
       (call/ec
        (lambda (return)
          (if (py-list? ,tmp-dict)
              (set-field! ,self "__containerdict__" (dict-ref ,tmp-dict "__containerdict__"))
              (set-field! ,self "__containerdict__" ,tmp-dict);(dict-ref tmp-list-dict "__containerdict__"))
          )))))
  
   (define (gen-tuple-init)
     (define self (gensym 'self))
    (define tmp-dict (gensym 'tmp-dict))
    `(lambda (,self ,tmp-dict)
       (call/ec
        (lambda (return)
          (if (or (py-list? ,tmp-dict) (tuple? ,tmp-dict) (set? ,tmp-dict))
              (set-field! ,self "__containerdict__" (dict-ref ,tmp-dict "__containerdict__"))
              (set-field! ,self "__containerdict__" ,tmp-dict);(dict-ref tmp-list-dict "__containerdict__"))
          )))))    
  
;  (define (gen-set-gbl-list)
;    `(set-global!
;     List
;     (lambda (tmp-list-dict)
;       (call/ec
;        (lambda (return)
;          (let (($localo 
;                 (dict 
;                  ("__class__" List) 
;                  ("__type__" "instPyList"))))
;            ((get-field $localo "__init__") tmp-list-dict) ; v is list of pairs
;            (return $localo)))))))
  
    (define (gen-set-gbl-container cls-type type-str)
      (define self (gensym 'self))
      (define tmp-dict (gensym 'tmp-dict))
      (define localo (gensym '$localo))
      
    `(set-global!
     ,cls-type
     (lambda (,tmp-dict)
       (call/ec
        (lambda (return)
          (let ( (,localo ;($localo 
                 (dict 
                  ("__class__" (get-global ,cls-type)) ;,cls-type) 
                  ("__type__" ,type-str))))
            ((get-field ,localo "__init__")  ,tmp-dict) ;tmp-dict is a dict.
            (return ,localo)))))))
  
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 3: High level translation for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
; 
; This program provides all util funcitons for the 
; pytrans.rkt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;