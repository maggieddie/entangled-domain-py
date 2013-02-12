#lang racket

(require racket/mpair)
(require "utils_pytrans.rkt")

(define-syntax program
  (syntax-rules ()
    [(_ body ...) (begin body ...)]))


;; Globals.
(define-syntax (set-global! stx)
  (syntax-case stx ()  
    [(_ var value)
     (with-syntax ([gvar (datum->syntax #'set-global! (syntax->datum #'var))])
       #'(set! gvar value))]))

(define-syntax (get-global stx)
  (syntax-case stx ()  
    [(_ var)
     (with-syntax ([gvar (datum->syntax #'set-global! (syntax->datum #'var))])
       #'gvar)]))


;; Control constructs.
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ cond body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (letrec ([loop (λ ()
                                     (when cond
                                       (call/ec (λ (continue)
                                                  body))
                                       (loop)))])
                                                  
                      (loop)
                      else))))]
    
    [(_ cond body)
     ; =>
     #'(while cond body (void))]))



(define-syntax (for-each stx)
  (syntax-case stx ()
    [(_ var seq body else)
     ; =>
     (with-syntax ([break    (datum->syntax #'body 'break)]
                   [continue (datum->syntax #'body 'continue)])
       #'(call/ec (λ (break)
                    (let (($seq seq))
                      (cond
;                        [(set? $seq)
;                         (for ([var $seq])
;                           (call/ec (λ (continue)
;                                      body)))]
                        
;                        [(tuple? $seq)
;                         (for ([var $seq])
;                           (call/ec (λ (continue)
;                                      body)))]
                        
                        [(or (set? $seq) (py-list? $seq) (tuple? $seq))
                         (for ([var 
                                ;(py-list-mlist $seq)
                                (ordered-values (dict-ref $seq "__containerdict__"))
                                    ])
                           (call/ec (λ (continue)
                                      body)))]
                        
                        [(hash? $seq)
                         (for ([(var _) $seq])
                           (call/ec (λ (continue)
                                      body)))])
                      else))))]
                          

    
    [(_ var seq body)
     ; =>
     #'(for-each var seq body (void))]))

(define (return) (error "cannot return from this context"))
(define (break) (error "cannot break from this context"))


;; Exceptions.
(define $current-handler (λ (ex) ("no handler installed")))

(define (current-handler) $current-handler)
(define (set-current-handler! handler) (set! $current-handler handler))

(define-syntax (try stx)
  (syntax-case stx ()
    [(_ body handler)
     ; =>
     (with-syntax ([return   (datum->syntax #'body 'return)]
                   [continue (datum->syntax #'body 'continue)]
                   [break    (datum->syntax #'body 'break)])
       #'(let* ([$old-handler   (current-handler)]
                [$old-return    return]
                [$old-continue  continue]
                [$old-break     break]
                [return       (λ args
                                (begin (set-current-handler! $old-handler)
                                       (apply return args)))]
                [continue     (λ ()
                                (begin (set-current-handler! $old-handler)
                                       ($old-continue)))]
                [break        (λ ()
                                (begin (set-current-handler! $old-handler)
                                       ($old-break)))])
           (call/ec (λ (ec)
                      (set-current-handler! 
                       (λ (ex)
                         (set-current-handler! $old-handler)
                         (ec (handler ex))))
                      (let ([rv body])
                        (set-current-handler! $old-handler)
                        rv)))))]))
                    
(define (throw ex)
  ($current-handler ex))
     

(define (Exception) '(Exception))

  

;; Assertion.
(define-syntax assert
  (syntax-rules ()
    [(_ test) 
     (when (not test)
       (error "AssertionFailure"))]
    
    [(_ test kind)
     (when (not test)
       (error (format "AssertionFailure: ~s~n" kind)))]))


;; Data structures.
(define-syntax dict
  (syntax-rules ()
    [(dict ((k v) ...))
     (make-hash (list (cons k v) ...))
     ]
    [(_ (k v) ...)
     ; =>
     (make-hash (list (cons k v) ...))]
   
    ))
;(define object (make-hash (list (cons "__base__" (list)))))


(define dict? hash?)
;(define dict-ref hash-ref)
(define (dict-ref hs key)
  (if (hash-has-key? hs key)
      (hash-ref hs key)
      #f))
(define dict-set! hash-set!)

;(define-syntax tuple
;  (syntax-rules ()
;    [(_ v ...)
;     ; =>
;     (vector v ...)]))

;(define tuple-ref vector-ref)
;(define tuple-set! vector-set!)
;(define tuple? vector?)


(define (mlist-set! mlst n value)
  (cond
    [(null? mlst)  (error "mlist-set! -- index too high")]
    [(= n 0)       (set-mcar! mlst value)]
    [else          (mlist-set! (mcdr mlst) (- n 1) value)]))

(define (mlist-remove! mlst n)
  (cond
    [(null? mlist) (error "cannot delete from empty list")]
    [(= n 1)       (set-mcdr! mlst (mcdr (mcdr mlst)))]
    [else          (mlist-remove! (mcdr mlst) (- n 1))]))

     
;(define-struct py-list ([mlist #:mutable]))

;(define (py-list-set!-old pl i val)
;  (mlist-set! (py-list-mlist pl) i val))

(define (py-list-set! base index val)
  (if (not (hash? base))
      (error (format "~a is not a py-list dict in listset!" base))
      (let ((cnt (hash-count (dict-ref base "__containerdict__"))))
        (if (>= index cnt)
            (error (format "py-list set index ~a is  out of range ~a!" index cnt))
            (dict-set! (dict-ref base "__containerdict__") index val)))))
            
    
    

;(define (py-list-ref-old pl i)
;  (mlist-ref (py-list-mlist pl) i))

(define (py-list-ref base index)
  (if (not (hash? base))
      (error (format "~a is not a py-list dict in list-ref!" base))
      (let ((cnt (hash-count (dict-ref base "__containerdict__"))))
        (if (>= index cnt)
            (error (format "py-list set index ~a is  out of range ~a!" index cnt))
            (dict-ref (dict-ref base "__containerdict__") index)))))


(define tuple-ref py-list-ref)



;(define (py-list-remove!-old pl i)
;  
;  (cond
;    [(< i 0)  (error "index out of bounds for removal")]
;    [(= i 0)  (set-py-list-mlist! (mcdr (py-list-mlist pl)))]
;    [else     (mlist-remove! (py-list-mlist pl) i)]))

;has to automatically shift the index after index -1
(define (py-list-remove! base index)
  (if (not (hash? base))
      (error (format "~a is not a py-list dict in list remove!" base))
      (let* ((base-elem (dict-ref base "__containerdict__"))
             (cnt (hash-count base-elem)))
        (cond 
          [(or (< index 0) (>= index cnt)) (error "index ~a is out of bounds for removal")]
          [(= index (sub1 cnt)) 
           (dict-remove! (dict-ref base "__containerdict__") index)
           
           ]
          [(and (= index 0) (= cnt 1)) (dict-remove! (dict-ref base "__containerdict__") index)]
          [(and (>= index 0) (< index (sub1 cnt)))  
           ;(displayln "in the index < cnt-1")
           (let* ( (list-dict (dict-ref base "__containerdict__"))
                   (key-lst #f))
             (dict-remove! list-dict index)
             (set! key-lst (hash-keys list-dict))
             (for ([key (sort key-lst <)])
               (if (> key index)
                   (begin
                     (let ((tmp (dict-ref list-dict key)))
                       ;(displayln tmp)
                       (dict-remove! list-dict key)
                       (dict-set! list-dict (sub1 key)  tmp))
                     )
                   (void)
                 ))
           
             )
           ]
          )
        )))

(define (py-list? base)
   (if (not (hash? base))
      #f;(error (format "~a is not a py-list dict in test list!" base))
      (if (equal? (dict-ref base "__type__") "instPyList") #t #f)))

(define (set? base)
  (if (not (hash? base))
      #f;(error (format "~a is not a py-list dict in test list!" base))
      (if (equal? (dict-ref base "__type__") "instSet") #t #f)))

(define (tuple? base)
  (if (not (hash? base))
      #f;(error (format "~a is not a py-list dict in test list!" base))
      (if (equal? (dict-ref base "__type__") "instTuple") #t #f))) 

(define (specific-container-ty? base typename)
  (if (not (hash? base))
      #f
      (if (equal? (dict-ref base "__type__")  typename) #t #f)))

; all the container will be turned into dict with the __containerdict__ mapping to the container elements   
;(define (len container)
;   ;(lambda (container)
;       (let ((i 0))
;         (for  ([(k v) (dict-ref container "__containerdict__")])
;           (set! i (+ i 1)))
;         i));)
(define-syntax len 
  (syntax-rules ()
    [(_ container)
     
     (let ((i 0))
         (for  ([(k v) (dict-ref container "__containerdict__")])
           (set! i (+ i 1)))
         i)]))
;(define (py-list* . args)
;  (py-list (list->mlist args)))
;
;(define (py-list-append! pl val)
;; (if (list? (py-list-mlist pl))
;     ;(set-py-list-mlist! pl
;      ;                   (mappend! (py-list-mlist pl) (list->mlist (list val))))
;  (if (equal? (mmember val (py-list-mlist pl)) #f)
;     (mappend! (py-list-mlist pl) (list->mlist (list val)))
;      (mappend! (py-list-mlist pl) (list->mlist (list)))))
      
(define (lam? val)
  (match val
    [`(lambda ,_ ,_) #t]
    [else #f]))



;; Objects.
(define-syntax get-field-old 
  (syntax-rules ()
    [(_ obj name) 
     ;(error "get-field not supported")
     ;; obj is a dict, 
     ;(let ((key-str (symbol->string name)))
       (if (and (hash? obj) (hash-has-key? obj name))
         ;(if (hash? obj)
          ;   (if (equal? hash-ref obj "__type__" "super")
           ;      (begin
                   ; find the next class-dict.
            ;       )
                ; (if (hash-has-key? obj name) ; inst or class has the name?
                     ;inst
                     (hash-ref obj name)
           ;; refer to the class dict
                     (let* ((class-dict  
                             (if (procedure? obj) ; procedure used to signify a class
                                 (begin 
                                   ;(display (format "~a is a procedure" obj))
                                   ; (display (format "~a hash-ref result: " (hash-ref gbl-dict obj)))
                                   (hash-ref gbl-dict obj)) ; if it is class, thendirectly find the class-dict in teh glb-dict
                                 ;                       (hash-ref gbl-dict 
                                 ;                                 (string-append  (hash-ref obj "__class__") "_dict"))
                                 (hash-ref gbl-dict 
                                           (hash-ref obj "__class__"))
                                 
                                 ))
                            ;(class-dict (eval (string->symbol (string-append (hash-ref obj "__class__") "_dict")))) 
                  
                            
                            (cpl (hash-ref class-dict "__base__")))
                       
                       (if (hash-has-key? class-dict name)
                           (let ((field-val  (hash-ref class-dict name))) ;the field is in the class definition.
                             (if (and (procedure? field-val) (not (procedure? obj))) ; the field is method and it is the instance
                                 ; since class is translated as callable procedure.
                                 (begin
                                   (lambda v (apply field-val obj v))) ; I like this line
                                 field-val)) ; it is a var reference, either instance or class
                           ;; iterate the class precedence list 
                           (if (equal? cpl 'object)
                               (if (hash-has-key? cpl)
                                   (hash-ref cpl name)
                                   (error (format "no ~a found in object" name)))
                               ; besides object, there are other base classes, and in this case, cpl is a list with at least 2 elements
                               (let ((search-res (search-field cpl name cflt)))
                                 (if (equal? search-res #f)
                                     (error (format "~a not found in ~a" name cpl))
                                     search-res)))
                 )))]))

;new get-field
;; instance, class, super has all been desugared into dict. each dict has its type field indicate whether it is instance
;; class or supeer

(define-syntax get-field 
  (syntax-rules ()
    [(_ obj name)
     
     (if (hash? obj) ; include instance dict and super dict.
          
         ;(if (equal? (substring (hash-ref obj "__type__") 0 4) "inst")
         (cond 
           [(equal? (substring (hash-ref obj "__type__") 0 4) "inst")
             (begin
               (if (hash-has-key? obj name)
                   (begin
                     (let ((field-val (hash-ref obj name)))
                       (if (procedure? field-val); instance method
                           
                           (begin 
                             (if  (not (equal? name "__class__"))
                                  (begin
                                    
                                    (lambda v (apply field-val obj v))
                                  )
                                  field-val))
                             
                           field-val))
                     
                     )
                   ; travel the mro of the class of the instance.
                   (begin
                     (let ((cls (hash-ref obj "__class__")))
                       (if (hash-has-key? gbl-dict cls)
                           (begin
                             (travel-mro (hash-ref (hash-ref gbl-dict cls) "__mro__") name "inst" obj #f)
                             )
                           (begin
                             (error (format "the gbl-dict ~a instance ~a not has class ~a" gbl-dict cls))
                             (newline)
                             )
                           ))
                     
                     )))
             ]
             ;;super dict
           [(equal?  (hash-ref obj "__type__")  "super")
             (begin
               (let ((cur-cls (hash-ref obj "__thisclass__"))
                     (super-self (hash-ref obj "__self__")))
                 (if (hash? super-self) ; instance super
                     (let ((next-class
                            (let* ((inst-cls-mro (hash-ref (hash-ref gbl-dict (hash-ref super-self "__class__")) "__mro__"))
                                   (cur-index (elem-index inst-cls-mro cur-cls)))
                              (if (equal? cur-index #f)
                                  (error (format "~a not found in the mro list ~a" cur-cls inst-cls-mro))
                                  (if (equal? (length inst-cls-mro) (add1 cur-index))
                                      (error (format "~a already the last one in the mro list ~a" cur-cls inst-cls-mro))
                                      (list-ref inst-cls-mro (add1 cur-index))))))
                           )
                       (travel-mro (hash-ref (hash-ref gbl-dict next-class) "__mro__") name "super" #f super-self))
                     (error (format "super-sefl is not an instance hash: ~a" super-self)))))
             ;)
             ]
           [(equal?  (hash-ref obj "__type__")  "pylist")
            (let ((list-dict (dict-ref obj "__containerdict__")))
              (if (hash-has-key? obj name)
                  (let ((res (hash-ref obj name)))
                        (if (procedure? res)
                            (lambda v (apply res obj v))
                            res))
                      
                  (error (format "~a does not have ~a" obj name))))
            ])
         ; obj is not an hash, then it is probably a class procedure
         (if (procedure? obj)
             (if (hash-has-key? gbl-dict obj)
                 (begin
                   (let ((obj-dict (hash-ref gbl-dict obj)))
                     (travel-mro (hash-ref obj-dict "__mro__") name "class" #f #f))
                   )
                 (begin
                   (error (format "no obj: ~a in the gbl-dict ~a " obj gbl-dict))
                   )
                 )
             ; not an procedure (and not an dict)
             (error (format "not an procedure nor hash: ~a" obj))
             
             ))
     ]))



;(define (get-precessors cls-lst)
;  (append-map (λ (x)  (hash-ref (hash-ref gbl-dict x) "__base__")) cls-lst))
;
;(define (cflt class-dict fn)
;  (if (hash-has-key? class-dict fn) #t #f))
;
;(define (search-field classes field-name flt)
;  (let* ((cls-dicts (map (λ (x) (hash-ref gbl-dict x)) classes))
;        (res (filter (λ (x) (flt x field-name)) cls-dicts)))
;       (if (not (empty? res))
;           (hash-ref (car res) field-name)
;           (let ((supers (get-precessors classes)))
;             (if (empty? supers)
;                 #f
;                 (search-field supers field-name flt))))))
;;;;;;;;;;;

(define (travel-mro mro-lst name type obj super-self)
  (if (empty? mro-lst)
      
      (error (format "~a not found in mro list ~a" name mro-lst))
      (let* ((cur-elem (car mro-lst))
             (cur-dict (hash-ref gbl-dict cur-elem))
             )
        (if (hash-has-key? cur-dict name)
            (begin
              (let* ((field-val (hash-ref cur-dict name)))
                (if (procedure? field-val)
                    (if (equal? type "inst")
                        (lambda v (apply field-val obj v))
                        (if (equal? type "super")
                            (lambda v (apply field-val super-self v))
                            field-val))
                    field-val))
              )
            (travel-mro (drop mro-lst 1) name type obj super-self)))))


(define (get-transformed-lam field-val obj)
  (match field-val
    [`(lambda ,formals (call/ec (lambda (return) (let ,let-vars . ,let-body))))
     (let* ((formals-without-self (drop formals 1))
           (new-let-var  (list (list `new-lam (list `void))))
           (new-let-body (append  (list (list `set! `new-lam  field-val))
                                 ;let-body 
                                  (list (append (list `new-lam) 
                                                (list `obj)
                                                formals-without-self))))
           
           )
       
       (lambda formals-without-self 
              (call/ec 
                    (lambda (return)
                         (let ((new-lam (void)))  
                           (set! new-lam  field-val) 
                           (new-lam obj))))))]))
        
;        `,(list `lambda formals-without-self 
;              (list `call/ec 
;                    (list `lambda 
;                          (list `return) 
;                          (list `let new-let-var  
;                                (list `set! `new-lam  field-val) 
;                                (append (list `new-lam) 
;                                                (list `obj)
;                                                formals-without-self))))))]))
                                
                
                                           
(define (get-lam-with-only-self val)
  (match val
    [`(lambda ,formals (call/ec (lambda (return) (let ,let-vars . ,let-body))))
     (list 
        (list `lambda (list (car formals))
              (list `call/ec 
                    (list `lambda 
                          (list `return) 
                          (list `let let-vars let-body)))))]))
                                           


(define-syntax set-field!
  (syntax-rules ()
    [(_ obj name val) 
     ;(error "set-field! not supported")]))
     (if (procedure? obj)
         (hash-set! (hash-ref gbl-dict obj) name val)
         (hash-set! obj name val))]))

(define-syntax remove-field!
  (syntax-rules ()
    [(_ obj name) 
     (if (procedure? obj)
         (hash-remove! (hash-ref gbl-dict obj) name val)
         (hash-remove! obj name))]))
         

 ;; the following is for computing MRO

;;  the mro computes the __mro__ for the cls, and extend the global store in the mapping for __mro__,
; which should not be like this.
;  (define (mro cls)
;    (let ((res 
;           (append (list cls)
;                   (merge
;                    (append 
;                     (map (lambda (x) (mro x)) (hash-ref (hash-ref gbl-dict cls) "__base__"))
;                     (list (hash-ref (hash-ref gbl-dict cls) "__base__")))
;                    (list)
;                    ))))
;      ;(hash-set! gbl-dict "__mro__" res)
;      res
;      ))
;  
;  ;; for the translator
;   (define (mro-trans cls)
;    (let ((res 
;           (append (list cls)
;                   (merge
;                    (append 
;                     (map (lambda (x) (mro-trans x)) (hash-ref (hash-ref runtime-gbl-dict cls) "__base__"))
;                     (list (hash-ref (hash-ref runtime-gbl-dict cls) "__base__")))
;                    (list)
;                    ))))
;      ;(hash-set! gbl-dict "__mro__" res)
;      res
;      ))



;(define (merge lst-of-parentlist res)
;  
;  (if (empty-lists? lst-of-parentlist)
;      ;(begin 
;       ; (display lst-of-parentlist)
;        res
;        ;)
;      (let ((ctl 0)) ; everytime new round, then ctl is set to 0
;        (let iter (;[tmp-lst lst-of-parentlist]
;                   [i ctl])
;          (if (= i (length lst-of-parentlist))
;              (begin 
;                ;(display i)
;                #f)
;              (begin
;                ;(display lst-of-parentlist)
;                ;(newline)
;                (let* ((cur-lst (list-ref lst-of-parentlist i)) 
;                       ;(cand (car cur-lst))
;                       )
;                  (if (empty? cur-lst)
;                      (iter (add1 i))
;                      (let ((cand (car cur-lst)))
;                        (if (in-tail? cand (remove cur-lst lst-of-parentlist))
;                            (begin
;                    
;                              (iter (add1 i)))
;                      ; a good header.
;                            (let ((temp-res (append res (list cand)))
;                                  (new-lsts (map 
;                                             (lambda (x)
;                                               (if (not (member cand x))
;                                                   x
;                                                   (if (equal? cand (car x))
;                                                       (drop x 1)
;                                                       x)))
;                                             lst-of-parentlist)))
;                              (merge new-lsts temp-res))))))))) )))
;
;(define (flt cand lst)
;  (if (empty? lst)
;     ;
;      #f
;      (if (not (member cand (drop lst 1)))  
;      ; good cands
;          #f 
;      ; cand in the tail of the lst
;          #t)))
;      
;
;(define (in-tail? cand others)
;  (let ((res (filter 
;              (lambda (x) (flt cand x))
;              others)))
;    (if (empty? res)
;        #f
;        #t)))
;
;(define (empty-lists? lsts)
;  (let ((res
;         (filter (lambda (x)  (if (empty? x) #f #t)) lsts)))
;    (if (empty? res) #t #f)))

;;;; Desugar super into a function --- temperaily support the explicit define current class as well as the instance.
; super:: current-class -> self argument -> class procedure 

(define super-old (lambda v 
                (if (empty? v)
                    (error "not support python 3 super!")
                    (let ((cur-cls (first v))
                          (inst (second v)))
                      (if (hash? inst)
                          (let* ((inst-cls-mro (hash-ref (hash-ref gbl-dict (hash-ref inst "__class__")) "__mro__"))
                                (cur-index (elem-index inst-cls-mro cur-cls)))
                            (if (equal? cur-index #f)
                                (error (format "~a not found in the mro list ~a" inst inst-cls-mro))
                                (if (equal? (length inst-cls-mro) (add1 cur-index))
                                    (error (format "~a already the last one in the mro list ~a" inst inst-cls-mro))
                                    (list-ref inst-cls-mro (add1 cur-index)))))
                          (error (format "not an instance hash in super" inst)))
                      ))))
;desugar super into a dict!

(define-syntax super 
  (syntax-rules ()
    [(_ cls inst-sub)
     
     (make-hash (list (cons "__type__" "super")
                      (cons "__thisclass__" cls)
                      (cons "__self__" inst-sub)))]))
     

(define (elem-index lst elem)
  (let find-index ([i 0])
    (if (= i (length lst))
        #f
        (if (equal? (list-ref lst i) elem)
            i
            (find-index (add1 i))))))


;; Operators.
(define (<< a n) (arithmetic-shift a n))
(define (>> a n) (arithmetic-shift a (- n)))

(define (not-equal? a b)
  (not (equal? a b)))

(define-syntax (define/return stx)
  (syntax-case stx ()
    [(_ f-params body ...)
     ; =>
     (with-syntax ([return (datum->syntax #'f-params 'return)])
     #'(define f-params (call/ec (λ (return) body ...))))]))
  
(define/return (in? needle haystack)
  (cond
    [(hash? haystack)     (for ([(x y) haystack])
                            (when (equal? x needle)
                              (return #t)))]
    [(or (py-list? haystack) (set? haystack) (tuple? haystack))
     (return (in? needle 
                  (hash-values (dict-ref haystack "__containerdict__")) 

                  ))]
    
    
    [else                 (for ([x haystack])
                            (when (equal? x needle) 
                              (return #t)))])
  #f)
        
(define not-in? (λ (needle haystack) (not (in? needle haystack))))


;; Special variables
(define None 'None)
(define Ellipsis 'Ellipsis)


;; Standard continuations:
; return
; break 
(define continue (λ _ (error "top-level continue")))



;; Library functions.

(define bitwise-or bitwise-ior)

;(define (py-print-old x) 
;  (cond 
;    [(py-list? x)  (display (py-list-mlist x))]
;    [else          (display x)])
;  (newline))

(define-syntax py-print 
  (syntax-rules ()
               [(py-print elem ...) 
                ((lambda v 
                  (for ([i v])
                    
                    (cond 
                      [(py-list?  i)  ; since v is a list!
                       ;(displayln (mlist->list (py-list-mlist  i)))
                       (if (= (len i) 0)
                           (begin
                             (displayln "[]"))
                           (begin
                             (display "[")
                             (recur-prnt-pylst i)
                             (displayln "]")))
                       ;(newline)
                       ]
                      [(set? i)
                       (if (= (len i) 0)
                           (begin
                             (displayln "set()"))
                           (begin
                             (display "{")
                             (recur-prnt-pylst i)
                             (displayln "}")))
                      ; (newline)
                       
                       ]
                      [(tuple? i)
                       (if (= (len i) 0)
                           (begin
                             (displayln "()"))
                           (begin
                             (display "(")
                             (recur-prnt-pylst i)
                             (displayln ")")))
                       ;(newline)
                       
                       ]
                      
                      
                      [else        
                       (displayln i)
                       
                       ])
                    )
                   
                   )
                 elem ...)]))

(define (ordered-values list-dict)
  (let ((sorted-keys (sort (hash-keys list-dict) <)))
    (map (lambda (x) (hash-ref list-dict x)) sorted-keys)))


(define (prnt-rkt container side)
  (cond
    [(py-list? container)
      (if (equal? "l" side)
          (display "[")
          (display "]"))]
    [(set? container)
      (if (equal? "l" side)
          (display "{")
          (display "}"))]
    [(tuple? container)
     (if (equal? "l" side)
          (display "(")
          (display ")"))
     ]
    
    ))
      
;; recursively print out hte list?
(define (recur-prnt-pylst pylst)
 
  (let (
        ;(lst (mlist->list (py-list-mlist  pylst)))
        (lst (ordered-values (dict-ref pylst "__containerdict__")))
        )
     (let ((flag #f))        
       (for ([elem lst])
               (if (or (py-list? elem) (set? elem) (tuple? elem))
                   
                   (begin 
                     (if (= (length (member elem lst)) 1)
                         (begin
                            (prnt-rkt elem "l");(display "[")
                            (recur-prnt-pylst elem)
                            (prnt-rkt elem "r");(display "]")
                            )
                         (begin
                           (prnt-rkt elem "l") ;(display "[")
                           (recur-prnt-pylst elem)
                           (prnt-rkt elem "r") ;(display "]")
                           (display ", ")
                           )))
                   (begin 
                     (if (= (length (member elem lst)) 1)
                         (begin
                           (display elem)
                           (prnt-rkt elem "r") ;(display "]")
                           )
                         (begin 
                           (if (equal? (length (member elem lst)) (length lst)) ; this line alone can't decide whether elem is the first element!
                               (begin
                                 (if (not flag) ; indeed the first one
                                     (begin
                                       (prnt-rkt elem "l") ;(display "[")
                                       (display elem)
                                       (display ", ")
                                       (set! flag #t))
                                     (begin   ; duplicate one but not the first one
                                       (display elem)
                                       (display ", ")
                                       (set! flag #f))))
                                     
                               (begin
                                 (display elem)
                                 (display ", ")))))))))))


              
      
      



; -- 
;(define (test-class-dict obj key)
;  (let ((class-dict (string->symbol (string-append  (hash-ref obj "__class__") "_dict"))))
;    (display (format "CLass-dict: ~a-------- " class-dict))
;    (display (hash? class-dict))))


(program
 (define gbl-dict (void))
 (define object (void))
 (define List (void))
 (define set (void))
 (define tuple (void))
 (define b1 (void))
 (define foo (void))
 (define b (void))
 (define b2 (void))
 (define bar (void))
 (set-global! object (lambda () (void)))
 (set-global!
  List
  (lambda (tmp-dict18)
    (call/ec
     (lambda (return)
       (let (($localo19
              (dict
               ("__class__" (get-global List))
               ("__type__" "instPyList"))))
         ((get-field $localo19 "__init__") tmp-dict18)
         (return $localo19))))))
 (set-global!
  set
  (lambda (tmp-dict21)
    (call/ec
     (lambda (return)
       (let (($localo22
              (dict ("__class__" (get-global set)) ("__type__" "instSet"))))
         ((get-field $localo22 "__init__") tmp-dict21)
         (return $localo22))))))
 (set-global!
  tuple
  (lambda (tmp-dict24)
    (call/ec
     (lambda (return)
       (let (($localo25
              (dict
               ("__class__" (get-global tuple))
               ("__type__" "instTuple"))))
         ((get-field $localo25 "__init__") tmp-dict24)
         (return $localo25))))))
 (set-global! gbl-dict (dict))
 (dict-set!
  gbl-dict
  object
  (dict
   ("__base__" (list))
   ("__mro__" (list object))
   ("__init__" (lambda v void))))
 (dict-set!
  gbl-dict
  List
  (dict
   ("__base__" (list object))
   ("__mro__" (list List object))
   ("__init__"
    (lambda (self26 tmpdict27)
      (call/ec
       (lambda (return)
         (if (py-list? tmpdict27)
           (set-field!
            self26
            "__containerdict__"
            (dict-ref tmpdict27 "__containerdict__"))
           (set-field! self26 "__containerdict__" tmpdict27))))))))
 (dict-set!
  gbl-dict
  set
  (dict
   ("__base__" (list object))
   ("__mro__" (list set object))
   ("__init__"
    (lambda (self28 tmpdict29)
      (call/ec
       (lambda (return)
         (if (py-list? tmpdict29)
           (set-field!
            self28
            "__containerdict__"
            (dict-ref tmpdict29 "__containerdict__"))
           (set-field! self28 "__containerdict__" tmpdict29))))))))
 (dict-set!
  gbl-dict
  tuple
  (dict
   ("__base__" (list object))
   ("__mro__" (list tuple object))
   ("__init__"
    (lambda (self30 tmp-dict31)
      (call/ec
       (lambda (return)
         (if (or (py-list? tmp-dict31) (tuple? tmp-dict31) (set? tmp-dict31))
           (set-field!
            self30
            "__containerdict__"
            (dict-ref tmp-dict31 "__containerdict__"))
           (set-field! self30 "__containerdict__" tmp-dict31))))))))
 (set-global!
  foo
  (lambda ()
    (call/ec
     (lambda (return)
       (let () (return (dict ("a" 1) ("b" "special") ("c" 3))))))))
 (set-global!
  bar
  (lambda ()
    (call/ec
     (lambda (return)
       (let ()
         (return ((get-global tuple) (dict (0 ((get-global foo))) (1 1)))))))))
 (set-global! b ((get-global bar)))
 (set-global!
  b1
  (let ((e11 (get-global b)))
    (let ((i12 1))
      (cond
       ((py-list? e11) (py-list-ref e11 i12))
       ((tuple? e11) (tuple-ref e11 i12))
       ((dict? e11) (dict-ref e11 i12))
       (else (error "cannot index object"))))))
 (set-global!
  b2
  (let ((e15
         (let ((e13 (get-global b)))
           (let ((i14 0))
             (cond
              ((py-list? e13) (py-list-ref e13 i14))
              ((tuple? e13) (tuple-ref e13 i14))
              ((dict? e13) (dict-ref e13 i14))
              (else (error "cannot index object")))))))
    (let ((i16 "b"))
      (cond
       ((py-list? e15) (py-list-ref e15 i16))
       ((tuple? e15) (tuple-ref e15 i16))
       ((dict? e15) (dict-ref e15 i16))
       (else (error "cannot index object")))))))
