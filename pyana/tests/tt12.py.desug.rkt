#lang racket

(require racket/mpair)

(define-syntax program
  (syntax-rules ()
    [(_ body ...) 
     (begin body ...)]))
    ; (let () body ...)]))



;; Data structures.
(define-syntax dict
  (syntax-rules ()
    [(_ (k v) ...)
     ; =>
     (make-hash (list (cons k v) ...))]))

;(define object (make-hash (list (cons "__base__" (list)))))
;(define object (lambda () (void)))

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
;
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

;     
;(define-struct py-list ([mlist #:mutable]))
;
;(define (py-list-set! pl i val)
;  (mlist-set! (py-list-mlist pl) i val))
;
;(define (py-list-ref pl i)
;  (mlist-ref (py-list-mlist pl) i))
;
;(define (py-list-remove! pl i)
;  (cond
;    [(< i 0)  (error "index out of bounds for removal")]
;    [(= i 0)  (set-py-list-mlist! (mcdr (py-list-mlist pl)))]
;    [else     (mlist-remove! (py-list-mlist pl) i)]))
;     
;(define (py-list* . args)
;  (py-list (list->mlist args)))





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
(define (len container)
   ;(lambda (container)
       (let ((i 0))
         (for  ([(k v) (dict-ref container "__containerdict__")])
           (set! i (+ i 1)))
         i));)






;; Iterators.
;(define (for-set set f)
;  (for ([v set])
;    (f v)))
;
;(define (for-tuple tuple f)
;  (for ([v tuple])
;    (f v)))
;
;
;
;
;(define (for-py-list lst f)
;  (for ([v  (ordered-values (dict-ref lst "__containerdict__"))])
;    (f v)))
;
;(define (for-dict dict f)
;  (for ([(v _) dict])
;    (f v)))

(define (for-container base f)
  (if (or (py-list? base) (set? base) (tuple? base))
      (for ([v  (ordered-values (dict-ref base "__containerdict__"))])
         (f v))
      (for ([(v _) base])
        (f v)))) 


;; Objects.
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
                                  (lambda v (apply field-val obj v))
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
;
;
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
  
;(define/return (in? needle haystack)
;  (cond
;    [(hash? haystack)     (for ([(x y) haystack])
;                            (when (equal? x needle)
;                              (return #t)))]
;    [(py-list? haystack)  (return (in? needle (py-list-mlist haystack)))]
;    [else                 (for ([x haystack])
;                            (when (equal? x needle) 
;                              (return #t)))])
;  #f)

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



;; Library functions.

(define bitwise-or bitwise-ior)
;
;(define (py-object->string o)
;  
;  (define (commas seq)
;    (define first? #t)
;    (define ans "")
;    (for ([c seq])
;      (when (not first?)
;        (set! ans (string-append ans ", ")))
;      (when first?
;        (set! first? #f))
;      (set! ans (string-append ans (py-object->string c))))
;    ans)
;    
;  (define (keyvals seq)
;    (define first? #t)
;    (define ans "")
;    (for ([(k v) seq])
;      (when (not first?)
;        (set! ans (string-append ans ", ")))
;      (when first?
;        (set! first? #f))
;      (set! ans (string-append ans (py-object->string k) ": " (py-object->string v))))
;    ans)
;    
;  
;  (cond
;    [(py-list? o)   (format "[~a]" (commas (py-list-mlist o)))]
;    [(tuple? o)     (format "(~a)" (commas o))]
;    [(dict? o)      (format "{~a}" (keyvals o))]
;    [(string? o)    (format "~v" o)] 
;    [else           (format "~a" o)]))

;(define (py-print x) 
;  (cond 
;    [(string? x)  (display x)]
;    [else         (display (py-object->string x))])
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

(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define gbl-dict (void))
 (define object (void))
 (define List (void))
 (define set (void))
 (define tuple (void))
 (define g$b (void))
 (define g$g (void))
 (define g$a (void))
 (define g$c (void))
 (define g$f (void))
 (begin
   (set! object (lambda () (void)))
   (set! List
     (lambda (tmp-dict20)
       (call/ec
        (lambda (return)
          (let (($localo21
                 (dict ("__class__" List) ("__type__" "instPyList"))))
            (begin
              ((get-field $localo21 "__init__") tmp-dict20)
              (return $localo21)))))))
   (set! set
     (lambda (tmp-dict23)
       (call/ec
        (lambda (return)
          (let (($localo24 (dict ("__class__" set) ("__type__" "instSet"))))
            (begin
              ((get-field $localo24 "__init__") tmp-dict23)
              (return $localo24)))))))
   (set! tuple
     (lambda (tmp-dict26)
       (call/ec
        (lambda (return)
          (let (($localo27
                 (dict ("__class__" tuple) ("__type__" "instTuple"))))
            (begin
              ((get-field $localo27 "__init__") tmp-dict26)
              (return $localo27)))))))
   (set! gbl-dict (dict))
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
    set
    (dict
     ("__base__" (list object))
     ("__mro__" (list set object))
     ("__init__"
      (lambda (self30 tmpdict31)
        (call/ec
         (lambda (return)
           (if (py-list? tmpdict31)
             (set-field!
              self30
              "__containerdict__"
              (dict-ref tmpdict31 "__containerdict__"))
             (set-field! self30 "__containerdict__" tmpdict31))))))))
   (dict-set!
    gbl-dict
    tuple
    (dict
     ("__base__" (list object))
     ("__mro__" (list tuple object))
     ("__init__"
      (lambda (self32 tmp-dict33)
        (call/ec
         (lambda (return)
           (if (let ((t11 (py-list? tmp-dict33)))
                 (begin
                   (if t11
                     t11
                     (let ((t12 (tuple? tmp-dict33)))
                       (begin (if t12 t12 (set? tmp-dict33)))))))
             (set-field!
              self32
              "__containerdict__"
              (dict-ref tmp-dict33 "__containerdict__"))
             (set-field! self32 "__containerdict__" tmp-dict33))))))))
   (set! g$f
     (lambda ()
       (call/ec (lambda (return) (begin (py-print "called f") (return 1))))))
   (set! g$g
     (lambda ()
       (call/ec (lambda (return) (begin (py-print "called g") (return 0))))))
   (set! g$a
     (List
      (dict
       (0 (List (dict (0 10) (1 20))))
       (1 (List (dict (0 30) (1 40))))
       (2 (List (dict (0 50) (1 60)))))))
   (set! g$b
     (let ((e13
            (let ((e11 g$a))
              (begin
                (let ((i12 (g$f)))
                  (begin
                    (if (py-list? e11)
                      (py-list-ref e11 i12)
                      (if (tuple? e11)
                        (tuple-ref e11 i12)
                        (if (dict? e11)
                          (dict-ref e11 i12)
                          (error "cannot index object"))))))))))
       (begin
         (let ((i14 (g$g)))
           (begin
             (if (py-list? e13)
               (py-list-ref e13 i14)
               (if (tuple? e13)
                 (tuple-ref e13 i14)
                 (if (dict? e13)
                   (dict-ref e13 i14)
                   (error "cannot index object")))))))))
   (set! g$c
     (+
      g$b
      (let ((e17
             (let ((e15 g$a))
               (begin
                 (let ((i16 (g$g)))
                   (begin
                     (if (py-list? e15)
                       (py-list-ref e15 i16)
                       (if (tuple? e15)
                         (tuple-ref e15 i16)
                         (if (dict? e15)
                           (dict-ref e15 i16)
                           (error "cannot index object"))))))))))
        (begin
          (let ((i18 (g$f)))
            (begin
              (if (py-list? e17)
                (py-list-ref e17 i18)
                (if (tuple? e17)
                  (tuple-ref e17 i18)
                  (if (dict? e17)
                    (dict-ref e17 i18)
                    (error "cannot index object"))))))))))))
