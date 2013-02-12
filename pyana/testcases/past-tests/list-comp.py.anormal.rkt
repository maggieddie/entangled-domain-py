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



(define (get-precessors cls-lst)
  (append-map (λ (x)  (hash-ref (hash-ref gbl-dict x) "__base__")) cls-lst))

(define (cflt class-dict fn)
  (if (hash-has-key? class-dict fn) #t #f))

(define (search-field classes field-name flt)
  (let* ((cls-dicts (map (λ (x) (hash-ref gbl-dict x)) classes))
        (res (filter (λ (x) (flt x field-name)) cls-dicts)))
       (if (not (empty? res))
           (hash-ref (car res) field-name)
           (let ((supers (get-precessors classes)))
             (if (empty? supers)
                 #f
                 (search-field supers field-name flt))))))
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
  (define (mro cls)
    (let ((res 
           (append (list cls)
                   (merge
                    (append 
                     (map (lambda (x) (mro x)) (hash-ref (hash-ref gbl-dict cls) "__base__"))
                     (list (hash-ref (hash-ref gbl-dict cls) "__base__")))
                    (list)
                    ))))
      ;(hash-set! gbl-dict "__mro__" res)
      res
      ))



(define (merge lst-of-parentlist res)
  
  (if (empty-lists? lst-of-parentlist)
      ;(begin 
       ; (display lst-of-parentlist)
        res
        ;)
      (let ((ctl 0)) ; everytime new round, then ctl is set to 0
        (let iter (;[tmp-lst lst-of-parentlist]
                   [i ctl])
          (if (= i (length lst-of-parentlist))
              (begin 
                ;(display i)
                #f)
              (begin
                ;(display lst-of-parentlist)
                ;(newline)
                (let* ((cur-lst (list-ref lst-of-parentlist i)) 
                       ;(cand (car cur-lst))
                       )
                  (if (empty? cur-lst)
                      (iter (add1 i))
                      (let ((cand (car cur-lst)))
                        (if (in-tail? cand (remove cur-lst lst-of-parentlist))
                            (begin
                    
                              (iter (add1 i)))
                      ; a good header.
                            (let ((temp-res (append res (list cand)))
                                  (new-lsts (map 
                                             (lambda (x)
                                               (if (not (member cand x))
                                                   x
                                                   (if (equal? cand (car x))
                                                       (drop x 1)
                                                       x)))
                                             lst-of-parentlist)))
                              (merge new-lsts temp-res))))))))) )))

(define (flt cand lst)
  (if (empty? lst)
     ;
      #f
      (if (not (member cand (drop lst 1)))  
      ; good cands
          #f 
      ; cand in the tail of the lst
          #t)))
      

(define (in-tail? cand others)
  (let ((res (filter 
              (lambda (x) (flt cand x))
              others)))
    (if (empty? res)
        #f
        #t)))

(define (empty-lists? lsts)
  (let ((res
         (filter (lambda (x)  (if (empty? x) #f #t)) lsts)))
    (if (empty? res) #t #f)))

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
 (define g$freshfruit (void))
 (define g$vec (void))
 (let ((b0 (lambda () (void))))
   (let ((_1 (set! object b0)))
     (let ((_2 (void)))
       (let ((b6
              (lambda (tmp-dict)
                (call/ec
                 (lambda (return)
                   (let (($localo
                          (dict ("__class__" List) ("__type__" "instPyList"))))
                     (let ((b3 (get-field $localo "__init__")))
                       (let ((_4 (b3 tmp-dict)))
                         (let ((b5 (return $localo))) b5)))))))))
         (let ((_7 (set! List b6)))
           (let ((_8 (void)))
             (let ((b12
                    (lambda (tmp-dict)
                      (call/ec
                       (lambda (return)
                         (let (($localo
                                (dict
                                 ("__class__" set)
                                 ("__type__" "instSet"))))
                           (let ((b9 (get-field $localo "__init__")))
                             (let ((_10 (b9 tmp-dict)))
                               (let ((b11 (return $localo))) b11)))))))))
               (let ((_13 (set! set b12)))
                 (let ((_14 (void)))
                   (let ((b18
                          (lambda (tmp-dict)
                            (call/ec
                             (lambda (return)
                               (let (($localo
                                      (dict
                                       ("__class__" tuple)
                                       ("__type__" "instTuple"))))
                                 (let ((b15 (get-field $localo "__init__")))
                                   (let ((_16 (b15 tmp-dict)))
                                     (let ((b17 (return $localo))) b17)))))))))
                     (let ((_19 (set! tuple b18)))
                       (let ((_20 (void)))
                         (let ((b21 (list object)))
                           (let ((b22 (lambda v void)))
                             (let ((b23
                                    (dict
                                     ("__base__" (list))
                                     ("__mro__" b21)
                                     ("__init__" b22))))
                               (let ((b24 (list object)))
                                 (let ((b25 (list List object)))
                                   (let ((b28
                                          (lambda (self tmp-list-dict)
                                            (call/ec
                                             (lambda (return)
                                               (let ((b26
                                                      (py-list?
                                                       tmp-list-dict)))
                                                 (if b26
                                                   (let ((b27
                                                          (dict-ref
                                                           tmp-list-dict
                                                           "__containerdict__")))
                                                     (set-field!
                                                      self
                                                      "__containerdict__"
                                                      b27))
                                                   (set-field!
                                                    self
                                                    "__containerdict__"
                                                    tmp-list-dict))))))))
                                     (let ((b52
                                            (lambda (self elem)
                                              (let ((container
                                                     (dict-ref
                                                      self
                                                      "__containerdict__")))
                                                (let ((i 0))
                                                  (let ((found #f))
                                                    (let ((max (len self)))
                                                      (let ((_48
                                                             (call/ec
                                                              (lambda (break)
                                                                (let ((loop
                                                                       (void)))
                                                                  (let ((b43
                                                                         (lambda ()
                                                                           (let ((b29
                                                                                  (<=
                                                                                   0
                                                                                   i)))
                                                                             (let ((b30
                                                                                    (if b29
                                                                                      (<
                                                                                       i
                                                                                       max)
                                                                                      #f)))
                                                                               (if b30
                                                                                 (let ((_41
                                                                                        (call/ec
                                                                                         (lambda (continue)
                                                                                           (let ((b31
                                                                                                  (dict-ref
                                                                                                   container
                                                                                                   i)))
                                                                                             (let ((b32
                                                                                                    (equal?
                                                                                                     b31
                                                                                                     elem)))
                                                                                               (if b32
                                                                                                 (let ((b33
                                                                                                        (not
                                                                                                         found)))
                                                                                                   (let ((_35
                                                                                                          (if b33
                                                                                                            (let ((_34
                                                                                                                   (set! found
                                                                                                                     i)))
                                                                                                              (void))
                                                                                                            (void))))
                                                                                                     (let ((b37
                                                                                                            (+
                                                                                                             i
                                                                                                             1)))
                                                                                                       (let ((_38
                                                                                                              (set! i
                                                                                                                b37)))
                                                                                                         (let ((b36
                                                                                                                (void)))
                                                                                                           b36)))))
                                                                                                 (let ((b39
                                                                                                        (+
                                                                                                         i
                                                                                                         1)))
                                                                                                   (let ((_40
                                                                                                          (set! i
                                                                                                            b39)))
                                                                                                     (void))))))))))
                                                                                   (let ((b42
                                                                                          (loop)))
                                                                                     b42))
                                                                                 (void)))))))
                                                                    (let ((_44
                                                                           (set! loop
                                                                             b43)))
                                                                      (let ((_45
                                                                             (void)))
                                                                        (let ((_46
                                                                               (loop)))
                                                                          (let ((b47
                                                                                 (void)))
                                                                            b47))))))))))
                                                        (let ((b50
                                                               (not found)))
                                                          (let ((b49
                                                                 (if b50
                                                                   (let ((b51
                                                                          (format
                                                                           "no such elem: ~a in list ~a"
                                                                           elem
                                                                           container)))
                                                                     (error
                                                                      b51))
                                                                   found)))
                                                            b49))))))))))
                                       (let ((b74
                                              (lambda (self elem)
                                                (let ((container
                                                       (dict-ref
                                                        self
                                                        "__containerdict__")))
                                                  (let ((i 0))
                                                    (let ((cnt 0))
                                                      (let ((max (len self)))
                                                        (let ((_72
                                                               (call/ec
                                                                (lambda (break)
                                                                  (let ((loop
                                                                         (void)))
                                                                    (let ((b67
                                                                           (lambda ()
                                                                             (let ((b53
                                                                                    (<=
                                                                                     0
                                                                                     i)))
                                                                               (let ((b54
                                                                                      (if b53
                                                                                        (<
                                                                                         i
                                                                                         max)
                                                                                        #f)))
                                                                                 (if b54
                                                                                   (let ((_65
                                                                                          (call/ec
                                                                                           (lambda (continue)
                                                                                             (let ((b55
                                                                                                    (dict-ref
                                                                                                     container
                                                                                                     i)))
                                                                                               (let ((b56
                                                                                                      (equal?
                                                                                                       b55
                                                                                                       elem)))
                                                                                                 (if b56
                                                                                                   (let ((b57
                                                                                                          (+
                                                                                                           cnt
                                                                                                           1)))
                                                                                                     (let ((_58
                                                                                                            (set! cnt
                                                                                                              b57)))
                                                                                                       (let ((_59
                                                                                                              (void)))
                                                                                                         (let ((b61
                                                                                                                (+
                                                                                                                 i
                                                                                                                 1)))
                                                                                                           (let ((_62
                                                                                                                  (set! i
                                                                                                                    b61)))
                                                                                                             (let ((b60
                                                                                                                    (void)))
                                                                                                               b60))))))
                                                                                                   (let ((b63
                                                                                                          (+
                                                                                                           i
                                                                                                           1)))
                                                                                                     (let ((_64
                                                                                                            (set! i
                                                                                                              b63)))
                                                                                                       (void))))))))))
                                                                                     (let ((b66
                                                                                            (loop)))
                                                                                       b66))
                                                                                   (void)))))))
                                                                      (let ((_68
                                                                             (set! loop
                                                                               b67)))
                                                                        (let ((_69
                                                                               (void)))
                                                                          (let ((_70
                                                                                 (loop)))
                                                                            (let ((b71
                                                                                   (void)))
                                                                              b71))))))))))
                                                          (let ((b73 cnt))
                                                            b73)))))))))
                                         (let ((b77
                                                (lambda (self elem)
                                                  (let ((max (len self)))
                                                    (let ((b76
                                                           (dict-ref
                                                            self
                                                            "__containerdict__")))
                                                      (let ((b75
                                                             (dict-set!
                                                              b76
                                                              max
                                                              elem)))
                                                        b75))))))
                                           (let ((b114
                                                  (lambda (self index elem)
                                                    (let ((b78 (= index 0)))
                                                      (if b78
                                                        (let ((i 0))
                                                          (let ((container
                                                                 (dict-ref
                                                                  self
                                                                  "__containerdict__")))
                                                            (let ((_88
                                                                   (call/ec
                                                                    (lambda (break)
                                                                      (let ((b82
                                                                             (lambda ($seq11
                                                                                      $loop12)
                                                                               (let ((_80
                                                                                      (for-container
                                                                                       $seq11
                                                                                       $loop12)))
                                                                                 (let ((b81
                                                                                        (void)))
                                                                                   (let ((b79
                                                                                          b81))
                                                                                     b79))))))
                                                                        (let ((b87
                                                                               (lambda (e)
                                                                                 (call/ec
                                                                                  (lambda (continue)
                                                                                    (let ((b83
                                                                                           (+
                                                                                            i
                                                                                            1)))
                                                                                      (let ((_84
                                                                                             (set! i
                                                                                               b83)))
                                                                                        (let ((_85
                                                                                               (void)))
                                                                                          (let ((b86
                                                                                                 (dict-set!
                                                                                                  container
                                                                                                  i
                                                                                                  e)))
                                                                                            b86)))))))))
                                                                          (b82
                                                                           self
                                                                           b87)))))))
                                                              (let ((b89
                                                                     (dict-set!
                                                                      container
                                                                      0
                                                                      elem)))
                                                                b89))))
                                                        (let ((b90 (len self)))
                                                          (let ((b91
                                                                 (>=
                                                                  index
                                                                  b90)))
                                                            (if b91
                                                              (let ((tail
                                                                     (len
                                                                      self)))
                                                                (let ((container
                                                                       (dict-ref
                                                                        self
                                                                        "__containerdict__")))
                                                                  (let ((b92
                                                                         (dict-set!
                                                                          container
                                                                          tail
                                                                          elem)))
                                                                    b92)))
                                                              (let ((b93
                                                                     (>
                                                                      index
                                                                      0)))
                                                                (let ((b95
                                                                       (if b93
                                                                         (let ((b94
                                                                                (len
                                                                                 self)))
                                                                           (<
                                                                            index
                                                                            b94))
                                                                         #f)))
                                                                  (if b95
                                                                    (let ((max
                                                                           (len
                                                                            self)))
                                                                      (let ((container
                                                                             (dict-ref
                                                                              self
                                                                              "__containerdict__")))
                                                                        (let ((i
                                                                               max))
                                                                          (let ((_112
                                                                                 (call/ec
                                                                                  (lambda (break)
                                                                                    (let ((loop
                                                                                           (void)))
                                                                                      (let ((b107
                                                                                             (lambda ()
                                                                                               (let ((b97
                                                                                                      (>=
                                                                                                       max
                                                                                                       i)))
                                                                                                 (let ((b98
                                                                                                        (if b97
                                                                                                          (>
                                                                                                           i
                                                                                                           index)
                                                                                                          #f)))
                                                                                                   (if b98
                                                                                                     (let ((_105
                                                                                                            (call/ec
                                                                                                             (lambda (continue)
                                                                                                               (let ((b99
                                                                                                                      (-
                                                                                                                       i
                                                                                                                       1)))
                                                                                                                 (let ((b100
                                                                                                                        (dict-ref
                                                                                                                         container
                                                                                                                         b99)))
                                                                                                                   (let ((_101
                                                                                                                          (dict-set!
                                                                                                                           container
                                                                                                                           i
                                                                                                                           b100)))
                                                                                                                     (let ((b103
                                                                                                                            (-
                                                                                                                             i
                                                                                                                             1)))
                                                                                                                       (let ((_104
                                                                                                                              (set! i
                                                                                                                                b103)))
                                                                                                                         (let ((b102
                                                                                                                                (void)))
                                                                                                                           b102))))))))))
                                                                                                       (let ((b106
                                                                                                              (loop)))
                                                                                                         b106))
                                                                                                     (void)))))))
                                                                                        (let ((_108
                                                                                               (set! loop
                                                                                                 b107)))
                                                                                          (let ((_109
                                                                                                 (void)))
                                                                                            (let ((_110
                                                                                                   (loop)))
                                                                                              (let ((b111
                                                                                                     (void)))
                                                                                                b111))))))))))
                                                                            (let ((b113
                                                                                   (dict-set!
                                                                                    container
                                                                                    index
                                                                                    elem)))
                                                                              (let ((b96
                                                                                     b113))
                                                                                b96))))))
                                                                    (void))))))))))))
                                             (let ((b115
                                                    (dict
                                                     ("__base__" b24)
                                                     ("__mro__" b25)
                                                     ("__init__" b28)
                                                     ("index" b52)
                                                     ("count" b74)
                                                     ("append" b77)
                                                     ("insert" b114)
                                                     ("reverse" (void))
                                                     ("sort" (void))
                                                     ("pop" (void))
                                                     ("extend" (void)))))
                                               (let ((b116 (list object)))
                                                 (let ((b117
                                                        (list set object)))
                                                   (let ((b120
                                                          (lambda (self
                                                                   tmp-list-dict)
                                                            (call/ec
                                                             (lambda (return)
                                                               (let ((b118
                                                                      (py-list?
                                                                       tmp-list-dict)))
                                                                 (if b118
                                                                   (let ((b119
                                                                          (dict-ref
                                                                           tmp-list-dict
                                                                           "__containerdict__")))
                                                                     (set-field!
                                                                      self
                                                                      "__containerdict__"
                                                                      b119))
                                                                   (set-field!
                                                                    self
                                                                    "__containerdict__"
                                                                    tmp-list-dict))))))))
                                                     (let ((b121
                                                            (dict
                                                             ("__base__" b116)
                                                             ("__mro__" b117)
                                                             ("__init__"
                                                              b120))))
                                                       (let ((b122
                                                              (list object)))
                                                         (let ((b123
                                                                (list
                                                                 tuple
                                                                 object)))
                                                           (let ((b127
                                                                  (lambda (self
                                                                           tmp-dict)
                                                                    (call/ec
                                                                     (lambda (return)
                                                                       (let ((t13
                                                                              (py-list?
                                                                               tmp-dict)))
                                                                         (let ((b124
                                                                                (if t13
                                                                                  t13
                                                                                  (let ((t14
                                                                                         (tuple?
                                                                                          tmp-dict)))
                                                                                    (let ((b125
                                                                                           (if t14
                                                                                             t14
                                                                                             (set?
                                                                                              tmp-dict))))
                                                                                      b125)))))
                                                                           (if b124
                                                                             (let ((b126
                                                                                    (dict-ref
                                                                                     tmp-dict
                                                                                     "__containerdict__")))
                                                                               (set-field!
                                                                                self
                                                                                "__containerdict__"
                                                                                b126))
                                                                             (set-field!
                                                                              self
                                                                              "__containerdict__"
                                                                              tmp-dict)))))))))
                                                             (let ((b151
                                                                    (lambda (self
                                                                             elem)
                                                                      (let ((container
                                                                             (dict-ref
                                                                              self
                                                                              "__containerdict__")))
                                                                        (let ((i
                                                                               0))
                                                                          (let ((found
                                                                                 #f))
                                                                            (let ((max
                                                                                   (len
                                                                                    self)))
                                                                              (let ((_147
                                                                                     (call/ec
                                                                                      (lambda (break)
                                                                                        (let ((loop
                                                                                               (void)))
                                                                                          (let ((b142
                                                                                                 (lambda ()
                                                                                                   (let ((b128
                                                                                                          (<=
                                                                                                           0
                                                                                                           i)))
                                                                                                     (let ((b129
                                                                                                            (if b128
                                                                                                              (<
                                                                                                               i
                                                                                                               max)
                                                                                                              #f)))
                                                                                                       (if b129
                                                                                                         (let ((_140
                                                                                                                (call/ec
                                                                                                                 (lambda (continue)
                                                                                                                   (let ((b130
                                                                                                                          (dict-ref
                                                                                                                           container
                                                                                                                           i)))
                                                                                                                     (let ((b131
                                                                                                                            (equal?
                                                                                                                             b130
                                                                                                                             elem)))
                                                                                                                       (if b131
                                                                                                                         (let ((b132
                                                                                                                                (not
                                                                                                                                 found)))
                                                                                                                           (let ((_134
                                                                                                                                  (if b132
                                                                                                                                    (let ((_133
                                                                                                                                           (set! found
                                                                                                                                             i)))
                                                                                                                                      (void))
                                                                                                                                    (void))))
                                                                                                                             (let ((b136
                                                                                                                                    (+
                                                                                                                                     i
                                                                                                                                     1)))
                                                                                                                               (let ((_137
                                                                                                                                      (set! i
                                                                                                                                        b136)))
                                                                                                                                 (let ((b135
                                                                                                                                        (void)))
                                                                                                                                   b135)))))
                                                                                                                         (let ((b138
                                                                                                                                (+
                                                                                                                                 i
                                                                                                                                 1)))
                                                                                                                           (let ((_139
                                                                                                                                  (set! i
                                                                                                                                    b138)))
                                                                                                                             (void))))))))))
                                                                                                           (let ((b141
                                                                                                                  (loop)))
                                                                                                             b141))
                                                                                                         (void)))))))
                                                                                            (let ((_143
                                                                                                   (set! loop
                                                                                                     b142)))
                                                                                              (let ((_144
                                                                                                     (void)))
                                                                                                (let ((_145
                                                                                                       (loop)))
                                                                                                  (let ((b146
                                                                                                         (void)))
                                                                                                    b146))))))))))
                                                                                (let ((b149
                                                                                       (not
                                                                                        found)))
                                                                                  (let ((b148
                                                                                         (if b149
                                                                                           (let ((b150
                                                                                                  (format
                                                                                                   "no such elem: ~a in list ~a"
                                                                                                   elem
                                                                                                   container)))
                                                                                             (error
                                                                                              b150))
                                                                                           found)))
                                                                                    b148))))))))))
                                                               (let ((b173
                                                                      (lambda (self
                                                                               elem)
                                                                        (let ((container
                                                                               (dict-ref
                                                                                self
                                                                                "__containerdict__")))
                                                                          (let ((i
                                                                                 0))
                                                                            (let ((cnt
                                                                                   0))
                                                                              (let ((max
                                                                                     (len
                                                                                      self)))
                                                                                (let ((_171
                                                                                       (call/ec
                                                                                        (lambda (break)
                                                                                          (let ((loop
                                                                                                 (void)))
                                                                                            (let ((b166
                                                                                                   (lambda ()
                                                                                                     (let ((b152
                                                                                                            (<=
                                                                                                             0
                                                                                                             i)))
                                                                                                       (let ((b153
                                                                                                              (if b152
                                                                                                                (<
                                                                                                                 i
                                                                                                                 max)
                                                                                                                #f)))
                                                                                                         (if b153
                                                                                                           (let ((_164
                                                                                                                  (call/ec
                                                                                                                   (lambda (continue)
                                                                                                                     (let ((b154
                                                                                                                            (dict-ref
                                                                                                                             container
                                                                                                                             i)))
                                                                                                                       (let ((b155
                                                                                                                              (equal?
                                                                                                                               b154
                                                                                                                               elem)))
                                                                                                                         (if b155
                                                                                                                           (let ((b156
                                                                                                                                  (+
                                                                                                                                   cnt
                                                                                                                                   1)))
                                                                                                                             (let ((_157
                                                                                                                                    (set! cnt
                                                                                                                                      b156)))
                                                                                                                               (let ((_158
                                                                                                                                      (void)))
                                                                                                                                 (let ((b160
                                                                                                                                        (+
                                                                                                                                         i
                                                                                                                                         1)))
                                                                                                                                   (let ((_161
                                                                                                                                          (set! i
                                                                                                                                            b160)))
                                                                                                                                     (let ((b159
                                                                                                                                            (void)))
                                                                                                                                       b159))))))
                                                                                                                           (let ((b162
                                                                                                                                  (+
                                                                                                                                   i
                                                                                                                                   1)))
                                                                                                                             (let ((_163
                                                                                                                                    (set! i
                                                                                                                                      b162)))
                                                                                                                               (void))))))))))
                                                                                                             (let ((b165
                                                                                                                    (loop)))
                                                                                                               b165))
                                                                                                           (void)))))))
                                                                                              (let ((_167
                                                                                                     (set! loop
                                                                                                       b166)))
                                                                                                (let ((_168
                                                                                                       (void)))
                                                                                                  (let ((_169
                                                                                                         (loop)))
                                                                                                    (let ((b170
                                                                                                           (void)))
                                                                                                      b170))))))))))
                                                                                  (let ((b172
                                                                                         cnt))
                                                                                    b172)))))))))
                                                                 (let ((b174
                                                                        (dict
                                                                         ("__base__"
                                                                          b122)
                                                                         ("__mro__"
                                                                          b123)
                                                                         ("__init__"
                                                                          b127)
                                                                         ("index"
                                                                          b151)
                                                                         ("count"
                                                                          b173))))
                                                                   (let ((b175
                                                                          (dict
                                                                           (object
                                                                            b23)
                                                                           (List
                                                                            b115)
                                                                           (set
                                                                            b121)
                                                                           (tuple
                                                                            b174))))
                                                                     (let ((_176
                                                                            (set! gbl-dict
                                                                              b175)))
                                                                       (let ((_177
                                                                              (void)))
                                                                         (let ((b178
                                                                                (dict
                                                                                 (0
                                                                                  2)
                                                                                 (1
                                                                                  4)
                                                                                 (2
                                                                                  6))))
                                                                           (let ((b179
                                                                                  (List
                                                                                   b178)))
                                                                             (let ((_180
                                                                                    (set! g$vec
                                                                                      b179)))
                                                                               (let ((_181
                                                                                      (void)))
                                                                                 (let ((res
                                                                                        (dict)))
                                                                                   (let (($i
                                                                                          0))
                                                                                     (let ((x
                                                                                            (void)))
                                                                                       (let ((_192
                                                                                              (call/ec
                                                                                               (lambda (break)
                                                                                                 (let ((b185
                                                                                                        (lambda ($seq15
                                                                                                                 $loop16)
                                                                                                          (let ((_183
                                                                                                                 (for-container
                                                                                                                  $seq15
                                                                                                                  $loop16)))
                                                                                                            (let ((b184
                                                                                                                   (void)))
                                                                                                              (let ((b182
                                                                                                                     b184))
                                                                                                                b182))))))
                                                                                                   (let ((b191
                                                                                                          (lambda (x)
                                                                                                            (call/ec
                                                                                                             (lambda (continue)
                                                                                                               (let ((b186
                                                                                                                      (*
                                                                                                                       3
                                                                                                                       x)))
                                                                                                                 (let ((_187
                                                                                                                        (dict-set!
                                                                                                                         res
                                                                                                                         $i
                                                                                                                         b186)))
                                                                                                                   (let ((b189
                                                                                                                          (+
                                                                                                                           $i
                                                                                                                           1)))
                                                                                                                     (let ((_190
                                                                                                                            (set! $i
                                                                                                                              b189)))
                                                                                                                       (let ((b188
                                                                                                                              (void)))
                                                                                                                         b188))))))))))
                                                                                                     (b185
                                                                                                      g$vec
                                                                                                      b191)))))))
                                                                                         (let ((b193
                                                                                                res))
                                                                                           (let ((b194
                                                                                                  (List
                                                                                                   b193)))
                                                                                             (let ((_195
                                                                                                    (py-print
                                                                                                     b194)))
                                                                                               (let ((res
                                                                                                      (dict)))
                                                                                                 (let (($i
                                                                                                        0))
                                                                                                   (let ((x
                                                                                                          (void)))
                                                                                                     (let ((_208
                                                                                                            (call/ec
                                                                                                             (lambda (break)
                                                                                                               (let ((b199
                                                                                                                      (lambda ($seq17
                                                                                                                               $loop18)
                                                                                                                        (let ((_197
                                                                                                                               (for-container
                                                                                                                                $seq17
                                                                                                                                $loop18)))
                                                                                                                          (let ((b198
                                                                                                                                 (void)))
                                                                                                                            (let ((b196
                                                                                                                                   b198))
                                                                                                                              b196))))))
                                                                                                                 (let ((b207
                                                                                                                        (lambda (x)
                                                                                                                          (call/ec
                                                                                                                           (lambda (continue)
                                                                                                                             (let ((b200
                                                                                                                                    (expt
                                                                                                                                     x
                                                                                                                                     2)))
                                                                                                                               (let ((b201
                                                                                                                                      (dict
                                                                                                                                       (0
                                                                                                                                        x)
                                                                                                                                       (1
                                                                                                                                        b200))))
                                                                                                                                 (let ((b202
                                                                                                                                        (List
                                                                                                                                         b201)))
                                                                                                                                   (let ((_203
                                                                                                                                          (dict-set!
                                                                                                                                           res
                                                                                                                                           $i
                                                                                                                                           b202)))
                                                                                                                                     (let ((b205
                                                                                                                                            (+
                                                                                                                                             $i
                                                                                                                                             1)))
                                                                                                                                       (let ((_206
                                                                                                                                              (set! $i
                                                                                                                                                b205)))
                                                                                                                                         (let ((b204
                                                                                                                                                (void)))
                                                                                                                                           b204))))))))))))
                                                                                                                   (b199
                                                                                                                    g$vec
                                                                                                                    b207)))))))
                                                                                                       (let ((b209
                                                                                                              res))
                                                                                                         (let ((b210
                                                                                                                (List
                                                                                                                 b209)))
                                                                                                           (let ((_211
                                                                                                                  (py-print
                                                                                                                   b210)))
                                                                                                             (let ((b212
                                                                                                                    (dict
                                                                                                                     (0
                                                                                                                      " banana")
                                                                                                                     (1
                                                                                                                      "  loganberry ")
                                                                                                                     (2
                                                                                                                      "passion fruit  "))))
                                                                                                               (let ((b213
                                                                                                                      (List
                                                                                                                       b212)))
                                                                                                                 (let ((_214
                                                                                                                        (set! g$freshfruit
                                                                                                                          b213)))
                                                                                                                   (let ((_215
                                                                                                                          (void)))
                                                                                                                     (let ((res
                                                                                                                            (dict)))
                                                                                                                       (let (($i
                                                                                                                              0))
                                                                                                                         (let ((weapon
                                                                                                                                (void)))
                                                                                                                           (let ((_225
                                                                                                                                  (call/ec
                                                                                                                                   (lambda (break)
                                                                                                                                     (let ((b219
                                                                                                                                            (lambda ($seq19
                                                                                                                                                     $loop20)
                                                                                                                                              (let ((_217
                                                                                                                                                     (for-container
                                                                                                                                                      $seq19
                                                                                                                                                      $loop20)))
                                                                                                                                                (let ((b218
                                                                                                                                                       (void)))
                                                                                                                                                  (let ((b216
                                                                                                                                                         b218))
                                                                                                                                                    b216))))))
                                                                                                                                       (let ((b224
                                                                                                                                              (lambda (weapon)
                                                                                                                                                (call/ec
                                                                                                                                                 (lambda (continue)
                                                                                                                                                   (let ((_220
                                                                                                                                                          (dict-set!
                                                                                                                                                           res
                                                                                                                                                           $i
                                                                                                                                                           weapon)))
                                                                                                                                                     (let ((b222
                                                                                                                                                            (+
                                                                                                                                                             $i
                                                                                                                                                             1)))
                                                                                                                                                       (let ((_223
                                                                                                                                                              (set! $i
                                                                                                                                                                b222)))
                                                                                                                                                         (let ((b221
                                                                                                                                                                (void)))
                                                                                                                                                           b221)))))))))
                                                                                                                                         (b219
                                                                                                                                          g$freshfruit
                                                                                                                                          b224)))))))
                                                                                                                             (let ((b226
                                                                                                                                    res))
                                                                                                                               (let ((b227
                                                                                                                                      (List
                                                                                                                                       b226)))
                                                                                                                                 (let ((_228
                                                                                                                                        (py-print
                                                                                                                                         b227)))
                                                                                                                                   (let ((res
                                                                                                                                          (dict)))
                                                                                                                                     (let (($i
                                                                                                                                            0))
                                                                                                                                       (let ((x
                                                                                                                                              (void)))
                                                                                                                                         (let ((_240
                                                                                                                                                (call/ec
                                                                                                                                                 (lambda (break)
                                                                                                                                                   (let ((b232
                                                                                                                                                          (lambda ($seq21
                                                                                                                                                                   $loop22)
                                                                                                                                                            (let ((_230
                                                                                                                                                                   (for-container
                                                                                                                                                                    $seq21
                                                                                                                                                                    $loop22)))
                                                                                                                                                              (let ((b231
                                                                                                                                                                     (void)))
                                                                                                                                                                (let ((b229
                                                                                                                                                                       b231))
                                                                                                                                                                  b229))))))
                                                                                                                                                     (let ((b239
                                                                                                                                                            (lambda (x)
                                                                                                                                                              (call/ec
                                                                                                                                                               (lambda (continue)
                                                                                                                                                                 (let ((b233
                                                                                                                                                                        (>
                                                                                                                                                                         x
                                                                                                                                                                         3)))
                                                                                                                                                                   (if b233
                                                                                                                                                                     (let ((b234
                                                                                                                                                                            (*
                                                                                                                                                                             3
                                                                                                                                                                             x)))
                                                                                                                                                                       (let ((_235
                                                                                                                                                                              (dict-set!
                                                                                                                                                                               res
                                                                                                                                                                               $i
                                                                                                                                                                               b234)))
                                                                                                                                                                         (let ((b237
                                                                                                                                                                                (+
                                                                                                                                                                                 $i
                                                                                                                                                                                 1)))
                                                                                                                                                                           (let ((_238
                                                                                                                                                                                  (set! $i
                                                                                                                                                                                    b237)))
                                                                                                                                                                             (let ((b236
                                                                                                                                                                                    (void)))
                                                                                                                                                                               b236)))))
                                                                                                                                                                     (void))))))))
                                                                                                                                                       (b232
                                                                                                                                                        g$vec
                                                                                                                                                        b239)))))))
                                                                                                                                           (let ((b241
                                                                                                                                                  res))
                                                                                                                                             (let ((b242
                                                                                                                                                    (List
                                                                                                                                                     b241)))
                                                                                                                                               (let ((_243
                                                                                                                                                      (py-print
                                                                                                                                                       b242)))
                                                                                                                                                 (let ((res
                                                                                                                                                        (dict)))
                                                                                                                                                   (let (($i
                                                                                                                                                          0))
                                                                                                                                                     (let ((x
                                                                                                                                                            (void)))
                                                                                                                                                       (let ((_255
                                                                                                                                                              (call/ec
                                                                                                                                                               (lambda (break)
                                                                                                                                                                 (let ((b247
                                                                                                                                                                        (lambda ($seq23
                                                                                                                                                                                 $loop24)
                                                                                                                                                                          (let ((_245
                                                                                                                                                                                 (for-container
                                                                                                                                                                                  $seq23
                                                                                                                                                                                  $loop24)))
                                                                                                                                                                            (let ((b246
                                                                                                                                                                                   (void)))
                                                                                                                                                                              (let ((b244
                                                                                                                                                                                     b246))
                                                                                                                                                                                b244))))))
                                                                                                                                                                   (let ((b254
                                                                                                                                                                          (lambda (x)
                                                                                                                                                                            (call/ec
                                                                                                                                                                             (lambda (continue)
                                                                                                                                                                               (let ((b248
                                                                                                                                                                                      (<
                                                                                                                                                                                       x
                                                                                                                                                                                       2)))
                                                                                                                                                                                 (if b248
                                                                                                                                                                                   (let ((b249
                                                                                                                                                                                          (*
                                                                                                                                                                                           3
                                                                                                                                                                                           x)))
                                                                                                                                                                                     (let ((_250
                                                                                                                                                                                            (dict-set!
                                                                                                                                                                                             res
                                                                                                                                                                                             $i
                                                                                                                                                                                             b249)))
                                                                                                                                                                                       (let ((b252
                                                                                                                                                                                              (+
                                                                                                                                                                                               $i
                                                                                                                                                                                               1)))
                                                                                                                                                                                         (let ((_253
                                                                                                                                                                                                (set! $i
                                                                                                                                                                                                  b252)))
                                                                                                                                                                                           (let ((b251
                                                                                                                                                                                                  (void)))
                                                                                                                                                                                             b251)))))
                                                                                                                                                                                   (void))))))))
                                                                                                                                                                     (b247
                                                                                                                                                                      g$vec
                                                                                                                                                                      b254)))))))
                                                                                                                                                         (let ((b256
                                                                                                                                                                res))
                                                                                                                                                           (let ((b257
                                                                                                                                                                  (List
                                                                                                                                                                   b256)))
                                                                                                                                                             (let ((_258
                                                                                                                                                                    (py-print
                                                                                                                                                                     b257)))
                                                                                                                                                               (let ((res
                                                                                                                                                                      (dict)))
                                                                                                                                                                 (let (($i
                                                                                                                                                                        0))
                                                                                                                                                                   (let ((x
                                                                                                                                                                          (void)))
                                                                                                                                                                     (let ((_272
                                                                                                                                                                            (call/ec
                                                                                                                                                                             (lambda (break)
                                                                                                                                                                               (let ((b263
                                                                                                                                                                                      (lambda ($seq25
                                                                                                                                                                                               $loop26)
                                                                                                                                                                                        (let ((_261
                                                                                                                                                                                               (for-container
                                                                                                                                                                                                $seq25
                                                                                                                                                                                                $loop26)))
                                                                                                                                                                                          (let ((b262
                                                                                                                                                                                                 (void)))
                                                                                                                                                                                            (let ((b260
                                                                                                                                                                                                   b262))
                                                                                                                                                                                              b260))))))
                                                                                                                                                                                 (let ((b271
                                                                                                                                                                                        (lambda (x)
                                                                                                                                                                                          (call/ec
                                                                                                                                                                                           (lambda (continue)
                                                                                                                                                                                             (let ((b264
                                                                                                                                                                                                    (expt
                                                                                                                                                                                                     x
                                                                                                                                                                                                     2)))
                                                                                                                                                                                               (let ((b265
                                                                                                                                                                                                      (dict
                                                                                                                                                                                                       (0
                                                                                                                                                                                                        x)
                                                                                                                                                                                                       (1
                                                                                                                                                                                                        b264))))
                                                                                                                                                                                                 (let ((b266
                                                                                                                                                                                                        (tuple
                                                                                                                                                                                                         b265)))
                                                                                                                                                                                                   (let ((_267
                                                                                                                                                                                                          (dict-set!
                                                                                                                                                                                                           res
                                                                                                                                                                                                           $i
                                                                                                                                                                                                           b266)))
                                                                                                                                                                                                     (let ((b269
                                                                                                                                                                                                            (+
                                                                                                                                                                                                             $i
                                                                                                                                                                                                             1)))
                                                                                                                                                                                                       (let ((_270
                                                                                                                                                                                                              (set! $i
                                                                                                                                                                                                                b269)))
                                                                                                                                                                                                         (let ((b268
                                                                                                                                                                                                                (void)))
                                                                                                                                                                                                           b268))))))))))))
                                                                                                                                                                                   (b263
                                                                                                                                                                                    g$vec
                                                                                                                                                                                    b271)))))))
                                                                                                                                                                       (let ((b273
                                                                                                                                                                              res))
                                                                                                                                                                         (let ((b274
                                                                                                                                                                                (List
                                                                                                                                                                                 b273)))
                                                                                                                                                                           (let ((b259
                                                                                                                                                                                  (py-print
                                                                                                                                                                                   b274)))
                                                                                                                                                                             b259)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))