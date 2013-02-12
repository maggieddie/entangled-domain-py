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
(define object (lambda () (void)))

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
                       (if (hash-has-key? g$gbl-dict cls)
                           (begin
                             (travel-mro (hash-ref (hash-ref g$gbl-dict cls) "__mro__") name "inst" obj #f)
                             )
                           (begin
                             (error (format "the g$g$gbl-dict ~a instance ~a not has class ~a" g$gbl-dict cls))
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
                            (let* ((inst-cls-mro (hash-ref (hash-ref g$gbl-dict (hash-ref super-self "__class__")) "__mro__"))
                                   (cur-index (elem-index inst-cls-mro cur-cls)))
                              (if (equal? cur-index #f)
                                  (error (format "~a not found in the mro list ~a" cur-cls inst-cls-mro))
                                  (if (equal? (length inst-cls-mro) (add1 cur-index))
                                      (error (format "~a already the last one in the mro list ~a" cur-cls inst-cls-mro))
                                      (list-ref inst-cls-mro (add1 cur-index))))))
                           )
                       (travel-mro (hash-ref (hash-ref g$gbl-dict next-class) "__mro__") name "super" #f super-self))
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
             (if (hash-has-key? g$gbl-dict obj)
                 (begin
                   (let ((obj-dict (hash-ref g$gbl-dict obj)))
                     (travel-mro (hash-ref obj-dict "__mro__") name "class" #f #f))
                   )
                 (begin
                   (error (format "no obj: ~a in the g$gbl-dict ~a " obj g$gbl-dict))
                   )
                 )
             ; not an procedure (and not an dict)
             (error (format "not an procedure nor hash: ~a" obj))
             
             ))
     ]))



(define (get-precessors cls-lst)
  (append-map (λ (x)  (hash-ref (hash-ref g$gbl-dict x) "__base__")) cls-lst))

(define (cflt class-dict fn)
  (if (hash-has-key? class-dict fn) #t #f))

(define (search-field classes field-name flt)
  (let* ((cls-dicts (map (λ (x) (hash-ref g$gbl-dict x)) classes))
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
             (cur-dict (hash-ref g$gbl-dict cur-elem))
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
         (hash-set! (hash-ref g$gbl-dict obj) name val)
         (hash-set! obj name val))]))

(define-syntax remove-field!
  (syntax-rules ()
    [(_ obj name) 
     (if (procedure? obj)
         (hash-remove! (hash-ref g$gbl-dict obj) name val)
         (hash-remove! obj name))]))
         

 ;; the following is for computing MRO

;;  the mro computes the __mro__ for the cls, and extend the global store in the mapping for __mro__,
; which should not be like this.
  (define (mro cls)
    (let ((res 
           (append (list cls)
                   (merge
                    (append 
                     (map (lambda (x) (mro x)) (hash-ref (hash-ref g$gbl-dict cls) "__base__"))
                     (list (hash-ref (hash-ref g$gbl-dict cls) "__base__")))
                    (list)
                    ))))
      ;(hash-set! g$gbl-dict "__mro__" res)
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
                          (let* ((inst-cls-mro (hash-ref (hash-ref g$gbl-dict (hash-ref inst "__class__")) "__mro__"))
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
 (define g$gbl-dict (void))
 (define List (void))
 (define set (void))
 (define tuple (void))
 (define g$a (void))
 (define g$x (void))
 ((let ((b6
         (lambda (tmp-dict)
           (let ((b5
                  (lambda (return)
                    (let ((b0 ("__class__" List)))
                      (let ((b1 ("__type__" "instPyList")))
                        (let (($localo (dict b0 b1)))
                          (let ((b2 (get-field $localo "__init__")))
                            (let ((b3 (b2 tmp-dict)))
                              (let ((b4 (return $localo)))
                                (begin b3 b4))))))))))
             (call/ec b5)))))
    (let ((_7 (set! List b6)))
      (let ((b14
             (lambda (tmp-dict)
               (let ((b13
                      (lambda (return)
                        (let ((b8 ("__class__" set)))
                          (let ((b9 ("__type__" "instSet")))
                            (let (($localo (dict b8 b9)))
                              (let ((b10 (get-field $localo "__init__")))
                                (let ((b11 (b10 tmp-dict)))
                                  (let ((b12 (return $localo)))
                                    (begin b11 b12))))))))))
                 (call/ec b13)))))
        (let ((_15 (set! set b14)))
          (let ((b22
                 (lambda (tmp-dict)
                   (let ((b21
                          (lambda (return)
                            (let ((b16 ("__class__" tuple)))
                              (let ((b17 ("__type__" "instTuple")))
                                (let (($localo (dict b16 b17)))
                                  (let ((b18 (get-field $localo "__init__")))
                                    (let ((b19 (b18 tmp-dict)))
                                      (let ((b20 (return $localo)))
                                        (begin b19 b20))))))))))
                     (call/ec b21)))))
            (let ((_23 (set! tuple b22)))
              (let ((b24 `()))
                (let ((b25 ("__base__" b24)))
                  (let ((b26 ,object))
                    (let ((b27 (b26)))
                      (let ((b28 `b27))
                        (let ((b29 ("__mro__" b28)))
                          (let ((b30 (lambda v void)))
                            (let ((b31 ("__init__" b30)))
                              (let ((b32 (dict b25 b29 b31)))
                                (let ((b33 (object b32)))
                                  (let ((b34 ,object))
                                    (let ((b35 (b34)))
                                      (let ((b36 `b35))
                                        (let ((b37 ("__base__" b36)))
                                          (let ((b38 ,List))
                                            (let ((b39 ,object))
                                              (let ((b40 (b38 b39)))
                                                (let ((b41 `b40))
                                                  (let ((b42 ("__mro__" b41)))
                                                    (let ((b46
                                                           (lambda (self
                                                                    tmp-list-dict)
                                                             (let ((b45
                                                                    (lambda (return)
                                                                      (let ((b43
                                                                             (py-list?
                                                                              tmp-list-dict)))
                                                                        (if b43
                                                                          (let ((b44
                                                                                 (dict-ref
                                                                                  tmp-list-dict
                                                                                  "__containerdict__")))
                                                                            (set-field!
                                                                             self
                                                                             "__containerdict__"
                                                                             b44))
                                                                          (set-field!
                                                                           self
                                                                           "__containerdict__"
                                                                           tmp-list-dict))))))
                                                               (call/ec
                                                                b45)))))
                                                      (let ((b47
                                                             ("__init__" b46)))
                                                        (let ((b72
                                                               (lambda (self
                                                                        elem)
                                                                 (let ((container
                                                                        (dict-ref
                                                                         self
                                                                         "__containerdict__")))
                                                                   (let ((i 0))
                                                                     (let ((found
                                                                            #f))
                                                                       (let ((max
                                                                              (len
                                                                               self)))
                                                                         (let ((b67
                                                                                (lambda (break)
                                                                                  (let ((loop
                                                                                         (void)))
                                                                                    (let ((b64
                                                                                           (lambda ()
                                                                                             (let ((b48
                                                                                                    (<=
                                                                                                     0
                                                                                                     i)))
                                                                                               (let ((b49
                                                                                                      (if b48
                                                                                                        (<
                                                                                                         i
                                                                                                         max)
                                                                                                        #f)))
                                                                                                 (if b49
                                                                                                   (let ((b61
                                                                                                          (lambda (continue)
                                                                                                            (let ((b50
                                                                                                                   (dict-ref
                                                                                                                    container
                                                                                                                    i)))
                                                                                                              (let ((b51
                                                                                                                     (equal?
                                                                                                                      b50
                                                                                                                      elem)))
                                                                                                                (let ((b52
                                                                                                                       (not
                                                                                                                        found)))
                                                                                                                  (let ((b54
                                                                                                                         (if b52
                                                                                                                           (let ((_53
                                                                                                                                  (set! found
                                                                                                                                    i)))
                                                                                                                             (void))
                                                                                                                           (void))))
                                                                                                                    (let ((b55
                                                                                                                           (+
                                                                                                                            i
                                                                                                                            1)))
                                                                                                                      (let ((_56
                                                                                                                             (set! i
                                                                                                                               b55)))
                                                                                                                        (let ((b57
                                                                                                                               (b51
                                                                                                                                b54
                                                                                                                                (void))))
                                                                                                                          (let ((b58
                                                                                                                                 (+
                                                                                                                                  i
                                                                                                                                  1)))
                                                                                                                            (let ((_59
                                                                                                                                   (set! i
                                                                                                                                     b58)))
                                                                                                                              (let ((b60
                                                                                                                                     (else
                                                                                                                                      (void))))
                                                                                                                                (cond
                                                                                                                                 b57
                                                                                                                                 b60))))))))))))))
                                                                                                     (let ((b62
                                                                                                            (call/ec
                                                                                                             b61)))
                                                                                                       (let ((b63
                                                                                                              (loop)))
                                                                                                         (begin
                                                                                                           b62
                                                                                                           b63))))
                                                                                                   (void)))))))
                                                                                      (let ((_65
                                                                                             (set! loop
                                                                                               b64)))
                                                                                        (let ((b66
                                                                                               (loop)))
                                                                                          (begin
                                                                                            (void)
                                                                                            b66
                                                                                            (void)))))))))
                                                                           (let ((b68
                                                                                  (call/ec
                                                                                   b67)))
                                                                             (let ((b69
                                                                                    (not
                                                                                     found)))
                                                                               (let ((b71
                                                                                      (if b69
                                                                                        (let ((b70
                                                                                               (format
                                                                                                "no such elem: ~a in list ~a"
                                                                                                elem
                                                                                                container)))
                                                                                          (error
                                                                                           b70))
                                                                                        found)))
                                                                                 (begin
                                                                                   b68
                                                                                   b71))))))))))))
                                                          (let ((b73
                                                                 ("index"
                                                                  b72)))
                                                            (let ((b94
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
                                                                             (let ((b92
                                                                                    (lambda (break)
                                                                                      (let ((loop
                                                                                             (void)))
                                                                                        (let ((b89
                                                                                               (lambda ()
                                                                                                 (let ((b74
                                                                                                        (<=
                                                                                                         0
                                                                                                         i)))
                                                                                                   (let ((b75
                                                                                                          (if b74
                                                                                                            (<
                                                                                                             i
                                                                                                             max)
                                                                                                            #f)))
                                                                                                     (if b75
                                                                                                       (let ((b86
                                                                                                              (lambda (continue)
                                                                                                                (let ((b76
                                                                                                                       (dict-ref
                                                                                                                        container
                                                                                                                        i)))
                                                                                                                  (let ((b77
                                                                                                                         (equal?
                                                                                                                          b76
                                                                                                                          elem)))
                                                                                                                    (let ((b78
                                                                                                                           (+
                                                                                                                            cnt
                                                                                                                            1)))
                                                                                                                      (let ((_79
                                                                                                                             (set! cnt
                                                                                                                               b78)))
                                                                                                                        (let ((b80
                                                                                                                               (+
                                                                                                                                i
                                                                                                                                1)))
                                                                                                                          (let ((_81
                                                                                                                                 (set! i
                                                                                                                                   b80)))
                                                                                                                            (let ((b82
                                                                                                                                   (b77
                                                                                                                                    (void)
                                                                                                                                    (void))))
                                                                                                                              (let ((b83
                                                                                                                                     (+
                                                                                                                                      i
                                                                                                                                      1)))
                                                                                                                                (let ((_84
                                                                                                                                       (set! i
                                                                                                                                         b83)))
                                                                                                                                  (let ((b85
                                                                                                                                         (else
                                                                                                                                          (void))))
                                                                                                                                    (cond
                                                                                                                                     b82
                                                                                                                                     b85))))))))))))))
                                                                                                         (let ((b87
                                                                                                                (call/ec
                                                                                                                 b86)))
                                                                                                           (let ((b88
                                                                                                                  (loop)))
                                                                                                             (begin
                                                                                                               b87
                                                                                                               b88))))
                                                                                                       (void)))))))
                                                                                          (let ((_90
                                                                                                 (set! loop
                                                                                                   b89)))
                                                                                            (let ((b91
                                                                                                   (loop)))
                                                                                              (begin
                                                                                                (void)
                                                                                                b91
                                                                                                (void)))))))))
                                                                               (let ((b93
                                                                                      (call/ec
                                                                                       b92)))
                                                                                 (begin
                                                                                   b93
                                                                                   cnt))))))))))
                                                              (let ((b95
                                                                     ("count"
                                                                      b94)))
                                                                (let ((b98
                                                                       (lambda (self
                                                                                elem)
                                                                         (let ((max
                                                                                (len
                                                                                 self)))
                                                                           (let ((b96
                                                                                  (dict-ref
                                                                                   self
                                                                                   "__containerdict__")))
                                                                             (let ((b97
                                                                                    (dict-set!
                                                                                     b96
                                                                                     max
                                                                                     elem)))
                                                                               (begin
                                                                                 b97)))))))
                                                                  (let ((b99
                                                                         ("append"
                                                                          b98)))
                                                                    (let ((b135
                                                                           (lambda (self
                                                                                    index
                                                                                    elem)
                                                                             (let ((b100
                                                                                    (=
                                                                                     index
                                                                                     0)))
                                                                               (if b100
                                                                                 (let ((i
                                                                                        0))
                                                                                   (let ((container
                                                                                          (dict-ref
                                                                                           self
                                                                                           "__containerdict__")))
                                                                                     (let ((b109
                                                                                            (lambda (break)
                                                                                              (let ((b103
                                                                                                     (lambda ($seq11
                                                                                                              $loop12)
                                                                                                       (let ((b101
                                                                                                              (for-container
                                                                                                               $seq11
                                                                                                               $loop12)))
                                                                                                         (let ((b102
                                                                                                                (begin
                                                                                                                  b101
                                                                                                                  (void))))
                                                                                                           (begin
                                                                                                             b102))))))
                                                                                                (let ((b108
                                                                                                       (lambda (e)
                                                                                                         (let ((b107
                                                                                                                (lambda (continue)
                                                                                                                  (let ((b104
                                                                                                                         (+
                                                                                                                          i
                                                                                                                          1)))
                                                                                                                    (let ((_105
                                                                                                                           (set! i
                                                                                                                             b104)))
                                                                                                                      (let ((b106
                                                                                                                             (dict-set!
                                                                                                                              container
                                                                                                                              i
                                                                                                                              e)))
                                                                                                                        (begin
                                                                                                                          (void)
                                                                                                                          b106)))))))
                                                                                                           (call/ec
                                                                                                            b107)))))
                                                                                                  (b103
                                                                                                   self
                                                                                                   b108))))))
                                                                                       (let ((b110
                                                                                              (call/ec
                                                                                               b109)))
                                                                                         (let ((b111
                                                                                                (dict-set!
                                                                                                 container
                                                                                                 0
                                                                                                 elem)))
                                                                                           (begin
                                                                                             b110
                                                                                             b111))))))
                                                                                 (let ((b112
                                                                                        (len
                                                                                         self)))
                                                                                   (let ((b113
                                                                                          (>=
                                                                                           index
                                                                                           b112)))
                                                                                     (if b113
                                                                                       (let ((tail
                                                                                              (len
                                                                                               self)))
                                                                                         (let ((container
                                                                                                (dict-ref
                                                                                                 self
                                                                                                 "__containerdict__")))
                                                                                           (let ((b114
                                                                                                  (dict-set!
                                                                                                   container
                                                                                                   tail
                                                                                                   elem)))
                                                                                             (begin
                                                                                               b114))))
                                                                                       (let ((b115
                                                                                              (>
                                                                                               index
                                                                                               0)))
                                                                                         (let ((b117
                                                                                                (if b115
                                                                                                  (let ((b116
                                                                                                         (len
                                                                                                          self)))
                                                                                                    (<
                                                                                                     index
                                                                                                     b116))
                                                                                                  #f)))
                                                                                           (if b117
                                                                                             (let ((max
                                                                                                    (len
                                                                                                     self)))
                                                                                               (let ((container
                                                                                                      (dict-ref
                                                                                                       self
                                                                                                       "__containerdict__")))
                                                                                                 (let ((i
                                                                                                        max))
                                                                                                   (let ((b131
                                                                                                          (lambda (break)
                                                                                                            (let ((loop
                                                                                                                   (void)))
                                                                                                              (let ((b128
                                                                                                                     (lambda ()
                                                                                                                       (let ((b118
                                                                                                                              (>=
                                                                                                                               max
                                                                                                                               i)))
                                                                                                                         (let ((b119
                                                                                                                                (if b118
                                                                                                                                  (>
                                                                                                                                   i
                                                                                                                                   index)
                                                                                                                                  #f)))
                                                                                                                           (if b119
                                                                                                                             (let ((b125
                                                                                                                                    (lambda (continue)
                                                                                                                                      (let ((b120
                                                                                                                                             (-
                                                                                                                                              i
                                                                                                                                              1)))
                                                                                                                                        (let ((b121
                                                                                                                                               (dict-ref
                                                                                                                                                container
                                                                                                                                                b120)))
                                                                                                                                          (let ((b122
                                                                                                                                                 (dict-set!
                                                                                                                                                  container
                                                                                                                                                  i
                                                                                                                                                  b121)))
                                                                                                                                            (let ((b123
                                                                                                                                                   (-
                                                                                                                                                    i
                                                                                                                                                    1)))
                                                                                                                                              (let ((_124
                                                                                                                                                     (set! i
                                                                                                                                                       b123)))
                                                                                                                                                (begin
                                                                                                                                                  b122
                                                                                                                                                  (void))))))))))
                                                                                                                               (let ((b126
                                                                                                                                      (call/ec
                                                                                                                                       b125)))
                                                                                                                                 (let ((b127
                                                                                                                                        (loop)))
                                                                                                                                   (begin
                                                                                                                                     b126
                                                                                                                                     b127))))
                                                                                                                             (void)))))))
                                                                                                                (let ((_129
                                                                                                                       (set! loop
                                                                                                                         b128)))
                                                                                                                  (let ((b130
                                                                                                                         (loop)))
                                                                                                                    (begin
                                                                                                                      (void)
                                                                                                                      b130
                                                                                                                      (void)))))))))
                                                                                                     (let ((b132
                                                                                                            (call/ec
                                                                                                             b131)))
                                                                                                       (let ((b133
                                                                                                              (dict-set!
                                                                                                               container
                                                                                                               index
                                                                                                               elem)))
                                                                                                         (let ((b134
                                                                                                                (begin
                                                                                                                  b132
                                                                                                                  b133)))
                                                                                                           (begin
                                                                                                             b134))))))))
                                                                                             (void))))))))))))
                                                                      (let ((b136
                                                                             ("insert"
                                                                              b135)))
                                                                        (let ((b137
                                                                               ("reverse"
                                                                                (void))))
                                                                          (let ((b138
                                                                                 ("sort"
                                                                                  (void))))
                                                                            (let ((b139
                                                                                   ("pop"
                                                                                    (void))))
                                                                              (let ((b140
                                                                                     ("extend"
                                                                                      (void))))
                                                                                (let ((b141
                                                                                       (dict
                                                                                        b37
                                                                                        b42
                                                                                        b47
                                                                                        b73
                                                                                        b95
                                                                                        b99
                                                                                        b136
                                                                                        b137
                                                                                        b138
                                                                                        b139
                                                                                        b140)))
                                                                                  (let ((b142
                                                                                         (List
                                                                                          b141)))
                                                                                    (let ((b143
                                                                                           ,object))
                                                                                      (let ((b144
                                                                                             (b143)))
                                                                                        (let ((b145
                                                                                               `b144))
                                                                                          (let ((b146
                                                                                                 ("__base__"
                                                                                                  b145)))
                                                                                            (let ((b147
                                                                                                   ,set))
                                                                                              (let ((b148
                                                                                                     ,object))
                                                                                                (let ((b149
                                                                                                       (b147
                                                                                                        b148)))
                                                                                                  (let ((b150
                                                                                                         `b149))
                                                                                                    (let ((b151
                                                                                                           ("__mro__"
                                                                                                            b150)))
                                                                                                      (let ((b155
                                                                                                             (lambda (self
                                                                                                                      tmp-list-dict)
                                                                                                               (let ((b154
                                                                                                                      (lambda (return)
                                                                                                                        (let ((b152
                                                                                                                               (py-list?
                                                                                                                                tmp-list-dict)))
                                                                                                                          (if b152
                                                                                                                            (let ((b153
                                                                                                                                   (dict-ref
                                                                                                                                    tmp-list-dict
                                                                                                                                    "__containerdict__")))
                                                                                                                              (set-field!
                                                                                                                               self
                                                                                                                               "__containerdict__"
                                                                                                                               b153))
                                                                                                                            (set-field!
                                                                                                                             self
                                                                                                                             "__containerdict__"
                                                                                                                             tmp-list-dict))))))
                                                                                                                 (call/ec
                                                                                                                  b154)))))
                                                                                                        (let ((b156
                                                                                                               ("__init__"
                                                                                                                b155)))
                                                                                                          (let ((b157
                                                                                                                 (dict
                                                                                                                  b146
                                                                                                                  b151
                                                                                                                  b156)))
                                                                                                            (let ((b158
                                                                                                                   (set
                                                                                                                    b157)))
                                                                                                              (let ((b159
                                                                                                                     ,object))
                                                                                                                (let ((b160
                                                                                                                       (b159)))
                                                                                                                  (let ((b161
                                                                                                                         `b160))
                                                                                                                    (let ((b162
                                                                                                                           ("__base__"
                                                                                                                            b161)))
                                                                                                                      (let ((b163
                                                                                                                             ,tuple))
                                                                                                                        (let ((b164
                                                                                                                               ,object))
                                                                                                                          (let ((b165
                                                                                                                                 (b163
                                                                                                                                  b164)))
                                                                                                                            (let ((b166
                                                                                                                                   `b165))
                                                                                                                              (let ((b167
                                                                                                                                     ("__mro__"
                                                                                                                                      b166)))
                                                                                                                                (let ((b173
                                                                                                                                       (lambda (self
                                                                                                                                                tmp-dict)
                                                                                                                                         (let ((b172
                                                                                                                                                (lambda (return)
                                                                                                                                                  (let ((t13
                                                                                                                                                         (py-list?
                                                                                                                                                          tmp-dict)))
                                                                                                                                                    (let ((b169
                                                                                                                                                           (if t13
                                                                                                                                                             t13
                                                                                                                                                             (let ((t14
                                                                                                                                                                    (tuple?
                                                                                                                                                                     tmp-dict)))
                                                                                                                                                               (let ((b168
                                                                                                                                                                      (if t14
                                                                                                                                                                        t14
                                                                                                                                                                        (set?
                                                                                                                                                                         tmp-dict))))
                                                                                                                                                                 (begin
                                                                                                                                                                   b168))))))
                                                                                                                                                      (let ((b170
                                                                                                                                                             (begin
                                                                                                                                                               b169)))
                                                                                                                                                        (if b170
                                                                                                                                                          (let ((b171
                                                                                                                                                                 (dict-ref
                                                                                                                                                                  tmp-dict
                                                                                                                                                                  "__containerdict__")))
                                                                                                                                                            (set-field!
                                                                                                                                                             self
                                                                                                                                                             "__containerdict__"
                                                                                                                                                             b171))
                                                                                                                                                          (set-field!
                                                                                                                                                           self
                                                                                                                                                           "__containerdict__"
                                                                                                                                                           tmp-dict))))))))
                                                                                                                                           (call/ec
                                                                                                                                            b172)))))
                                                                                                                                  (let ((b174
                                                                                                                                         ("__init__"
                                                                                                                                          b173)))
                                                                                                                                    (let ((b199
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
                                                                                                                                                     (let ((b194
                                                                                                                                                            (lambda (break)
                                                                                                                                                              (let ((loop
                                                                                                                                                                     (void)))
                                                                                                                                                                (let ((b191
                                                                                                                                                                       (lambda ()
                                                                                                                                                                         (let ((b175
                                                                                                                                                                                (<=
                                                                                                                                                                                 0
                                                                                                                                                                                 i)))
                                                                                                                                                                           (let ((b176
                                                                                                                                                                                  (if b175
                                                                                                                                                                                    (<
                                                                                                                                                                                     i
                                                                                                                                                                                     max)
                                                                                                                                                                                    #f)))
                                                                                                                                                                             (if b176
                                                                                                                                                                               (let ((b188
                                                                                                                                                                                      (lambda (continue)
                                                                                                                                                                                        (let ((b177
                                                                                                                                                                                               (dict-ref
                                                                                                                                                                                                container
                                                                                                                                                                                                i)))
                                                                                                                                                                                          (let ((b178
                                                                                                                                                                                                 (equal?
                                                                                                                                                                                                  b177
                                                                                                                                                                                                  elem)))
                                                                                                                                                                                            (let ((b179
                                                                                                                                                                                                   (not
                                                                                                                                                                                                    found)))
                                                                                                                                                                                              (let ((b181
                                                                                                                                                                                                     (if b179
                                                                                                                                                                                                       (let ((_180
                                                                                                                                                                                                              (set! found
                                                                                                                                                                                                                i)))
                                                                                                                                                                                                         (void))
                                                                                                                                                                                                       (void))))
                                                                                                                                                                                                (let ((b182
                                                                                                                                                                                                       (+
                                                                                                                                                                                                        i
                                                                                                                                                                                                        1)))
                                                                                                                                                                                                  (let ((_183
                                                                                                                                                                                                         (set! i
                                                                                                                                                                                                           b182)))
                                                                                                                                                                                                    (let ((b184
                                                                                                                                                                                                           (b178
                                                                                                                                                                                                            b181
                                                                                                                                                                                                            (void))))
                                                                                                                                                                                                      (let ((b185
                                                                                                                                                                                                             (+
                                                                                                                                                                                                              i
                                                                                                                                                                                                              1)))
                                                                                                                                                                                                        (let ((_186
                                                                                                                                                                                                               (set! i
                                                                                                                                                                                                                 b185)))
                                                                                                                                                                                                          (let ((b187
                                                                                                                                                                                                                 (else
                                                                                                                                                                                                                  (void))))
                                                                                                                                                                                                            (cond
                                                                                                                                                                                                             b184
                                                                                                                                                                                                             b187))))))))))))))
                                                                                                                                                                                 (let ((b189
                                                                                                                                                                                        (call/ec
                                                                                                                                                                                         b188)))
                                                                                                                                                                                   (let ((b190
                                                                                                                                                                                          (loop)))
                                                                                                                                                                                     (begin
                                                                                                                                                                                       b189
                                                                                                                                                                                       b190))))
                                                                                                                                                                               (void)))))))
                                                                                                                                                                  (let ((_192
                                                                                                                                                                         (set! loop
                                                                                                                                                                           b191)))
                                                                                                                                                                    (let ((b193
                                                                                                                                                                           (loop)))
                                                                                                                                                                      (begin
                                                                                                                                                                        (void)
                                                                                                                                                                        b193
                                                                                                                                                                        (void)))))))))
                                                                                                                                                       (let ((b195
                                                                                                                                                              (call/ec
                                                                                                                                                               b194)))
                                                                                                                                                         (let ((b196
                                                                                                                                                                (not
                                                                                                                                                                 found)))
                                                                                                                                                           (let ((b198
                                                                                                                                                                  (if b196
                                                                                                                                                                    (let ((b197
                                                                                                                                                                           (format
                                                                                                                                                                            "no such elem: ~a in list ~a"
                                                                                                                                                                            elem
                                                                                                                                                                            container)))
                                                                                                                                                                      (error
                                                                                                                                                                       b197))
                                                                                                                                                                    found)))
                                                                                                                                                             (begin
                                                                                                                                                               b195
                                                                                                                                                               b198))))))))))))
                                                                                                                                      (let ((b200
                                                                                                                                             ("index"
                                                                                                                                              b199)))
                                                                                                                                        (let ((b221
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
                                                                                                                                                         (let ((b219
                                                                                                                                                                (lambda (break)
                                                                                                                                                                  (let ((loop
                                                                                                                                                                         (void)))
                                                                                                                                                                    (let ((b216
                                                                                                                                                                           (lambda ()
                                                                                                                                                                             (let ((b201
                                                                                                                                                                                    (<=
                                                                                                                                                                                     0
                                                                                                                                                                                     i)))
                                                                                                                                                                               (let ((b202
                                                                                                                                                                                      (if b201
                                                                                                                                                                                        (<
                                                                                                                                                                                         i
                                                                                                                                                                                         max)
                                                                                                                                                                                        #f)))
                                                                                                                                                                                 (if b202
                                                                                                                                                                                   (let ((b213
                                                                                                                                                                                          (lambda (continue)
                                                                                                                                                                                            (let ((b203
                                                                                                                                                                                                   (dict-ref
                                                                                                                                                                                                    container
                                                                                                                                                                                                    i)))
                                                                                                                                                                                              (let ((b204
                                                                                                                                                                                                     (equal?
                                                                                                                                                                                                      b203
                                                                                                                                                                                                      elem)))
                                                                                                                                                                                                (let ((b205
                                                                                                                                                                                                       (+
                                                                                                                                                                                                        cnt
                                                                                                                                                                                                        1)))
                                                                                                                                                                                                  (let ((_206
                                                                                                                                                                                                         (set! cnt
                                                                                                                                                                                                           b205)))
                                                                                                                                                                                                    (let ((b207
                                                                                                                                                                                                           (+
                                                                                                                                                                                                            i
                                                                                                                                                                                                            1)))
                                                                                                                                                                                                      (let ((_208
                                                                                                                                                                                                             (set! i
                                                                                                                                                                                                               b207)))
                                                                                                                                                                                                        (let ((b209
                                                                                                                                                                                                               (b204
                                                                                                                                                                                                                (void)
                                                                                                                                                                                                                (void))))
                                                                                                                                                                                                          (let ((b210
                                                                                                                                                                                                                 (+
                                                                                                                                                                                                                  i
                                                                                                                                                                                                                  1)))
                                                                                                                                                                                                            (let ((_211
                                                                                                                                                                                                                   (set! i
                                                                                                                                                                                                                     b210)))
                                                                                                                                                                                                              (let ((b212
                                                                                                                                                                                                                     (else
                                                                                                                                                                                                                      (void))))
                                                                                                                                                                                                                (cond
                                                                                                                                                                                                                 b209
                                                                                                                                                                                                                 b212))))))))))))))
                                                                                                                                                                                     (let ((b214
                                                                                                                                                                                            (call/ec
                                                                                                                                                                                             b213)))
                                                                                                                                                                                       (let ((b215
                                                                                                                                                                                              (loop)))
                                                                                                                                                                                         (begin
                                                                                                                                                                                           b214
                                                                                                                                                                                           b215))))
                                                                                                                                                                                   (void)))))))
                                                                                                                                                                      (let ((_217
                                                                                                                                                                             (set! loop
                                                                                                                                                                               b216)))
                                                                                                                                                                        (let ((b218
                                                                                                                                                                               (loop)))
                                                                                                                                                                          (begin
                                                                                                                                                                            (void)
                                                                                                                                                                            b218
                                                                                                                                                                            (void)))))))))
                                                                                                                                                           (let ((b220
                                                                                                                                                                  (call/ec
                                                                                                                                                                   b219)))
                                                                                                                                                             (begin
                                                                                                                                                               b220
                                                                                                                                                               cnt))))))))))
                                                                                                                                          (let ((b222
                                                                                                                                                 ("count"
                                                                                                                                                  b221)))
                                                                                                                                            (let ((b223
                                                                                                                                                   (dict
                                                                                                                                                    b162
                                                                                                                                                    b167
                                                                                                                                                    b174
                                                                                                                                                    b200
                                                                                                                                                    b222)))
                                                                                                                                              (let ((b224
                                                                                                                                                     (tuple
                                                                                                                                                      b223)))
                                                                                                                                                (let ((b225
                                                                                                                                                       (dict
                                                                                                                                                        b33
                                                                                                                                                        b142
                                                                                                                                                        b158
                                                                                                                                                        b224)))
                                                                                                                                                  (let ((_226
                                                                                                                                                         (set! g$gbl-dict
                                                                                                                                                           b225)))
                                                                                                                                                    (let ((b227
                                                                                                                                                           (0
                                                                                                                                                            "b")))
                                                                                                                                                      (let ((b228
                                                                                                                                                             (0
                                                                                                                                                              "A")))
                                                                                                                                                        (let ((b229
                                                                                                                                                               (1
                                                                                                                                                                "B")))
                                                                                                                                                          (let ((b230
                                                                                                                                                                 (dict
                                                                                                                                                                  b228
                                                                                                                                                                  b229)))
                                                                                                                                                            (let ((b231
                                                                                                                                                                   (List
                                                                                                                                                                    b230)))
                                                                                                                                                              (let ((b232
                                                                                                                                                                     (1
                                                                                                                                                                      b231)))
                                                                                                                                                                (let ((b233
                                                                                                                                                                       (2
                                                                                                                                                                        "c")))
                                                                                                                                                                  (let ((b234
                                                                                                                                                                         (dict
                                                                                                                                                                          b227
                                                                                                                                                                          b232
                                                                                                                                                                          b233)))
                                                                                                                                                                    (let ((b235
                                                                                                                                                                           (List
                                                                                                                                                                            b234)))
                                                                                                                                                                      (let ((_236
                                                                                                                                                                             (set! g$a
                                                                                                                                                                               b235)))
                                                                                                                                                                        (let ((b237
                                                                                                                                                                               (py-print
                                                                                                                                                                                g$a)))
                                                                                                                                                                          (let ((e11
                                                                                                                                                                                 g$a))
                                                                                                                                                                            (let ((i12
                                                                                                                                                                                   0))
                                                                                                                                                                              (let ((b238
                                                                                                                                                                                     (py-list?
                                                                                                                                                                                      e11)))
                                                                                                                                                                                (let ((b242
                                                                                                                                                                                       (if b238
                                                                                                                                                                                         (py-list-ref
                                                                                                                                                                                          e11
                                                                                                                                                                                          i12)
                                                                                                                                                                                         (let ((b239
                                                                                                                                                                                                (tuple?
                                                                                                                                                                                                 e11)))
                                                                                                                                                                                           (if b239
                                                                                                                                                                                             (tuple-ref
                                                                                                                                                                                              e11
                                                                                                                                                                                              i12)
                                                                                                                                                                                             (let ((b240
                                                                                                                                                                                                    (dict?
                                                                                                                                                                                                     e11)))
                                                                                                                                                                                               (if b240
                                                                                                                                                                                                 (dict-ref
                                                                                                                                                                                                  e11
                                                                                                                                                                                                  i12)
                                                                                                                                                                                                 (let ((b241
                                                                                                                                                                                                        (string?
                                                                                                                                                                                                         e11)))
                                                                                                                                                                                                   (if b241
                                                                                                                                                                                                     (string-ref
                                                                                                                                                                                                      e11
                                                                                                                                                                                                      i12)
                                                                                                                                                                                                     (error
                                                                                                                                                                                                      "cannot index object"))))))))))
                                                                                                                                                                                  (let ((b243
                                                                                                                                                                                         (begin
                                                                                                                                                                                           b242)))
                                                                                                                                                                                    (let ((b244
                                                                                                                                                                                           (begin
                                                                                                                                                                                             b243)))
                                                                                                                                                                                      (let ((b245
                                                                                                                                                                                             (py-print
                                                                                                                                                                                              b244)))
                                                                                                                                                                                        (let ((e13
                                                                                                                                                                                               g$a))
                                                                                                                                                                                          (let ((i14
                                                                                                                                                                                                 1))
                                                                                                                                                                                            (let ((b246
                                                                                                                                                                                                   (py-list?
                                                                                                                                                                                                    e13)))
                                                                                                                                                                                              (let ((b250
                                                                                                                                                                                                     (if b246
                                                                                                                                                                                                       (py-list-ref
                                                                                                                                                                                                        e13
                                                                                                                                                                                                        i14)
                                                                                                                                                                                                       (let ((b247
                                                                                                                                                                                                              (tuple?
                                                                                                                                                                                                               e13)))
                                                                                                                                                                                                         (if b247
                                                                                                                                                                                                           (tuple-ref
                                                                                                                                                                                                            e13
                                                                                                                                                                                                            i14)
                                                                                                                                                                                                           (let ((b248
                                                                                                                                                                                                                  (dict?
                                                                                                                                                                                                                   e13)))
                                                                                                                                                                                                             (if b248
                                                                                                                                                                                                               (dict-ref
                                                                                                                                                                                                                e13
                                                                                                                                                                                                                i14)
                                                                                                                                                                                                               (let ((b249
                                                                                                                                                                                                                      (string?
                                                                                                                                                                                                                       e13)))
                                                                                                                                                                                                                 (if b249
                                                                                                                                                                                                                   (string-ref
                                                                                                                                                                                                                    e13
                                                                                                                                                                                                                    i14)
                                                                                                                                                                                                                   (error
                                                                                                                                                                                                                    "cannot index object"))))))))))
                                                                                                                                                                                                (let ((b251
                                                                                                                                                                                                       (begin
                                                                                                                                                                                                         b250)))
                                                                                                                                                                                                  (let ((b252
                                                                                                                                                                                                         (begin
                                                                                                                                                                                                           b251)))
                                                                                                                                                                                                    (let ((b253
                                                                                                                                                                                                           (py-print
                                                                                                                                                                                                            b252)))
                                                                                                                                                                                                      (let ((e15
                                                                                                                                                                                                             g$a))
                                                                                                                                                                                                        (let ((i16
                                                                                                                                                                                                               2))
                                                                                                                                                                                                          (let ((b254
                                                                                                                                                                                                                 (py-list?
                                                                                                                                                                                                                  e15)))
                                                                                                                                                                                                            (let ((b258
                                                                                                                                                                                                                   (if b254
                                                                                                                                                                                                                     (py-list-ref
                                                                                                                                                                                                                      e15
                                                                                                                                                                                                                      i16)
                                                                                                                                                                                                                     (let ((b255
                                                                                                                                                                                                                            (tuple?
                                                                                                                                                                                                                             e15)))
                                                                                                                                                                                                                       (if b255
                                                                                                                                                                                                                         (tuple-ref
                                                                                                                                                                                                                          e15
                                                                                                                                                                                                                          i16)
                                                                                                                                                                                                                         (let ((b256
                                                                                                                                                                                                                                (dict?
                                                                                                                                                                                                                                 e15)))
                                                                                                                                                                                                                           (if b256
                                                                                                                                                                                                                             (dict-ref
                                                                                                                                                                                                                              e15
                                                                                                                                                                                                                              i16)
                                                                                                                                                                                                                             (let ((b257
                                                                                                                                                                                                                                    (string?
                                                                                                                                                                                                                                     e15)))
                                                                                                                                                                                                                               (if b257
                                                                                                                                                                                                                                 (string-ref
                                                                                                                                                                                                                                  e15
                                                                                                                                                                                                                                  i16)
                                                                                                                                                                                                                                 (error
                                                                                                                                                                                                                                  "cannot index object"))))))))))
                                                                                                                                                                                                              (let ((b259
                                                                                                                                                                                                                     (begin
                                                                                                                                                                                                                       b258)))
                                                                                                                                                                                                                (let ((b260
                                                                                                                                                                                                                       (begin
                                                                                                                                                                                                                         b259)))
                                                                                                                                                                                                                  (let ((b261
                                                                                                                                                                                                                         (py-print
                                                                                                                                                                                                                          b260)))
                                                                                                                                                                                                                    (let ((b262
                                                                                                                                                                                                                           (py-print
                                                                                                                                                                                                                            "before deleting....")))
                                                                                                                                                                                                                      (let ((b263
                                                                                                                                                                                                                             (len
                                                                                                                                                                                                                              g$a)))
                                                                                                                                                                                                                        (let ((b17
                                                                                                                                                                                                                               g$a))
                                                                                                                                                                                                                          (let ((i18
                                                                                                                                                                                                                                 0))
                                                                                                                                                                                                                            (let ((b264
                                                                                                                                                                                                                                   (tuple?
                                                                                                                                                                                                                                    b17)))
                                                                                                                                                                                                                              (let ((b268
                                                                                                                                                                                                                                     (if b264
                                                                                                                                                                                                                                       (error
                                                                                                                                                                                                                                        "Cannot delete from tuples!")
                                                                                                                                                                                                                                       (let ((b265
                                                                                                                                                                                                                                              (py-list?
                                                                                                                                                                                                                                               b17)))
                                                                                                                                                                                                                                         (if b265
                                                                                                                                                                                                                                           (py-list-remove!
                                                                                                                                                                                                                                            b17
                                                                                                                                                                                                                                            i18)
                                                                                                                                                                                                                                           (let ((b266
                                                                                                                                                                                                                                                  (dict?
                                                                                                                                                                                                                                                   b17)))
                                                                                                                                                                                                                                             (if b266
                                                                                                                                                                                                                                               (dict-remove!
                                                                                                                                                                                                                                                b17
                                                                                                                                                                                                                                                i18)
                                                                                                                                                                                                                                               (let ((b267
                                                                                                                                                                                                                                                      (string?
                                                                                                                                                                                                                                                       b17)))
                                                                                                                                                                                                                                                 (if b267
                                                                                                                                                                                                                                                   (error
                                                                                                                                                                                                                                                    "Cannot delete from string!")
                                                                                                                                                                                                                                                   (void))))))))))
                                                                                                                                                                                                                                (let ((b269
                                                                                                                                                                                                                                       (begin
                                                                                                                                                                                                                                         b268)))
                                                                                                                                                                                                                                  (let ((b270
                                                                                                                                                                                                                                         (begin
                                                                                                                                                                                                                                           b269)))
                                                                                                                                                                                                                                    (let ((b271
                                                                                                                                                                                                                                           (py-print
                                                                                                                                                                                                                                            g$a)))
                                                                                                                                                                                                                                      (let ((b272
                                                                                                                                                                                                                                             (py-print
                                                                                                                                                                                                                                              "after deleting...")))
                                                                                                                                                                                                                                        (let ((b273
                                                                                                                                                                                                                                               (len
                                                                                                                                                                                                                                                g$a)))
                                                                                                                                                                                                                                          (let ((e19
                                                                                                                                                                                                                                                 g$a))
                                                                                                                                                                                                                                            (let ((i20
                                                                                                                                                                                                                                                   0))
                                                                                                                                                                                                                                              (let ((b274
                                                                                                                                                                                                                                                     (py-list?
                                                                                                                                                                                                                                                      e19)))
                                                                                                                                                                                                                                                (let ((b278
                                                                                                                                                                                                                                                       (if b274
                                                                                                                                                                                                                                                         (py-list-ref
                                                                                                                                                                                                                                                          e19
                                                                                                                                                                                                                                                          i20)
                                                                                                                                                                                                                                                         (let ((b275
                                                                                                                                                                                                                                                                (tuple?
                                                                                                                                                                                                                                                                 e19)))
                                                                                                                                                                                                                                                           (if b275
                                                                                                                                                                                                                                                             (tuple-ref
                                                                                                                                                                                                                                                              e19
                                                                                                                                                                                                                                                              i20)
                                                                                                                                                                                                                                                             (let ((b276
                                                                                                                                                                                                                                                                    (dict?
                                                                                                                                                                                                                                                                     e19)))
                                                                                                                                                                                                                                                               (if b276
                                                                                                                                                                                                                                                                 (dict-ref
                                                                                                                                                                                                                                                                  e19
                                                                                                                                                                                                                                                                  i20)
                                                                                                                                                                                                                                                                 (let ((b277
                                                                                                                                                                                                                                                                        (string?
                                                                                                                                                                                                                                                                         e19)))
                                                                                                                                                                                                                                                                   (if b277
                                                                                                                                                                                                                                                                     (string-ref
                                                                                                                                                                                                                                                                      e19
                                                                                                                                                                                                                                                                      i20)
                                                                                                                                                                                                                                                                     (error
                                                                                                                                                                                                                                                                      "cannot index object"))))))))))
                                                                                                                                                                                                                                                  (let ((b279
                                                                                                                                                                                                                                                         (begin
                                                                                                                                                                                                                                                           b278)))
                                                                                                                                                                                                                                                    (let ((e21
                                                                                                                                                                                                                                                           (begin
                                                                                                                                                                                                                                                             b279)))
                                                                                                                                                                                                                                                      (let ((i22
                                                                                                                                                                                                                                                             0))
                                                                                                                                                                                                                                                        (let ((b280
                                                                                                                                                                                                                                                               (py-list?
                                                                                                                                                                                                                                                                e21)))
                                                                                                                                                                                                                                                          (let ((b284
                                                                                                                                                                                                                                                                 (if b280
                                                                                                                                                                                                                                                                   (py-list-ref
                                                                                                                                                                                                                                                                    e21
                                                                                                                                                                                                                                                                    i22)
                                                                                                                                                                                                                                                                   (let ((b281
                                                                                                                                                                                                                                                                          (tuple?
                                                                                                                                                                                                                                                                           e21)))
                                                                                                                                                                                                                                                                     (if b281
                                                                                                                                                                                                                                                                       (tuple-ref
                                                                                                                                                                                                                                                                        e21
                                                                                                                                                                                                                                                                        i22)
                                                                                                                                                                                                                                                                       (let ((b282
                                                                                                                                                                                                                                                                              (dict?
                                                                                                                                                                                                                                                                               e21)))
                                                                                                                                                                                                                                                                         (if b282
                                                                                                                                                                                                                                                                           (dict-ref
                                                                                                                                                                                                                                                                            e21
                                                                                                                                                                                                                                                                            i22)
                                                                                                                                                                                                                                                                           (let ((b283
                                                                                                                                                                                                                                                                                  (string?
                                                                                                                                                                                                                                                                                   e21)))
                                                                                                                                                                                                                                                                             (if b283
                                                                                                                                                                                                                                                                               (string-ref
                                                                                                                                                                                                                                                                                e21
                                                                                                                                                                                                                                                                                i22)
                                                                                                                                                                                                                                                                               (error
                                                                                                                                                                                                                                                                                "cannot index object"))))))))))
                                                                                                                                                                                                                                                            (let ((b285
                                                                                                                                                                                                                                                                   (begin
                                                                                                                                                                                                                                                                     b284)))
                                                                                                                                                                                                                                                              (let ((b286
                                                                                                                                                                                                                                                                     (begin
                                                                                                                                                                                                                                                                       b285)))
                                                                                                                                                                                                                                                                (let ((b287
                                                                                                                                                                                                                                                                       (py-print
                                                                                                                                                                                                                                                                        b286)))
                                                                                                                                                                                                                                                                  (let ((b296
                                                                                                                                                                                                                                                                         (lambda (break)
                                                                                                                                                                                                                                                                           (let ((b290
                                                                                                                                                                                                                                                                                  (lambda ($seq15
                                                                                                                                                                                                                                                                                           $loop16)
                                                                                                                                                                                                                                                                                    (let ((b288
                                                                                                                                                                                                                                                                                           (for-container
                                                                                                                                                                                                                                                                                            $seq15
                                                                                                                                                                                                                                                                                            $loop16)))
                                                                                                                                                                                                                                                                                      (let ((b289
                                                                                                                                                                                                                                                                                             (begin
                                                                                                                                                                                                                                                                                               b288
                                                                                                                                                                                                                                                                                               (void))))
                                                                                                                                                                                                                                                                                        (begin
                                                                                                                                                                                                                                                                                          b289))))))
                                                                                                                                                                                                                                                                             (let ((b295
                                                                                                                                                                                                                                                                                    (lambda (i23)
                                                                                                                                                                                                                                                                                      (let ((b294
                                                                                                                                                                                                                                                                                             (lambda (continue)
                                                                                                                                                                                                                                                                                               (let ((_291
                                                                                                                                                                                                                                                                                                      (set! g$x
                                                                                                                                                                                                                                                                                                        i23)))
                                                                                                                                                                                                                                                                                                 (let ((b292
                                                                                                                                                                                                                                                                                                        (py-print
                                                                                                                                                                                                                                                                                                         g$x)))
                                                                                                                                                                                                                                                                                                   (let ((b293
                                                                                                                                                                                                                                                                                                          (begin
                                                                                                                                                                                                                                                                                                            b292)))
                                                                                                                                                                                                                                                                                                     (begin
                                                                                                                                                                                                                                                                                                       (void)
                                                                                                                                                                                                                                                                                                       b293)))))))
                                                                                                                                                                                                                                                                                        (call/ec
                                                                                                                                                                                                                                                                                         b294)))))
                                                                                                                                                                                                                                                                               (b290
                                                                                                                                                                                                                                                                                g$a
                                                                                                                                                                                                                                                                                b295))))))
                                                                                                                                                                                                                                                                    (let ((b297
                                                                                                                                                                                                                                                                           (call/ec
                                                                                                                                                                                                                                                                            b296)))
                                                                                                                                                                                                                                                                      (let ((b298
                                                                                                                                                                                                                                                                             (get-field
                                                                                                                                                                                                                                                                              g$a
                                                                                                                                                                                                                                                                              "append")))
                                                                                                                                                                                                                                                                        (let ((b299
                                                                                                                                                                                                                                                                               (b298
                                                                                                                                                                                                                                                                                "D")))
                                                                                                                                                                                                                                                                          (let ((b300
                                                                                                                                                                                                                                                                                 (py-print
                                                                                                                                                                                                                                                                                  g$a)))
                                                                                                                                                                                                                                                                            (begin
                                                                                                                                                                                                                                                                              (void)
                                                                                                                                                                                                                                                                              (void)
                                                                                                                                                                                                                                                                              (void)
                                                                                                                                                                                                                                                                              (void)
                                                                                                                                                                                                                                                                              (void)
                                                                                                                                                                                                                                                                              b237
                                                                                                                                                                                                                                                                              b245
                                                                                                                                                                                                                                                                              b253
                                                                                                                                                                                                                                                                              b261
                                                                                                                                                                                                                                                                              b262
                                                                                                                                                                                                                                                                              b263
                                                                                                                                                                                                                                                                              b270
                                                                                                                                                                                                                                                                              b271
                                                                                                                                                                                                                                                                              b272
                                                                                                                                                                                                                                                                              b273
                                                                                                                                                                                                                                                                              b287
                                                                                                                                                                                                                                                                              b297
                                                                                                                                                                                                                                                                              b299
                                                                                                                                                                                                                                                                              b300))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
