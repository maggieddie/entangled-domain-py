#lang racket
(require "utils_cesk.rkt")
;; the experiment CESK* machine for the simple grammar: 

;; langauge grammar

;; <prog> ::= <dec> ...

;; <dec> ::= (define <var> <exp>)
;;        |  (begin <dec> ...) 
;;        |  <exp>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The above is like the following:
;; <program> ::= (program <vardef>* <exp>)
;; <vardef>  ::= (define <var> <exp>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <aexp> ::= (lambda (<name> ...) <exp>)
;;         |  <number>
;;         |  <boolean>
;;         |  <string>
;;         |  <var>
;;         |  (void)



;; <cexp> ::= (<aexp> <aexp> ...)

;;;;;;;; not right now ;;;;;;;;;;;;;;;
;;         |  (if <aexp> <exp> <exp>)
;;         |  (set! <var> <aexp>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <exp> ::= (let ([<var> <cexp>]) <exp>)
;;        |  <aexp>
;;        |  <cexp>

(define (atom-eval benv store)
  (match-lambda  
    [(? lam? lam)
     (set (list lam benv))]
    [(? ae-list? l) (get-list-set l) ]
    [(? symbol? sym)
     ;(DEBUG sym)
     (if (equal? sym '$halt)
         (set '$halt)      
         (store-lookup store (benv-lookup benv sym)))] 
    
    [(? lam? lam)
     (set (list lam benv))]
    
    [(? kont-addr? ka)
     (store-lookup store ka)
     ]
    
    [(? number? num) (set num)]  
    [(? string? str) (set str)]
    [(? boolean? b)  (set b)]
    
    [(? void? v)     (set)]
    [(? empty? mt)   (set '$halt)]
    
    
    ;    [(? pyset? s)    (set s)]
    ;    [(? tuple? t)    (set t)]
    ;    [(? py-list? l)  (set l)]
    ;[(? dict? d)     (set d)] ; it seemst aht I should not make this dict a atomic in the cesk*! 
    
    ; 
    [(? set? s)     s]
    
    ))

; flag indicates that it is a oo-dict and builtinOO 

(define (get-vals k-lst v-lst benv store ostore flag)
 
  (define kv-paris (map (λ  (x y)
                          (list x y)) k-lst v-lst))
  
  (DEBUG kv-paris)
  
  (for/fold ([res-lst (list)])
    ([elem (in-list kv-paris) ])
    (if (or (equal? "__class__" (car elem)) (equal? "__base"  (car elem)));(equal? "__class__" (car elem))
        (append res-lst (list (set (second elem))))
        (if flag ; if it is for built-in-cls-dict
            (append res-lst (list (store-lookup ostore (benv-lookup benv (second elem)))))
            (append res-lst (list ((atom-eval benv store) (second elem))))))))

; next : state -> set[state]
; There seems to need a  global seen states!  
; no need to explore later on.
(define seen-states empty-set)

;; The following cases in next are the new types for var generated! 
;; state -> [state]
(define (next st)
  
  ; (DEBUG "the new state epxression:**********")
  ; ;(pretty-write (state-context st)
  
  (match-define (struct state (context benv store ostore kont-addr measure time)) st)
  
  (begin
    
    ;add the matched current state in the seen-states set  
    ; (set! seen-states (set-add seen-states st))
    (when (equal? context 'b39)
      (DEBUG "B39 caugt!!!!!!!!!!!!!!!!!!!"))
    
    (match context
      
      ; if there is an error, the abstract interpretetation should stop also!!! --->?
      [`(error ,aexp) 
       (DEBUG "----------- in error: -----------")
       empty]
      ;(next (make-state aexp2 benv store value-store  sstore ostore time))]
      
      ;(if <aexp> <cexp> <cexp>)
      [`(if ,aexp ,tcexp ,fcexp)  
       
       (DEBUG "----------- in if: -----------")
       
       (define condition ((atom-eval benv store) aexp))
       
       (when (set-empty? condition)
         (set! condition (store-lookup ostore (benv-lookup benv aexp)))
         )
       
       (when (set-empty? condition)
         (error "empty condition set in if"))
       
       (if (car (set->list condition)) ; should have used for/list
           (list (make-state tcexp benv store ostore kont-addr  measure time))
           (list (make-state fcexp benv store ostore kont-addr  measure time)))    
       ]
      
      
      [`(set! ,var ,ae)
       
       (DEBUG "----------- in set: -----------")
       (define old-val #f)
       
       (define new-val ((atom-eval benv store) ae))
       (when (set-empty? new-val)
         (set! new-val (store-lookup ostore (benv-lookup benv ae))))
      
       (define ostore* ostore)
       (define store* #f)
       ; though new contour updated for the var, but the next state should be new?
       ; so let's tick the time to pass to the next state.
       ;(define cur-time (get-current-time context))
       
       ;the new contour
       ;(define time* (tick cur-time time))
       (set! old-val
             (let ((tmp (set->list (store-lookup store (benv-lookup benv var)))))
               (if (empty? tmp) 
                   ; no value yet in the store.
                   '(void)
                   (set (last tmp))))) ; return the new value of the set!
       
       ; Strong update!
       (if (<=  (store-lookup measure (benv-lookup benv var)) 1)
           (begin
             (set! store* (store-update!! store (benv-lookup benv var) new-val))
             (when (OO-dict-set? new-val)
               (set! ostore* (store-update!! ostore (benv-lookup benv var) new-val)))
             )
           (begin
             (set! store* (store-update! store (benv-lookup benv var) new-val))
             (when (OO-dict-set? new-val)
               (set! ostore* (store-update! ostore (benv-lookup benv var) new-val)))
             )
           )
       
       (list (make-state old-val benv store* ostore* kont-addr measure time))
       ]
      
      [`(let ((,v ,call)) ,e)
       (DEBUG "----------- in let: -----------")
     
       ;(define cur-time (get-current-time context))
       
       ;the new contour
      ;(define time* (tick cur-time time))
       
       (when (or (equal? '_68 v) (equal? 'b70 v))
         (DEBUG "the set-field!!!!!!")
         (DEBUG call))
       
       (when (equal? 'b63 v)
         (DEBUG "containerdict!!!")
       )
       
       ;; cur-time is the current expression label to represent the current context for kontP
       (define new-kont-addr ((alloc time) context)) ;; cur-time is the expression label; allocate the kont address, which is the pair of context * time
       
       (define new-kont (set (letK v e benv kont-addr)))
       
       (define new-store (store-update!! store new-kont-addr new-kont)) ; I strong updte!!!
       
       (define measure* (measure-update! measure new-kont-addr))
       
       (define new-state (make-state call benv new-store ostore new-kont-addr measure* time))
       ; (DEBUG "next state after let: " new-state)
       ;;(pretty-write (state-context new-state))
       (list new-state)]
      
      [`(call/ec ,f)
       
       (DEBUG "************----------- in  call/ec: -----------***********")
       (DEBUG context)
       
       (define procs ((atom-eval benv store) f))
       
       (define cur-time (get-current-time context))
       
       ;the new time
       (define time* (tick cur-time time))
       
       (for/list ([proc (in-set procs)])
         (match proc
           [`((lambda (,v) ,exp) ,benv*)
            
            (define kaddr ((alloc time*) v))
            (define benv** (benv-extend benv* v kaddr))
            (define store* (store-update!! store kaddr (set (kontP kont-addr)))) ; here I strong update. 
            (define measure* (measure-update! measure kaddr))
            
            (make-state exp benv** store* ostore kont-addr measure* time*)]
           
           
           ['$halt empty]))
       ]
      
      
      [`(set-field! ,ae1 ,v ,ae2)
       
       
       (DEBUG "------------- in set-field! ----------------")
       (DEBUG context)
       
       
      ; (define cur-time (get-current-time context))
       ;the new time
       ;(define time* (tick cur-time time))
       
       (define oo-dict (get-elem-from-singleton-set (store-lookup ostore (benv-lookup benv ae1))))
       
       (define store* false)
       (set! store* store)
       
       (define vals ((atom-eval benv store) ae2))
       (when (set-empty? vals)
         (DEBUG "value to set is empty set)")
         (DEBUG vals)
         (set! vals ((atom-eval benv ostore) ae2))
         (DEBUG "after setting the vals again from ostore..")
         (DEBUG vals))
       
       (if (closure? oo-dict)
           ;class side effect
           (begin
             (DEBUG "----in set-field! closure oodict")
             (DEBUG  oo-dict)
             
             (let ((l-clses (store-lookup ostore (benv-lookup benv ae1))))
               
               (for ([l-cls (in-set l-clses)])
                 (set! store* (store-update! store (membPl l-cls v) vals))))
             )
           ;; instance side effect
           (begin
             (DEBUG "----in set-field! instance set-field!")
             (DEBUG  oo-dict)
             (set! store* (store-update! store (membPl oo-dict v) vals))
             (DEBUG (membPl oo-dict v))
             ))
       
       
       (list (make-state '(void) benv store* ostore kont-addr measure time))
       ]
      
      
      [`(get-field ,ae ,v) 
       
       (DEBUG "***************** in get-field ******************")
       (DEBUG context)
       
       ;(define cur-time (get-current-time context))
       ;the new time
       ;(define time* (tick cur-time time))
       
       ;(define oo-dict (get-elem-from-singleton-set ((atom-eval benv store) ae)))
       (define oo-dict (get-elem-from-singleton-set (store-lookup ostore (benv-lookup benv ae))))
       
       (when (empty? oo-dict)
         (let ((connected-var (get-elem-from-singleton-set ((atom-eval benv store) ae))))
           (set! oo-dict (get-elem-from-singleton-set (store-lookup ostore (benv-lookup benv connected-var))))))
      
       (DEBUG oo-dict)
       
       
       (define res-set (set))
       
       (if (closure? oo-dict) 
           ; class refernce
           (begin
             (let* ((l-cls (get-elem-from-singleton-set (store-lookup ostore (benv-lookup benv ae))))
                    (mro-lst (get-elem-from-singleton-set (store-lookup store (membPl l-cls "__mro__")))))
               
               (set! res-set (travel-mro mro-lst v "class" false false benv store ostore)))
             
             ) 
           
           ; instance reference
           (begin
             (let ((tmp-val (store-lookup store (membPl oo-dict v))))
               (if (set-empty? tmp-val)
                   ; current instance not found the field.
                   ; need to go through the mro list
                   (begin
                     
                     (let* ((v-cls (get-elem-from-singleton-set (store-lookup store (membPl oo-dict "__class__")))) ; e.g. 'OOdict53
                            (l-cls (get-elem-from-singleton-set (store-lookup ostore (benv-lookup benv v-cls)))) 
                            (mro-lst (get-elem-from-singleton-set (store-lookup store (membPl l-cls "__mro__"))))
                            )
                       
                       (DEBUG "instance reference")
                       ;(pretty-write v-cls)
                       ;(pretty-write l-cls)
                       ;(pretty-write mro-lst)
                       
                       (set! res-set (travel-mro mro-lst v "inst" oo-dict false benv store ostore)))
                     )
                   
                   ; found the field in the current instance dict! 
                   ; dinstinguish the function member and the value memeber! 
                   (begin
                     ; the (oo-dict v) can be mapped to a function or a value or multiple function before.
                     (let ((tmp-vals  (store-lookup store (membPl oo-dict v))))
                       (set! res-set 
                             (for/fold ([res-vals (set)])
                               ([val-elem (in-set tmp-vals)])
                               (if (closure? val-elem)
                                   (begin
                                     
                                     (DEBUG " a closure??")
                                     (DEBUG val-elem)
                                     
                                     (match (car val-elem)
                                       
                                       [`(lambda v void)
                                        (set-add  res-vals (list `(lambda () void) (second val-elem)))
                                        ]
                                       
                                       [`(lambda (,formals ...) ,body ...)
                                        (set-add  res-vals (list `((lambda ,formals ,@body) ,oo-dict) (second val-elem))) ;; this might be wrong!!!
                                        ]
                                       
                                       [else (error (format "~a NOT MATCHED in get-field's instance closure!" (car val-elem)))])
                                     )
                                   (begin
                                     (DEBUG "not a closure??")
                                     (DEBUG val-elem)
                                     (DEBUG res-set)
                                     (DEBUG tmp-val)
                                     (set-add res-vals val-elem) 
                                     ))))))))))
       (DEBUG "WHAT GET_FIELD reTuRN: ........")
       ;(pretty-write res-set)
       
       (list (make-state res-set benv store ostore kont-addr measure time))
       
       ]
      
      ;; class definition: class name will be bound to its dict location in ostore
      ;; You remember that this format is to set a OO dict so ae should be in the ostore
      [`((anf dict-set!) gbl-dict ,v-cls ,ae)
       
       ;(define cur-time (get-current-time context))
       
       ;the new time
       ;(define time* (tick cur-time time))
       
       (define ostore* (store-update!! ostore (benv-lookup benv v-cls) (store-lookup ostore (benv-lookup benv ae))))
       (define measure* (measure-update! measure (benv-lookup benv v-cls)))
       
       (list (make-state '(void) benv store ostore* kont-addr measure* time))
       ]
      
      [`((anf dict-ref) gbl-dict ,v-cls)
       (define res-set (store-lookup ostore (benv-lookup benv v-cls)))
       (DEBUG "in -dictref")
       (DEBUG res-set)
       
       (list (make-state res-set benv store ostore kont-addr measure time))
       
       ]
      
      ;; dict-ref is used only when ae is dict-locations.
      
      [ `((anf ,biop) ,ae1 ,ae2)
        (DEBUG "******************** in biop: ********************")
        (define possible-value #f)
        
        ;(define cur-time (get-current-time context))
        
        ;the new time
        ;(define time* (tick cur-time time))
        
        (define ae1-set ((atom-eval benv store) ae1))
        (when (equal? '$seq13 ae1)
          (DEBUG "VALUE?")
          (DEBUG ae1-set))
        
        (when (set-empty? ae1-set) ;; in the case of OOdict, will not affect arith-op
          (set! ae1-set (store-lookup ostore (benv-lookup benv ae1))))
        
        (when (equal? '$seq13 ae1)
          (DEBUG "VALUE?")
          (DEBUG ae1-set))
        
        (define ae2-set ((atom-eval benv store) ae2))
        
        (cond
          [(or (arith-ops? biop) (comp-biops? biop))
           
           ;(set! possible-value (best-effort-arith-value (last (set->list ae1-set))  (last (set->list ae2-set)) biop))
           (set! possible-value (map-set2 best-effort-arith-value ae1-set ae2-set biop))
           ;(list  (make-state possible-value benv store ostore kont-addr measure time*))
           (list  (make-state possible-value benv store ostore kont-addr measure time))
           ]
          
          [(container-biops? biop) ;ae1 is the container, ae2 is teh index
           ; index is singletone set
           (define index  (get-elem-from-singleton-set ae2-set))
           (set! possible-value (get-ref-vals ae1-set index store ostore biop))
           (list  (make-state possible-value benv store ostore kont-addr measure time))
           ]
          
          ; semantically, for container will apply the a proc to each element in the container.
          ; To get all the elements belonging to a dict, we do have to search the entire store to get the container values
          ; However, the search and match complexity is limited to linear! where N is the size of the store
          
          [(equal? biop 'for-container)
           (DEBUG "CAUGT FOR CONTAINER!!")
           (DEBUG "NO RULE YET, just to see what's there")
           (DEBUG context)
           
           (define base (get-elem-from-singleton-set ae1-set))
           (DEBUG "in anf " base)
           
           (define container-dict-lb (get-elem-from-singleton-set (store-lookup store (membPl base "__containerdict__"))))
           (define container-vals (get-orderedkey-vals (indexed-hash (get-raw-addrs store container-dict-lb)) store))
           
           ;; every for container is translated seperately, argumetns are singletone set
           (define proc (get-elem-from-singleton-set ae2-set))
           
           (if (or (py-list? base store ostore) (pyset? base store ostore) (tuple? base store ostore))
               (begin
                 (DEBUG "for container: eith list set or tuple!")
                 (DEBUG container-dict-lb)
                ; (DEBUG (get-elem-from-singleton-set ((atom-eval benv store) o-dict-lb)))
                 (DEBUG "elemfent valus;" container-vals)
                 (for/list ([elem (in-list container-vals)])
                   
                   (match proc
                     [`((lambda (,v) ,e) ,b)
                      (define binding ((alloc time) v))
                      (define benv* (benv-extend b v binding))
                      
                      (define store* (store-update! store binding elem))
                      
                      ;;; this line indicates I should really switchting to refactoring process!
                      ;; because the values returned in the container can be actually another oodict,which should stored in the ostore!
                     ; (define ostore* (store-update! ostore binding elem)) 
                      
                      (define measure* (measure-update! measure binding))
                      (define new-state (make-state e benv* store* ostore kont-addr measure*  time))
                      
                      new-state
                      ] 
           ))  
                 )
               (begin
                 (DEBUG "real dict!")
                 ))
             ]
          )
        
        ;(list  (make-state possible-value benv store ostore kont-addr measure time))
        
        ]
      
      
      ;; dict always bound to a gensym first
      ;; in this case, the dict location binding time is different from.
      [`(dict (,v-k ,ae-v) ...)
       
       (DEBUG "************** in dict ******************")

         
     
       ;(define cur-time (get-current-time context))
       ;the new time
       ;(define time* (tick cur-time time))
       
       (define loc false)
       
       (if (OO-dict? v-k ae-v)
           (set! loc (gensym 'OOdict))
           (set! loc (gensym 'dict)))
       
              
       
       (define loc-s (set loc))
       
       (cond
         ; OO-dict?: class dict and inst dict
         [(OO-dict? v-k ae-v)
          
          (DEBUG "OO dict!!!")
          (DEBUG context)
          
          (define memb* (map (alloc-membP loc) v-k))
          (define mem-val* (get-vals v-k ae-v benv store ostore #f))  ;before, this line's last argument is #f
          (DEBUG "OO's value to set: ")
          (DEBUG mem-val*)
          (define store* (store-update!* store memb* mem-val*))
          (list (make-state loc-s benv store* ostore kont-addr measure time))
          ]
         
         ; built-in class dict
         [(builtin-cls-in-dict? v-k)
        
          (DEBUG "builtint dict!!!")
          (define memb* (map (λ (x) (benv-lookup benv x)) v-k))
          (define mem-val* (get-vals v-k ae-v benv store ostore #t))
          (define ostore*    (store-update!* ostore memb* mem-val*))
          
          (define measure*   (measure-update!* measure memb*))
          (list (make-state loc-s benv store ostore* kont-addr measure* time))
          ]
         
         ; ordinary dict
         [ else
           (DEBUG "ordinary dict")
           (define memb* (map (alloc-membP loc) v-k))
           (define mem-vals* (get-vals v-k ae-v benv store ostore #f))
           (define store* (store-update!* store memb* mem-vals*))
           (list (make-state loc-s benv store* ostore kont-addr measure time))
           ]
         )
       ]
      
      
      ; uni op
      [`((anf ,unop) ,ae)
       (DEBUG (format "------ in unop------: ~a" unop))
       
       (define ret-abstract-value-set #f)
       (define store* #f)
       
      
         
       ;(define cur-time (get-current-time context))
       
       ;the new time
       ;(define time* (tick cur-time time))
       
       (define ae-val ((atom-eval benv store) ae))
       
       
        (when (equal? ae 'tmp-dict27)
          (DEBUG "tmp-dict27 is : ")
          (DEBUG ae-val))
       
       (when (set-empty? ae-val)
         (set! ae-val (store-lookup ostore (benv-lookup benv ae))))
       
       (set! ret-abstract-value-set (map-set-uniop compute-each ae-val unop store ostore))
       
       (list (make-state ret-abstract-value-set benv store ostore kont-addr measure time))]
      
      
      ; triop py-list-set! (dict-set! will be matched first above)
      ; base is supposed to be var
      ; I have strong update directly on the dict member
      [`((anf ,triop) ,base ,index ,ae-val)
       (DEBUG "---------- in triop--------------")
       (DEBUG context)
       
       (define cur-time (get-current-time context))
       ;the new time
       ;(define time* (tick cur-time time))
       
       (define store* store)
       
       ; the index is singleton set in the py-list-set! and dict-set!
       (define indx (get-elem-from-singleton-set ((atom-eval benv store) index)))
       
       (define base-set ((atom-eval benv store) base))
       
       (when (set-empty? base-set)
         (set! base-set (store-lookup ostore (benv-lookup benv base))))
       
       ; the new-val is also singleton set
       (define new-vals ((atom-eval benv store) ae-val))
         ;(get-elem-from-singleton-set ((atom-eval benv store) ae-val)))
       ;(define new-base-val-set (compute-from-base-set ((atom-eval benv store) base) indx new-val)) 
       
       (cond 
         [(equal? triop 'dict-set!)
          (for ([d-l (in-set base-set)])
            (set! store* (store-update!! store* (membPl d-l indx) new-vals)))] ;;;strong update! 
         [(equal? triop 'py-list-set!)
          
          (for ([d-l (in-set base-set)])
            
            (let ((membdl (membPl d-l "__containerdict__")))
              (if (hash-has-key? ostore membdl)
                  (let* ((mem-dict-l (get-elem-from-singleton-set (store-lookup ostore membdl)))
                         (memb-to-set (membPl mem-dict-l indx)))
                    (if (hash-has-key? ostore memb-to-set)
                        (set! store* (store-update!! store* memb-to-set new-vals))
                        (error (format "py-list-set!: does not have the field: ~a, let alone membP:~a" indx memb-to-set))
                        ))
                  (error (format "py-list-set!: does not have __container__ field!")))))
          ])
       
       (list (make-state '(void) benv store* ostore kont-addr measure time))
       ]
      
      ;; originally in ae rule, but added here to distinguish the pattern mactching from function application!
      [(or 
        `(lambda (,formals ...) ,exp)
        `(lambda ,formals ,exp)) ; not yet tested
       
       (DEBUG "---------------in ---LAMBDA----------------------")
       
       
       (define konts ((atom-eval benv store) kont-addr))
       
       (define val ((atom-eval benv store) context))
       
       (for/list ([kont (in-set konts)])
         (match kont
           [`(,v ,e ,b ,k) 
            (define binding ((alloc time) v))
            (define benv* (benv-extend b v binding))
            
            (define store* (store-update! store binding val))
            (define measure* (measure-update! measure binding))
            (define new-state (make-state e benv* store* ostore k measure*  time))
            
            new-state
            ] 
           ))
       ]
      
      [(or `(void) 'void)
       (DEBUG "---------------in ---void----------------------")
      
       (define konts ((atom-eval benv store) kont-addr))
       
       ; (define val ((atom-eval benv store) context))
       
       (for/list ([kont (in-set konts)])
         (match kont
           [`(,v ,e ,b ,k) 
            (define binding ((alloc time) v))
            (define benv* (benv-extend b v binding))
            (define store* (store-update! store binding (set)))
            (define measure* (measure-update! measure binding))
            (define new-state (make-state e benv* store* ostore k measure* time))
            new-state
            ]
           
           ['$halt empty]
           ))
       
       
       ]
      
      
      [`(list ,e ...)  
       (DEBUG "----------- in llllist: -----------")
       
       (DEBUG context)
       (define konts ((atom-eval benv store) kont-addr))
       
       (define val ((atom-eval benv store) context))
       ;(define val (set context))
       ;(DEBUG "**************")
       ;(DEBUG val)
     
       
       (for/list ([kont (in-set konts)])
         (match kont
           [`(,v ,ce ,b ,k) 
            (define binding ((alloc time) v))
            (define benv* (benv-extend b v binding))
            (define store* (store-update! store binding val))
            (define measure* (measure-update! measure binding))
            (define new-state (make-state ce benv* store* ostore k measure* time))
           
            new-state
            ]
           
           ['$halt empty]
           ))
       ]
      
      [`(,f ,args ...)
       
       (DEBUG "---------------- in function app -----------------")
       (DEBUG context)
       
       (define procs ((atom-eval benv store) f))
       
       (when (and (equal? f 'tuple) (equal? (first args) 'b63))
         (DEBUG "TUPLE APPLIED to B63!")
         ;(define params  (map (atom-eval benv store) args))
         (DEBUG (map (atom-eval benv store) args))
         )
       
       (when (equal? f 'b15)
         (DEBUG "in applying b15 to tmpdcit20")
         (DEBUG procs)
         (DEBUG (map (atom-eval benv store) args))
         )
       
       (when (equal? f 'b60)
          (DEBUG "in applying b60 to b64 b67")
         
         (DEBUG (map (atom-eval benv store) args)))
       
       (when (and (equal? f 'return) (equal? (first args)  '$localo21))
         (DEBUG "in applying return locallo21")
         ;(DEBUG (map (atom-eval benv ostore) args))
         (DEBUG "begin to print procs;")
        ; (pretty-write procs)
         (DEBUG "finished rhe proce"))
         
         
       
       (define cur-time (get-current-time context))
       ;the new time
       (define time* (tick cur-time time))
       
       (for/list ([proc (in-set procs)])   
         
         (match proc
           [ `((lambda (,formals ...) ,exp) ,benv*)
             
             ;(define params  (map (atom-eval benv store) args))  ; This line should be buggy!
             (define params (map (lambda (x) 
                                   (let ((tmp-res ((atom-eval benv store) x)))
                                     (if (set-empty? tmp-res)
                                         (begin
                                           (DEBUG "one of the function app args return empty set in store eval: ")
                                           (DEBUG x)
                                           (DEBUG "value changed to find in ostore eval: ")
                                           (DEBUG ((atom-eval benv ostore) x))
                                           ((atom-eval benv ostore) x))
                                         (begin
                                           (DEBUG "arg found in store eval")
                                           (DEBUG tmp-res)
                                           tmp-res)))) args))
                                           
                                           
             (DEBUG "parameters in the function app " params)
             (DEBUG formals)
             
             (define bindings (map (alloc time*) formals))
             (define benv**   (benv-extend* benv* formals bindings))
             (define store*   (store-update!* store bindings params))
             (define measure* (measure-update!* measure bindings))
             (make-state exp benv** store* ostore kont-addr measure* time*)]
           
           [`(((lambda (,formals ...) ,exp) ,obj-dict) ,benv*)
            
            (DEBUG "*************** in partial lambda!!!! ***********")
            (DEBUG proc)
            
            (define oo-val  (set obj-dict))
            (define params  (map (atom-eval benv store) args))
            (define params* (append (list oo-val) params))
            
            (define bindings (map (alloc time*)  formals))
            (define benv**   (benv-extend* benv*  formals  bindings))
            
            (DEBUG "before the store is:")
            ;;(pretty-write ((atom-eval benv** store) 'self20))
            
            (define store*   (store-update!* store (cdr bindings) params))
            (define ostore*  (store-update! ostore (car bindings) oo-val))
            
            (when (equal? (length args) (length formals))
              (set! store* (store-update!* store (cdr bindings) (cdr params)))
              (set! ostore* (store-update! ostore (car bindings) (car params))))
               
            
            (define measure* (measure-update!* measure bindings))
            ;(pretty-write exp)
            ;(pretty-write benv**)
            ;;(pretty-write ((atom-eval benv** store*) 'self20))
            ;(pretty-write kont-addr)
            
            (make-state exp benv** store* ostore* kont-addr measure* time*)] 
           
            [`((lambda () void) ,benv*)
             (DEBUG "--------- app: in obejct lambda --------------")
             
             (make-state '(void) benv* store ostore kont-addr measure time*)
             ]
           
           ; work with call/cc
           [`(,letk-addr)  
            
            (when  (equal? f 'return)
               (DEBUG "RETURN'S 909099090909 value: ")
              (DEBUG letk-addr)
               (DEBUG proc))
            
            (define letks ((atom-eval benv store) letk-addr))
            
            (DEBUG (format "call/ec proc: ~a" letks))
            
            (for/list ([letk (in-set letks)]) 
              (match letk
                [`(,v ,exp ,env ,ka)
                 
                 (define baddr ((alloc time*) v))
                 (define benv* (benv-extend env v baddr))
                
                 (define callec-arg (car args))
                 (define val ((atom-eval benv store) callec-arg))
;                 (when (set-empty? val)
;                   (set! val ((atom-eval benv ostore) callec-arg)))
                 (cond 
                   [( set-empty? val) 
                    ; then it must be in the OO-dictet
                    
                    (set! val (store-lookup ostore (benv-lookup benv callec-arg)))
                    (when (equal? callec-arg '$localo21)
                      (DEBUG "v = " v) 
                      (DEBUG "$localo21 VAL = " val))
                    
                    (define ostore* (store-update! ostore baddr val))
                    (define measure* (measure-update! measure baddr))
                    (make-state exp benv* store ostore* ka measure* time*)
                    ]
                   
                   [else
                    (DEBUG "call/ec: the callec-arg's atomeval in store is not empty set")
                    (define store* (store-update! store baddr val))
                    (define measure* (measure-update! measure baddr))
                    (make-state exp benv* store* ostore ka measure* time*)
                    ]
                   )
                 ]))]
           
           ['$halt empty]            
           [else (error (format "in app: NO matching for ~a " proc))]))
       ]
      
      [`,ae  ; ae is buggy1!! it returns the same state back!!!!
       
       (DEBUG "----------- in ae: -----------")
       ;(pretty-write ae)
       (when (equal? ae 'b39)
           (begin
             (DEBUG ae)
             ))
       
       (define ostore* ostore)
       (define store* store)
       
       ; it seems that the atomic expression does not have to tick time? but using the current time?
       (cond 
         [(atom? ae)
          
         ; (define konts ((atom-eval benv store) kont-addr))
          (define konts (store-lookup store kont-addr))
          
         
            (DEBUG "                     kont.......")
            (DEBUG kont-addr)
            (DEBUG "                     konts after atomeval .......")
            ;(pretty-write konts)
            
          (define val ((atom-eval benv store) ae)) 
          
          (DEBUG "what's the stupid ae????")
          (DEBUG val)
          
          ; (define val-o (store-lookup ostore (benv-lookup benv ae)))
          
          (cond
            [  (set-empty? val) ;(and (set-empty? val) (set? ae) (not (set-empty? ae))) 
              (let ((val-o (store-lookup ostore (benv-lookup benv ae)))) 
                (if (set-empty? val-o)
                    (begin
                      (set)
                      )
                    (set! val val-o)))
              ]
            [(not (set-empty? val)) val])
          
          (for/list ([kont (in-set konts)])
            
            (match kont
              [`(,v ,e ,b ,k)
               
               ; (define cur-time (get-current-time context))
                ;the new time
                ;(define time* (tick cur-time time))
               
               (when (equal? v 'b64)
                 (DEBUG "NO THIS LINE??????????")
                 (DEBUG val))
               
               (define binding ((alloc time) v))
               (define benv* (benv-extend b v binding))
               
               ;(define measure* (measure-update! measure binding))
               (define measure* #f)
               
               (define store* store)
               (define ostore* ostore)
               
               ;(define store* (store-update! store binding val)) ; 
               
               (cond 
                 [(OO-dict-set? val) 
                 
                  ;(set! ostore* (store-update! ostore binding val))
                  (define counting (store-lookup measure (benv-lookup benv* v)))
                  (if (and (set? counting) (set-empty? counting))
                      (begin
                      (set! counting 0)
                      
                      (set! measure* (measure-update! measure binding))
                      )
                      ; number
                      (begin
                        (set! measure* measure)))
                       
                  
                  (if (<=  counting 1) 
                      (set! ostore* (store-update!! ostore binding val))
                      (set! ostore* (store-update! ostore binding val)))
               
                  ]
                 
                 [else
                    
                     (define counting (store-lookup measure (benv-lookup benv* v)))
                  (if (and (set? counting) (set-empty? counting))
                      (begin
                      (set! counting 0)
                      
                      (set! measure* (measure-update! measure binding))
                      )
                      ; number
                      (begin
                        (set! measure* measure)))
                  
                   (if (<=  counting 1)
                       (begin
                         (DEBUG "strong update!")
                         (set! store* (store-update!! store binding val)))
                       (begin
                         
                         (DEBUG "NO strong update!")
                         (set! store* (store-update! store binding val))))
                  ])
               
               (define return-state (make-state e benv* store* ostore* k measure* time))
               (if (equal? return-state st) ; this is not solving problem! since there should be other to do states but here not!!!!
                   empty
                   return-state)
               ]
              
              ['$halt empty]
              ))
          ]
         [else (DEBUG "NOT ATOM")
               empty])
       ]
      [_ empty]
      )))


; analyze : exp -> mono-summary
(define (analyze exp initial-benv initial-store initial-ostore initial-measure) 
  ; (define init-state (make-state exp initial-benv empty-store  empty time-zero))
  (define init-state (make-state exp initial-benv initial-store initial-ostore `$halt initial-measure time-zero)) 
  
  ;(next init-state) 
  ;(define summary (summarize seen-states))
  
  (define states (explore empty-set (list init-state)))
  
  (display "Number of States = ") (displayln (length (set->list states)))
  (newline)
  (displayln "Types Info --- ")
  (newline)
  (define summary (summarize states))
  
  ; (define mono-summary2 (monovariant-store2 (summarize-v seen-states) (summarize-o seen-states)))
  
  (define mono-summary (monovariant-mstore summary))
  
  
  mono-summary)

; (Set seen) -> (list todo) -> (Set states)

(define (explore seen todo) 
  
  (match (flatten todo) ; in the function call, it is possible to return a list of list of states. but we just flatten all the states.
    [(list)
     seen]
    [(list-rest 
     ; (? (curry cesk-equal? seen)) 
      (? (curry seen? seen));(? (curry cesk-equal? seen)) 
      todo) ; The states are essentially not comparable. 
     (DEBUG "state seen!")
     (explore seen todo)]
    
    [(list-rest st0 todo)    
     (define succs  #f)
     (if (empty? st0)   ; final state. no more exploration
         (begin
           seen
         )
         (begin
           (set! succs (next st0))
           
           (when (equal? (state-context st0) 'b39)
             (DEBUG "add new B39 to hte seen state")
             (DEBUG (length (set->list seen)))
             (DEBUG (length succs))
             (DEBUG (set-member? (list->set succs) st0)) ;;;;;;;;;;; OMG. The success state to add to todo is the same as 
             )
           (explore (set-add seen st0)
                    (append succs todo)))
         )
]))


;  (parameterize ([k 1])
;    (print-mono-summary (analyze cleaned-ex)))
(define final-res #f)

;; The top level entry point. The input is the p4 CPSed program
(define (infer-types-program program)
  (define prog (match program [`(program . ,stmts) stmts]))
  (define defs (get-defs prog))
  
  (set! prog (kcfa-skip-def prog))
  
  (parameterize ([k 2])   ;; k =10 works for ttt.py.anormalnt
    
    (define initial-benv (init-benv defs))
    (set! final-res (analyze prog initial-benv (init-store initial-benv) (init-ostore initial-benv) (init-measure initial-benv)));(get-defs prog))))
    (print-mono-summary final-res))
  ;(print-mono-summary (analyze prog)))
  )

;; non used code
(define (analyze-mro cls runtime-dict benv store)
  (DEBUG "-----analyze-mro---------")
  ;(pretty-write runtime-dict)
  ; ;(pretty-write ((atom-eval benv store) (car cls))))
  ;(pretty-write (hash-ref runtime-dict (car ((atom-eval benv store) (car cls)))))
  (let ((res 
         (append (list cls)
                 (merge
                  (append 
                   (map  (lambda (x) (analyze-mro x runtime-dict)) (hash-ref (hash-ref runtime-dict cls) "__base__"))
                   (list (hash-ref (hash-ref runtime-dict cls) "__base__")))
                  (list)
                  ))))
    ;(hash-set! gbl-dict "__mro__" res)
    res
    ))

;;Input helpers
;; read input from input from stdin or a file
(define (read-all port)
  (let ((next (read port)))
    (if (eof-object? next)
        '()
        (cons next (read-all port)))))

(define (pycesk-file filename)
  (pycesk-port (open-input-file filename)))

(define (pycesk-port port)
  (let* ([lines (read-all port)])
    (when (not (empty? lines) )        
      (infer-types-program (car lines))
      )))
;
(define args (current-command-line-arguments))
(match args
  [(vector filename)   (pycesk-file filename) (exit)]  
  [else  (pycesk-port (current-input-port))]) 

;(pycesk-file "./experiment/experiment1.anormal.rkt")
;(pycesk-file "./experiment/unittest-nestedlet.anormal.rkt")
;(pycesk-file "./experiment/unittest-set.anormal.rkt")
;(pycesk-file "./experiment/unittest-simpleif.anormal.rkt")
;(pycesk-file "./experiment/unittest-simplebiops.anormal.rkt")
;(pycesk-file "./experiment/unittest-unop.anf.rkt")
;(pycesk-file "./experiment/unittest-begin.rkt")
;(pycesk-file "./experiment/unittest-funcall.anormal.rkt")

;this deals with the recursive progarm! not tested now!
;(pycesk-file "./experiment/unittest-fact.anormal.rkt")
;(pycesk-file "./experiment/unittest-tailrecur.anf.rkt")

;(pycesk-file "./experiment/unittest-acset.anf.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(pycesk-file "tests/tt0.py.anormalnt")
;(pycesk-file "./examples/testforin.py.anormal")
