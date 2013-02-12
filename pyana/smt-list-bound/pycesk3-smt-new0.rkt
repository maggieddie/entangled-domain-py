(module pycesk3
  racket/base
  (require "utils_cesk3_smt.rkt")
  (provide (all-defined-out))
  (require racket/pretty)
  (require racket/match)
  (require racket/list)
  (require racket/set)
  (require racket/local)
  (require racket/bool)
  (require racket/function)
  (require "smt-list-bound-new.rkt")
  (require "spww-poly-jumping.rkt")
  
  (define pw? #t)
  
  ;; Abstract interpretation based analyzer for ANF Python code
  ;; support Python dynamic class system, collections and comprehensions
  
  
  ;; apply :: kont-addr -> D -> Store -> OStore -> Measure -> time -> [state]
  (define (apply kont-addr d store ostore  measure time govenor-base sym-meta-info)
   (DEBUG " d's value to apply is: " (set-count d))
    (local ([define konts (store-lookup store kont-addr)])
      ;(displayln "kont-length") (displayln (set-count konts))
      ;(for/list ([kont (in-set konts)])
     ; (define kont (get-elem-from-singleton-set konts))
      (displayln (set-count konts))
      (flatten 
        (for/list ([kont (in-set konts)])
          (cond
            [(letK? kont) 
           (displayln "letk")
           (match-define (struct letK (v e benv kaddr)) kont)
          
           (define binding ((alloc-bind time) v))
           (define benv*  (benv-extend benv v binding))
           (define store* (store-update store binding d))
          ; (define measure* (measure-update measure binding))
           
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;; added to support theorem provers
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
           (cond
             [(symbolic-len? d)
              (displayln (format "in len? ~a: binding ~a" d binding))
              
              (update-smt-symbol-table! binding sym-meta-info)
              ]
             
             [(predicate? d)
                (update-smt-symbol-table! binding sym-meta-info)]
             
             [else
              (define int-singleton-test-res (singleton-type-set? d 'int))
              (define oloc-singleton-test-res (singleton-type-set? d 'oloc))
              (define addr-val (singleton-type-set? d 'addr))
              
              ;; for int
              (deal-with-single-int #f #f #f binding int-singleton-test-res)
              (deal-with-single-addr #f #f #f binding addr-val)
              ;(deal-with-single-int #f #f #f binding addr-val)
;           (cond
;                [(not (equal? int-singleton-test-res #f)) 
              ;                 (define smt-var (gen-smt-var))
;                 (add-formula! `(smt:declare-fun ,smt-var  ()  Int))
;                 (add-formula! `(smt:assert (=/s ,smt-var ,int-singleton-test-res)))
;                 (update-smt-symbol-table! binding (sym-meta smt-var 'Int))
;                 ]
              ;                [else (void)])
              (cond
                [(not (equal? oloc-singleton-test-res #f ))
                 
             
                 
                 ; continue to find the container dict case since for both fict and the relist they will return oloc)
                 (define objMap (store-lookup ostore oloc-singleton-test-res)) ; this is singleton
                 ;; lookinto the container
                 (define locs-val (obj-lookup* (set "__containerdict__") (set oloc-singleton-test-res) ostore))
                 (cond
                   ;; (dict alloc)
                   [(set-empty? locs-val)
                    (displayln "containerdict is found to be empty!!!!")
                    (cond 
                      [(equal? sym-meta-info #f) 
                       (displayln "empty so do nothing!!!!")
                       (void)]
                     [else
                       (update-smt-symbol-table! binding sym-meta-info)])]
                   ; container object oloc
                   [else 
                    ;; if the container dict is found, then we should look at the 
                    ;; oloc, find its smt stymbol and then bind the v to the same smt sym
                      (displayln "containerdict container Found!!!!")
                      (define filtered-oloc  (singleton-type-set? locs-val 'oloc))
                      (cond
                        [(not (equal? filtered-oloc #f))
                        
                         (define smt-sym-res (addr-in-smt-sym-table? filtered-oloc))
                       
                         
                         (cond
                           [(not (equal? smt-sym-res #f))
                           
                            (update-smt-symbol-table! binding smt-sym-res)
                           
                            ]
                           [else 
                            
                            (void)])
                         ]
                        [else 
                        
                         (void)])])])])
           
           (list  (state e benv* store* ostore measure  kaddr  time govenor-base ))]
          
          [(forK? kont)
           (displayln "forK")
           (match-define (struct forK (v-k val remaining-seq exp be last-kaddr)) kont)
           (cond 
             [(empty? remaining-seq) 
              (define binding ((alloc-bind time) v-k))
              (define benv*  (benv-extend be v-k binding))
              (define store* (store-update store binding val))
              (define measure* (measure-update measure binding))
              (list (state exp benv* store* ostore measure* last-kaddr time govenor-base))]
             [else
              (define binding ((alloc-bind time) v-k))
              (define benv*  (benv-extend be v-k binding))
              (define store* (store-update store binding val))
              (define measure* (measure-update measure binding))
              (define next-fork-addr ((alloc-kont time) (new-kont-exp kont-addr)))
              (define new-fork (forK v-k (car remaining-seq) 
                                       (rest remaining-seq) exp be last-kaddr))
              (define store** (store-update store* next-fork-addr (set new-fork)))
              (define measure** (measure-update measure* next-fork-addr))
              (list (state exp benv* store** ostore measure**  next-fork-addr time govenor-base ))])]
          ['$halt (displayln "halt") empty]
          [else (DEBUG "WARNING: not applying values to either or continuation?!")])))))
  
  ;; travel-mro :: [VAR] -> VAR -> String -> OLOC -> OLOC -> benv -> store -> ostore -> D
  ;; this part it is harder to do in the compile time
  (define (travel-mro var-clses var-fld str-type loc-inst loc-super benv store ostore)
    (cond 
      [(empty? var-clses)  (error "referencing unexisted field")]
      [else
       (define cls-cls ((atom-eval benv store) (car var-clses)))
       (define key-set ((atom-eval benv store) var-fld))
       (for/fold ([res-set (set)])
         ([cls (in-set cls-cls)])
         (match cls
           [(struct Cls (cl oloc))
            (local ([define val (obj-lookup key-set (ostore-lookup ostore oloc))])
              (cond
                [(set-empty? val) (travel-mro (rest var-clses) var-fld str-type loc-inst loc-super benv store ostore)]
                [else
                 (cond
                   [(equal? str-type "class") (set-union res-set val)]
                   [(equal? str-type "inst") (set-union res-set (transform-methods-in-inst-fld-reference val loc-inst))]
                   [(equal? str-type "super") (set-union res-set (transform-methods-in-inst-fld-reference val loc-super))]
                   [else (DEBUG "travel mro's tr-type not in class, inst, nor super?") val])]))]
           [else 
            (DEBUG "in travel-mro, and the reference to the cls in the store is not Cls struct!") 
            (error "in travel-mro, and the reference to the cls in the store is not Cls struct!")]))]))
  
  
   ; ********************************* helpers for the ordinary lambda matching **********************
  ;; just avoid code dupilicates which appear in both ordinary lambda class lambda case in function app
  ;; ordinary-lambda-bind :: exp -> store -> ostore -> measure -> kont-addr ->time -> state
  (define (ordinary-lambda-bind clo args env store ostore measure kont-addr time govenor-base)
    ;(DEBUG "lambda" (closure-lambda clo))
    ;(DEBUG "benv*"  (closure-benv clo))
     (match clo
       [(struct closure (lam benv*))
        (match lam
          [`(lambda (,formals ...) ,exp)
           (DEBUG " app matched normal lambda " (closure-lambda clo))
           (define params (map (atom-eval env store) args))
           (define bindings (map (alloc-bind time) formals))
          ; (displayln (format "could return be here~a" formals))
          ; (displayln (format "bindings ~a" bindings))
          ; (pretty-write  (hash-keys store))
           (define benv**  (benv-extend* benv* formals bindings))
           (define store* (store-update* store bindings params))
         ; (displayln  "after bindings ~a" )
         ;  (pretty-write (hash-keys store*))
           ;(define measure* (measure-update* measure bindings))
           
                   
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           ;;;; added to support theorem provers
           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           
           (define arg-len (length args))
           (cond 
             [(and (= arg-len 1)
                   (equal? (car args) 'lst))
              (displayln "caught the List b83 call site")]
             [else (void)])
           
         (deal-with-func-call-binding bindings params env store args)
         (state exp benv** store* ostore  measure kont-addr time govenor-base)])]))
 
 
  ;******************************* transition rules *******************************************
  ;; next :: state -> [state]
  (define (next st)
    (match-define (struct state (exp benv store ostore measure kont-addr time govenor-base)) st)
   
  
    (match exp
      ;; if there is an error, the analyzer should also stop?
      [`(error ,ae) (DEBUG " in error ") empty]
      
      ;; condition
      [`(if ,ae ,tce ,fce) 
       (DEBUG " in if " exp)
        
    
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;;; added to support theorem provers
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; ae is supposed to be some predicate that is to put into hte 
       ; it is a singleton set with predicate indication
       ; and we are suppoed to find its smt meta information in the table
       (define ae-cond-set ((atom-eval benv store) ae))
        (define pred-res (predicate? ae-cond-set))
       (define pred (addr-in-smt-sym-table? (benv-lookup benv ae)))
;      
       (cond 
         [( = (set-count ae-cond-set) 1)
          (define condition (first (set->list ae-cond-set)))
          (cond 
            ;; for other boolean operation result  bound to ae
            [(equal? condition #t)
              ;(define new--t (set-add  pred)) 
              (list  (state tce benv store ostore measure  kont-addr time govenor-base )
                    )]
              ;; for other boolean operation result  bound to ae
            [(equal? condition #f) 
            ; (define new--f (set-add  neg-predicate))
              (list (state fce benv store ostore measure kont-addr time govenor-base))]
            ;; for binary compare operations
            [(equal? condition 'predicate)
              (define pred (addr-in-smt-sym-table? (benv-lookup benv ae)))
               (define neg-predicate `(not ,pred))
              (displayln (format "the condition is predicate ae ~a: ~a!!!" ae pred))
               (define new-govenor-base-t (set-add govenor-base pred )) 
               (define new-govenor-base-f (set-add govenor-base neg-predicate ))
                (list 
                 (state tce benv store ostore measure  kont-addr time new-govenor-base-t)
                 (state fce benv store ostore measure kont-addr time new-govenor-base-f))]
             
            )]
         [else
          (list 
           (state tce benv store ostore measure  kont-addr time govenor-base)
           (state fce benv store ostore measure  kont-addr time govenor-base))])]
       

      
      [`(set! ,var ,ae)
       (DEBUG " in set! " exp)
       
       (local ([define new-val ((atom-eval benv store) ae)])
         ;(define store* (store-update store (benv-lookup benv var) new-val)) ;--> before measure compoenent added
         
                            
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;; added to support theorem provers
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (displayln (format "in set! :~a " exp))
         
         
         (define oloc-res (singleton-type-set? new-val 'oloc))
         
         (define int-val (singleton-type-set? new-val 'int))
         (define addr-val (singleton-type-set? new-val 'addr))
         
       
         
         ;(deal-with-single-oloc #t var benv #f oloc-res)
         ;(deal-with-single-int #t var benv #f int-val)
         (deal-with-single-abstract-type #t var ae benv #f oloc-res int-val addr-val)
         
         (deal-with-sym-len-or-pred #t var benv store #f ae 'symbol-len)
         (deal-with-sym-len-or-pred #t var benv store #f ae 'predicate)

  ;   (define store* (proper-update var new-val benv store measure))
         (define store* (store-update  store (benv-lookup benv var) new-val))
         (apply kont-addr (set)  store* ostore  measure time govenor-base #f))]
      
      ;; let expression
      [`(let ((,v ,ce)) ,e)
       (displayln (format " in let: ~a " v))
       (define time* (tick (get-current-time exp) time))
     
       (define new-kont-addr ((alloc-kont time*) exp))
       (define new-kont (letK v e benv kont-addr))
       (define new-store (store-update store new-kont-addr (set new-kont)))
      ; (define measure* (measure-update measure  new-kont-addr))
       (list (state ce benv new-store ostore measure  new-kont-addr time*  govenor-base))]
      
      ;; call/ec
      [`(call/ec ,f)
       (DEBUG " in call/ec " exp)
       (define procs ((atom-eval benv store) f))
       (define time* (tick (get-current-time exp) time))
       (for/list ([proc (in-set procs)])
         (match proc
           [(struct closure (lam benv*))
            (match lam
              [`(lambda (,v) ,exp)
               (define kaddr ((alloc-bind time*) v))
               (define benv** (benv-extend benv* v kaddr))
              
               (displayln (format "the value of the kaddr is ~a ~a" (hash-has-key? store kaddr) (set-count (store-lookup store kaddr))))
               
               (define store* (store-update store kaddr (set (kontP kont-addr))))
               ;(define measure* (measure-update measure kaddr))
               (state exp benv** store* ostore measure kont-addr time* govenor-base )])]))]
      
      ;; set-field!
      [`(set-field! ,ae-b ,ae-f ,ae-v)
     
       (DEBUG " in set-field! " exp)
       (define val-set ((atom-eval benv store) ae-v))
       (define field-set ((atom-eval benv store) ae-f))
       (define locs ((atom-eval benv store) ae-b))
       (define ostore* 
         (foldr (λ (loc os)
                  (ostore-update ostore loc (obj-update field-set val-set (ostore-lookup ostore loc))))
                ostore
                (pull-locs locs)))
       (apply kont-addr (set) store ostore* measure  time govenor-base #f)]
      
      ;; in dict-set! for class deifinition
      [`((anf dict-set!) gbl-dict ,v-cls ,ae)
        
       (DEBUG " in " exp)
       (when (equal? v-cls 'tuple) (DEBUG "Yes, we are setting the tuple dict"))
       (define oloc ((atom-eval benv store) ae))
       ;; the following cond is for testing purpose
       (match (get-elem-from-singleton-set oloc)
         [(struct OLoc (e t)) (DEBUG "Yes, it is a oloc in the gbl-set! " oloc)]
         [else (DEBUG "oops, not an location!") (error "sorry, no need to continue since class allocation failed!")])
       ;; in my scheme, every time the oloc returns, it is a singelton set! so another test:
       (local ([define num-oloc (set-count oloc)])
         (cond
           [(= 1 num-oloc) (DEBUG "Yes, it is the only location allocated ")]
           [(< 1 num-oloc) (DEBUG "multiple olocs mapped from the same classname!")]
           [(> 1 num-oloc) (DEBUG "ridiculous result! nothing returned from oloc") (error "nothing returned at the oloc!")]))
       (define store* (store-update-cls store (benv-lookup benv v-cls) (get-elem-from-singleton-set oloc))) ; NOTE: This line may not be 
       ; it makes sense to apply the empty set for any *-set! expression, since there is no need to use the let bound var at the end!
       (apply kont-addr (set) store* ostore measure time govenor-base  #f)]
      
      ;; in dict-ref gbl-dict
      [`((anf dict-ref) gbl-dict ,v-cls) 
       (DEBUG " should elminate this one! in dict-ref gbl-dict?" exp)
       (define res-set ((atom-eval benv store) v-cls))
      
       (DEBUG "the class values: " res-set)
       (apply kont-addr res-set store ostore measure  time govenor-base  #f)]
      
      ;; in biop
      [`((anf ,biop) ,ae1 ,ae2)
      
       (DEBUG " in biop " exp)
       (define ae1-set ((atom-eval benv store)  ae1))
       (define ae2-set ((atom-eval benv store) ae2))
       
   
       
       
       (cond
         [(container-biops? biop) 
          (DEBUG " in container biop " biop)
          (define items-locs (obj-lookup* (set "__containerdict__") ae1-set ostore))
          (define d-containerdict-vals (obj-lookup* ae2-set items-locs ostore)) (DEBUG " the val set from __items__ " d-containerdict-vals)
          (define d-elem-vals (obj-lookup* ae2-set ae1-set ostore)) (DEBUG " the val set from what referring " d-elem-vals)
          (define res-set (set-union d-containerdict-vals d-elem-vals))
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;; added to support theorem provers
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (cond 
            [(equal? biop 'py-list-ref)
             (displayln "py-list-ref bound checking for expression..." )
             (displayln exp)
             (deal-with-list-ops-reference-and-verify ae1 ae2 benv exp govenor-base)
             (apply kont-addr res-set store ostore measure time govenor-base  #f)]
            
            [(equal? biop 'dict-ref)
             (define singleton-oloc-val (singleton-type-set?  items-locs 'oloc))
             (define oloc-sym-info (addr-in-smt-sym-table? singleton-oloc-val))
             (cond
               [(not (equal? oloc-sym-info #f))
                (apply kont-addr res-set store ostore measure time govenor-base oloc-sym-info)
                ]
               [else (apply kont-addr res-set store ostore measure time govenor-base #f)])
             ]
            [else (apply kont-addr res-set store ostore measure time govenor-base #f)])
          
          ;(apply kont-addr res-set store ostore measure time #f)
          ]
         
         ;
         [(or (arith-ops? biop) (comp-biops? biop))
          ; no such computation
          ;(define res (map-set2 best-effort-arith-value ae1-set ae2-set biop))
          ; ;; we don't care what's the exact value anyway
          (define res 'num)
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;; added to support theorem provers
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          
          (define oloc1 (singleton-type-set? ae1-set 'oloc))
          (define int-val1 (singleton-type-set? ae1-set 'int))
          (define addr-val1 (singleton-type-set? ae1-set 'addr))
          (define oloc2 (singleton-type-set? ae2-set 'oloc))
          (define int-val2 (singleton-type-set? ae2-set 'int))
          (define addr-val2 (singleton-type-set? ae2-set 'addr))
             
          ; biop arithmetics
          ; it will have new binding for the operations!
          (cond
            [(set-member?  (set '+ '- '* '/) biop)
             (set! res (set 'num))
             (displayln (format "biop operation ~a" exp))
             
              
             (define new-smt-sym-to-bind (gen-smt-var))
             (define new-smt-meta-info (sym-meta new-smt-sym-to-bind 'int))
             (add-formula! `(smt:declare-fun ,new-smt-sym-to-bind () Int))
             
           ;  (cond
               ;; two integers only integers
               ;[(and (not (equal? int-val1 #f)) 
                ;     (not (equal? int-val2 #f) ))
                (cond 
                   [(and (not (symbol? ae1))
                        (not (symbol? ae2)))
                    (add-formula! `(smt:assert (=/s ,new-smt-meta-info (,(smtop biop) ,ae1 ,ae2))))]
                   ;; either one is not symbol, probably integer, will not cause rebidning (no new assertions added
                  [(and (symbol? ae1)
                        (not (symbol? ae2)))
                   (define ae1-meta (addr-in-smt-sym-table? (benv-lookup benv ae1)))
                   (set! new-smt-meta-info ae1-meta)]
                  [(and (symbol? ae2)
                        (not (symbol? ae1)))
                  
                   (define ae2-meta (addr-in-smt-sym-table? (benv-lookup benv ae2)))
                    (displayln (format "ae1 is not symbol, the meta returns is ~a" ae2-meta))
                   (set! new-smt-meta-info ae2-meta)]
                  
                  [(and (symbol? ae2)
                         (symbol? ae1))
                   (define sym-info1 (addr-in-smt-sym-table? (benv-lookup benv ae1)))
                   ;(define smt-name1 (sym-meta-sym-name sym-info1))
                   (define sym-info2 (addr-in-smt-sym-table? (benv-lookup benv ae2)))
                   ;(define smt-name2 (sym-meta-sym-name sym-info2))
                   (cond 
                     [(and (normal-meta? sym-info1)
                           (normal-meta? sym-info2))
                      (define smt-name1 (sym-meta-sym-name sym-info1))
                      (define smt-name2 (sym-meta-sym-name sym-info2))
                      (add-formula! `(smt:assert (=/s ,new-smt-meta-info (,(smtop biop) ,smt-name1 ,smt-name2))))]
                     
                     [(and (not (normal-meta? sym-info1)
                           (normal-meta? sym-info2)))
                       (define smt-name2 (sym-meta-sym-name sym-info2))
                       (add-formula! `(smt:assert (=/s ,new-smt-meta-info (,(smtop biop) ,sym-info1 ,smt-name2))))]
                     
                      [(and  (normal-meta? sym-info1)
                           (not (normal-meta? sym-info2)))
                       (define smt-name1 (sym-meta-sym-name sym-info1))
                       (add-formula! `(smt:assert (=/s ,new-smt-meta-info (,(smtop biop) ,smt-name1 ,sym-info2))))]
                      
                      [(and (not (normal-meta? sym-info1))
                           (not (normal-meta? sym-info2)))
                        (add-formula! `(smt:assert (=/s ,new-smt-meta-info (,(smtop biop) ,sym-info1 ,sym-info2))))])])
               
                 (apply kont-addr res store ostore  measure time govenor-base new-smt-meta-info)]
               ;[else
                
               ; (apply kont-addr res store ostore  measure time  new-smt-meta-info)])]
                
;               ;; if addr updated with another integer, we will not rebinding! just use the original one.
;               ;; frst int and second addr
;               [(and (not (equal? int-val1 #f))
;                    (not (equal? addr-val2 #f)))
;                (define sym-info (addr-in-smt-sym-table? addr-val2))
;                (define smt-name (sym-meta-sym-name sym-info)) ;no rebinding
;                (set! new-smt-meta-info sym-info)]
;               ;; (add-formula! `(smt:assert (=/s ,new-smt-sym-to-bind (,(smtop biop) ,int-val1 ,smt-name))))]
;               ;; first addr and second int
;               [(and (not (equal? int-val2 #f))
;                     (not (equal? addr-val1 #f)))
;                (define sym-info (addr-in-smt-sym-table? addr-val1))
;                (define smt-name (sym-meta-sym-name sym-info))
;                 (set! new-smt-meta-info sym-info)]
;                 ;(add-formula! `(smt:assert (=/s ,new-smt-sym-to-bind (,(smtop biop) ,int-val2 ,smt-name))))]
;               ;; both addr
;               [(and (not (equal? addr-val1 #f))
;                     (not (equal? addr-val2 #f)))
;                (define sym-info1 (addr-in-smt-sym-table? addr-val1))
;                (define smt-name1 (sym-meta-sym-name sym-info1))
;                (define sym-info2 (addr-in-smt-sym-table? addr-val2))
;                (define smt-name2 (sym-meta-sym-name sym-info2))
;                (add-formula! `(smt:assert (=/s ,new-smt-sym-to-bind (,(smtop biop) ,smt-name1 ,smt-name2))))]
;               [else (void)])
;              (apply kont-addr res store ostore  measure time  new-smt-meta-info)
;             ]
            
            
            ;; there is no newe binding meta varible for the, just the formua
             [(set-member? (set '> '< '>= '<= 'equal? 'not-equal?) biop)
            
              (displayln (format "biop compare ~a" exp))
     
              (set! res (set 'predicate))
              (define smt-res-to-bind #t)
             ; (cond
               ;; two integers, in this case, maybe not necessary
             ;  [(and (not (equal? int-val1 #f)) 
              ;       (not (equal? int-val2 #f) ))
                 
                ;;(define meta1 (get-meta #t ae1 #f benv store))
                ;(define meta2 (get-meta #t ae2 #f  benv store))
                ;(set! smt-res-to-bind (smtop2 biop meta1 meta2))
                
                ; if not symbol, then we suppose it is iteger
                (cond 
                   [(and (not (symbol? ae1))
                        (not (symbol? ae2)))
                     (displayln (format "both are not symbol ~a ~a" ae1 ae2))
                     (set! smt-res-to-bind (smtop2 biop ae1 ae2))]
                   
                  [(and (symbol? ae1)
                        (not (symbol? ae2)))
                     (displayln (format  "symbol and not ~a ~a" ae1 ae2))
                   (define ae1-meta (addr-in-smt-sym-table? (benv-lookup benv ae1)))
                   (cond
                     [(normal-meta? ae1-meta) 
                       (set!  smt-res-to-bind (smtop2 biop (sym-meta-sym-name ae1-meta) ae2))]
                      
                     [else
                      (set!  smt-res-to-bind (smtop2 biop ae1-meta ae2))
                      ])]
                  [(and (symbol? ae2)
                        (not (symbol? ae1)))
                    (displayln (format  "symbol and not ~a ~a" ae2 ae1))
                   (define ae2-meta (addr-in-smt-sym-table? (benv-lookup benv ae2)))
                   (cond
                     [(normal-meta? ae2-meta) 
                       (set!  smt-res-to-bind (smtop2 biop  ae1 (sym-meta-sym-name ae2-meta)))]
                     [else
                      (set!  smt-res-to-bind (smtop2 biop ae1 ae2-meta))
                      ])]
                  
                  [(and (symbol? ae2)
                         (symbol? ae1))
                    
                   (define sym-info1 (addr-in-smt-sym-table? (benv-lookup benv ae1)))
                   ;(define smt-name1 (sym-meta-sym-name sym-info1))
                   (define sym-info2 (addr-in-smt-sym-table? (benv-lookup benv ae2)))
                   ;(define smt-name2 (sym-meta-sym-name sym-info2))
                   ;(pretty-write smt-symbol-table)
                  
                   (cond 
                     [(and (normal-meta? sym-info1)
                           (normal-meta? sym-info2))
                      (define smt-name1 (sym-meta-sym-name sym-info1))
                      (define smt-name2 (sym-meta-sym-name sym-info2))
                      (set!  smt-res-to-bind (smtop2 biop smt-name1 smt-name2))]
                     [(and (not (normal-meta? sym-info1))
                           (normal-meta? sym-info2))
                       (define smt-name2 (sym-meta-sym-name sym-info2))
                        (set!  smt-res-to-bind (smtop2 biop  sym-info1 smt-name2))]
                      [(and  (normal-meta? sym-info1)
                           (not (normal-meta? sym-info2)))
                       (define smt-name1 (sym-meta-sym-name sym-info1))
                       (set!  smt-res-to-bind (smtop2 biop  smt-name1 sym-info2))]
                      [(and (not (normal-meta? sym-info1))
                           (not (normal-meta? sym-info2)))
                        (set!  smt-res-to-bind (smtop2 biop  sym-info1 sym-info2))]
                  [else (void)]
                  )
                ])
                (apply kont-addr res store ostore measure time govenor-base smt-res-to-bind)]
             )]
               
           
         ;; for container
         [(equal? biop 'for-container)
           
          (DEBUG "biop for-container " exp)
          ;(cond 
            ;; todo: for container will iterate dictionary, but now it is pylist pyset tuple
            ;[(or (py-list? base store ostore) (pyset? base store ostore) (tuple? base store ostore))
             ;; it is the proc to be applied to every elements in the containers.
             (define proc (get-elem-from-singleton-set ae2-set))
           
             (define flattened-container-vals (flatten-container-vals ae1-set ostore))
             (DEBUG " flatten-container-vals is " flattened-container-vals)
             (match proc
               [(struct closure (lamb be))
                (match lamb
                    [`(lambda (,v) ,e) 
                     (define new-fork-addr ((alloc-kont time) exp)) ;this exp as part of the forK address)
                    ; (define new-kont-addr ((alloc-kont time) exp))
                    ; (define new-kont (make-letK v e benv kont-addr))
                    ; (define new-store (store-update store new-kont-addr (set new-kont)))
                    ; (list (state ce benv new-store ostore new-kont-addr time*)
                            ;(define-struct forK (vk vv rest-seq e benv addr) #:transparent)
                    (cond
                       [(empty? flattened-container-vals) (flatten (apply kont-addr (set) store ostore measure time govenor-base   #f))]
                       [else
                        (define new-kont-fork (forK v (car flattened-container-vals) (rest flattened-container-vals) e be kont-addr))
                        (define store* (store-update store new-fork-addr (set new-kont-fork)))
                        ;(define measure* (measure-update  new-fork-addr))
                        (flatten (apply new-fork-addr (set) store* ostore measure time govenor-base  #f))])])])])
                ]

      ;; in dict
      [`(dict (,v-k ,ae-v) ...)
       (DEBUG " in dict " exp)
       ; (define time* (tick (get-current-time exp) time))
       ;; guess this is the program point where we tick time to construct a new address!?
        (define time* (tick (get-current-time time) time))
       (define oloc (OLoc exp time*))
       (define d-ks  (map (atom-eval benv store) v-k))
      ; (define d-vs  (map (atom-eval benv store) ae-v))
       (define d-vs (new-vals v-k ae-v benv store))
        
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;; added to support theorem provers
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (define symbol-meta-to-bind 
         (cond
           [(number-keys? v-k)
            (define z3-sym (gen-smt-var))
            ;; suppose this first we start list
            (add-formula! `(smt:declare-fun ,z3-sym () (Array Int Int)))
            (cond
              ;; we only deal with the values in the list is integers!
              [(number-keys? ae-v)
               (define rktLst (symIntLst->rktIntList ae-v))
               ;(add-formula! `(smt:assert (=/s ,z3-sym (list->z3-list ,rktLst))))
               (sym-meta z3-sym 'array)
               ]
              [else #f])]
           ;; empty dict initialiazation
           [(empty? v-k)
            (define z3-sym (gen-smt-var))
            ;; suppose this first we start list
            (add-formula! `(smt:declare-fun ,z3-sym () (Array Int Int)))
            (sym-meta z3-sym 'array)
            ]
            
           [else #f] ))
       
       ;; the oloc will be in the sym table too, since it is the value passed around
       (update-smt-symbol-table! oloc symbol-meta-to-bind)
       
       (define obj-map (ostore-lookup ostore oloc))
       ;; the following is for testing purpose
;       (when (empty? (hash->list (first obj-map))) 
;         (DEBUG " the bottom value for obj-map, probably the first time allocation? " obj-map))
       (define obj-map* (obj-update* d-ks d-vs obj-map))
       (define ostore* (ostore-update ostore oloc obj-map*))
       (apply kont-addr (set oloc) store ostore* measure time* govenor-base symbol-meta-to-bind)]
      
      ;; get-field
      [`(get-field ,ae-b ,ae-f)
       (DEBUG " in get-field " exp)
     
       (define d-f ((atom-eval benv store) ae-f))
       (define d-b ((atom-eval benv store) ae-b))
       (DEBUG "d-f: " d-f)
      
       
       (flatten
        (for/list ([val (in-set d-b)])
          ;; yeah, this is ugly, but for the __class__ case, whose evaluation is just a  class symbol
          (match val
            ;; class get-field
            [(struct Cls (cl oloc)) 
             (DEBUG "get-field's base is a class " val)
             ;(DEBUG "ostore" ostore)
             (local ([define mro-lst (get-elem-from-singleton-set (obj-lookup (set "__mro__") (ostore-lookup ostore oloc)))]
                     [define res (travel-mro mro-lst ae-f "class" #f #f benv store ostore)])
          
               (apply kont-addr res store ostore  measure time  govenor-base #f))]
            ;; instance get-field
            [(struct OLoc (e t))
             (DEBUG "get-field's based is an normal location, which is an instance location " (OLoc-exp val))
             ;(DEBUG "the ostore is: " ostore)
             (local ([define inst-om (ostore-lookup ostore val)]
                     [define tmp-res (obj-lookup d-f inst-om)])
             
               (cond
                 [(set-empty? tmp-res)
                  (DEBUG " instance field result not found for " ae-b)
                  (DEBUG "will search up the instance's class mro-lst....")
                  ;(DEBUG "the inst-om is: " inst-om)
                  ;(DEBUG "obj-lookup: "(obj-lookup (set "__class__") inst-om))
                  ; go through the __class__
                  ; note: the __class__ as well the __mro__ is singleton set
                  (define cls (get-elem-from-singleton-set (obj-lookup (set "__class__") inst-om)))
                  ;(DEBUG "cls" cls)
                  (define cls-cls (get-elem-from-singleton-set ((atom-eval benv store) cls)))
                  ;(DEBUG "cls-cls" cls-cls)
                  (match cls-cls
                    [(struct Cls (cl oloc)) 
                     (define mro-lst (get-elem-from-singleton-set (obj-lookup (set "__mro__") (ostore-lookup ostore oloc))))
                     (DEBUG "mro-lst get is: " mro-lst)
                     (DEBUG "ae-f passed is: " ae-f)
                     (define res  (travel-mro mro-lst ae-f "inst" val #f benv store ostore))
                     
                     (define states (apply kont-addr res store ostore measure time govenor-base #f))
                     ;(DEBUG "list of states?" (list? states)) ;(pretty-write states) 
                     states]
                    ;(apply kont-addr res store ostore time)]
                    [else  (DEBUG "instance get-field not found instance and neither reference to the instance's class field succeeds " cls)
                            (apply kont-addr (set) store ostore measure time govenor-base #f)])]
                 [else
                  
                   (define res (transform-methods-in-inst-fld-reference tmp-res val) )
                    
                  (apply kont-addr res store ostore measure time govenor-base #f)]))])))]
      
      ;; unop
      [`((anf ,unop) ,ae)
       (DEBUG " in unary operation: " exp)
     
       (define base-set ((atom-eval benv store) ae))
       (apply kont-addr (map-set-uniop compute-each base-set unop store ostore) store ostore measure  time govenor-base #f)]
      
      ;; triop
      [`((anf ,triop) ,ae-b ,ae-f ,ae-v) 
       (DEBUG " in triop " exp)
       
       (define field-set ((atom-eval benv store) ae-f))
       (define val-set ((atom-eval benv store) ae-v))
       (define locs ((atom-eval benv store) ae-b))
       (DEBUG " the triop base set: " locs)
                           
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;;; added to support theorem provers
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       (cond
         [(equal? triop 'py-list-set!)
          
          (deal-with-list-ops-reference-and-verify ae-b ae-f benv exp govenor-base)
          ]
         
         ;; list append, or some other mutation is done by dict-set! see implementation of append in rakcet
         ;; we are going to turn it into append in Z3
         ;; also, we only add intger type element if the base it is the list location
         
         ;; but for unintepreted array, we don;t need to rebind!!!!
         ;[(equal? triop 'dict-set!)
       ;   (deal-with-dictset ae-b ae-f ae-v benv store exp)
        ;  ]
         
         ; not dealing with temparaily.
         [else (void)])
       
       (cond
         [(or (equal? triop 'dict-set!) (equal? triop 'py-list-set!))
          (define ostore* 
            (foldr (λ (loc os)  ;; for every locations that ae-b points to
                     (local ([define om (ostore-lookup ostore loc)]
                             [define item-locs (obj-lookup (set "__containerdict__") om)])
                       (cond ;; to find the nested dict location
                         [(set-empty? item-locs) (DEBUG " dict-set! no containerdict field found for " ae-b) os]
                         [else
                          (local ([define locs-lst (pull-locs item-locs)]) ;; yeah, we find (possibly) nested locations
                            (cond 
                              [(empty? locs-lst) (DEBUG " dict-set! no containerdict locations for " ae-b) os] ;we don't update values that's not objs
                              [else ;; here we found
                               (foldr (λ (nest-loc oss) ;; we fold the current os 
                                        (ostore-update oss nest-loc (obj-update field-set val-set (ostore-lookup ostore nest-loc))))
                                      os
                                      locs-lst)]))]) 
                       ))
                   ostore
                   (pull-locs locs)))
          (define ostore** (foldr (λ (loc os) (ostore-update os loc (obj-update field-set val-set (ostore-lookup ostore loc))))
                                  ostore*
                                  (pull-locs locs)))
          (apply kont-addr (set) store ostore** measure time govenor-base #f)])
       ]
      
      ;; the let binding lambda
      [(or `(lambda (,formals ...) ,body) `(lambda ,formals ,body))
        
       (DEBUG " in let binding lambda " exp)
       (apply kont-addr ((atom-eval benv store) exp) store ostore measure time govenor-base #f)]
      
      ;; special case void
      [(or `(void) 'void)
       (DEBUG " in void" exp)
      
       (apply kont-addr ((atom-eval benv store) exp) store ostore measure  time govenor-base #f)]
      
      ;; special case: (list of class-precedences in the __mro__ field
      [`(list ,e ...)
       (DEBUG " in list of class precedences " exp)
       ; the atom-eval is extended to pack the exp as a singletone set of as it is
       ; for the ease of travel-mro
       (define val-set ((atom-eval benv store) exp)) 
      
       (apply kont-addr val-set store ostore measure time govenor-base #f)]
      
      ;; application
      [`(,f ,args ...)
       (displayln (format " in function app ~a" exp))
       (define procs ((atom-eval benv store) f))
      
       (define time* (tick (get-current-time exp) time))
       (cond
         ;; probably external function definitions.
         [;(and (set-empty? procs)
          (stubbed-external-func? f)
          (cond
            [(special-function? f 'read-int)
             ;; whatever the value, just stuff 3.
             ;; the analysis is not using 3 anyway
             (apply kont-addr (set 3) store ostore measure time* govenor-base #f)
             ]
            [(special-function? f 'len)
              ;(define z3-sym (gen-smt-var))
             ;; then the argument will be one
             
           
              (define lenarg-meta (get-len-sym (first args) benv))
             
               (define formula-to-bind `(LEN ,lenarg-meta))
              (apply kont-addr (set  'symbol-len) store ostore measure time* govenor-base  formula-to-bind)
              ]
            
;            [else
;             ; other funcs, we may just return empty set
;              (apply kont-addr (set) store ostore measure time  #f)
;             ]
            )
          
          ]
        [ else
          
       (flatten 
        (for/list ([proc (in-set procs)])
          (match proc
            ;; closures with various lambda form
            [(struct closure (lambda-exp benv*))
             (match lambda-exp
               ;; normal lambda
               [`(lambda (,formals ...) ,exp) 
                (displayln (format "in normal lambda: ~a" lambda-exp))
                (ordinary-lambda-bind proc args benv store ostore measure kont-addr time*  govenor-base)]
               ;; curried lambda
               [`((lambda (,formals ...) ,exp) ,obj-loc)
                (DEBUG " app matched curried lambda " proc)
             
                (define params (append (list (set obj-loc)) (map (atom-eval benv store) args)))
                ;(DEBUG "params: " params)
                (define bindings (map (alloc-bind time*) formals))
                ;(DEBUG "bindings" bindings)
                (define benv** (benv-extend* benv* formals bindings))
                (define store* (store-update* store bindings params))
                ;(define measure* (measure-update* measure bindings))
                
                ;; added for theorem prover
                (deal-with-func-call-binding bindings params benv store (append (list obj-loc) args))
                
                ;(DEBUG "ere")
                (state exp benv** store* ostore measure kont-addr time* govenor-base)]
               ;; object lambda
               [`(lambda () void)
                (DEBUG " app matched in object lambda ")
                (apply kont-addr (set proc) store ostore measure time* govenor-base #f)])]
            
            ;; the process to be applied from call/ec
            [(struct kontP (letk-addr))
             (DEBUG " app matched in continuation " proc)
             (cond
               [(empty? args) (apply letk-addr (set) store ostore measure time* govenor-base #f)]
               [else  
                (define val ((atom-eval benv store) (car args)))
                
                (displayln "call/ec part")
              ;;  (displayln val)
                
                (apply letk-addr val store ostore measure time govenor-base  #f)])]
            
            ;; the Cls structure that enclosed a closure
            [(struct Cls (clo loc))
             (DEBUG "in app: f is evaluted to be cls closure")
             (ordinary-lambda-bind clo  args benv store ostore measure  kont-addr time*  govenor-base)]
            ;; falling through all the cases
            [else
            
             (error (format "no matching in app: ~a" exp))])))])]
      
      [e (cond
            [(atom? e) (apply kont-addr ((atom-eval benv store) e) store ostore measure  time govenor-base  #f)]
            [else empty])]))
  
  ;; compute all the reachable states
  ;; reachale :: Set State -> [State] -> Set State
  (define (reachable seen todo)
    (match todo
      [(list) seen]
      [(list-rest (? (curry set-member? seen)) todo) ;(displayln "middle case") 
                                                     (reachable seen todo)]
      [(list-rest st0 todo) 
       (cond 
         [pw? 
          (cond 
            [(point-wise-widen? st0)
             (define new-st0 (point-wise-widen st0))
             (reachable (set-add seen new-st0) (append (next new-st0) todo))]
            [else  (reachable (set-add seen st0) (append (next st0) todo))])]
         [else
           (reachable (set-add seen st0) (append (next st0) todo))])]
          ))
  
  ;; gives out initial state
  (define (inject-cesk exp benv0 store0 ostore0 measure0)
    (state exp benv0 store0 ostore0 measure0 ((alloc-kont time-zero) `$halt) time-zero (set)
                ))
 
  ;; explore all reachable states
  ;; explore :: initial-state -> Set state
  (define (explore injected-state)
    (define all-reachables-states (reachable empty-set (list injected-state)))
    (display "Number of States = ") (displayln (length (set->list all-reachables-states)))
    (newline)
    (set)
    ;all-reachables-states
    )
                      
 
  

  )



