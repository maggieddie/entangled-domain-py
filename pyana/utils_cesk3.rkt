(module utils_cesk3
  racket/base
  (provide (all-defined-out))
  (require racket/match)
  (require racket/promise)
  (require racket/list)
  (require racket/set)
  (require racket/pretty)
  (require racket/local)
  
  (define debug-flag #f)
  
  ;; State Space (comments are in Haskell, but not purely)
  
  ;; type State = (Exp, BEnv, Store, OStore, Measure, KAddr, Time)
  (define-struct state (context benv store ostore measure 
                                kont-addr time) #:transparent)
  ;; type BEnv = Var -> Addr
  (define empty-benv (make-immutable-hasheq empty))
  
  ;; type Store = Addr -> D
  ;; type OStore = OLoc -> Obj
  (define empty-store (make-immutable-hasheq empty))
  (define empty-ostore (make-immutable-hasheq empty))
  
  ;; type measure = Addr -> Abstract Num, which is either {0, 1, >=1}
  (define empty-measure (make-immutable-hasheq empty))
  
  ;; type Obj = (ObjMap, ElemSetMap) 
  
  ;; type ObjMap = Map D -> D
  
  ;; type ElemSetMap = Map Val P(D)
  
  ;; type D = Set Val
  ;; few cosmetics
  (define empty-set (set))
  (define d-bot empty-set)
  
  ;; type Val = Clo + Kont + OLoc + kontP + String + Z + Bool + Cls(Clo, OLoc)
  (define-struct closure (lambda benv) #:transparent)  
  (define-struct kontP (addr))
  
  ;; Data Kont = letk (var exp benv addr) | halt
  (define-struct letK (var exp benv addr)); #:transparent)
  (define-struct forK (vk vv rest-seq e benv addr) #:transparent)
  
  (define-struct Cls (clo oloc)); #:transparent)
  
  ;; Data Addr = KAddr (Exp, Time) | BAddr (Var, Time)
  (define-struct KAddr (exp time)); #:transparent)
  (define-struct BAddr (var time) #:transparent)
  
  ; alloc : time -> var -> addr
  ; can alloc cont address, where the var will be the a list of kont!
  (define (alloc-bind time)
    (lambda (var)
      (make-BAddr var time)))
  
  (define (alloc-kont time)
    (lambda (exp)
      (make-KAddr exp time)))
  
  ;; type OLoc = OLoc ( Exp, Time)
  (define-struct OLoc (exp time)); #:transparent)
  
  ;; operations  
  ; d-join : d d -> d
  (define d-join set-union)  
  
  ; benv-lookup : benv var -> addr
  (define benv-lookup hash-ref)
  
  ; benv-extend : benv var addr -> benv
  (define benv-extend hash-set)
  
  ; benv-extend* : benv list[var] list[addr] -> benv
  (define (benv-extend* benv vars addrs)
    (for/fold ([benv benv])
      ([v (in-list vars)]
       [a (in-list addrs)])
      (benv-extend benv v a)))
  
  ; store-lookup : store addr -> d
  (define (store-lookup s a)
    (hash-ref s a d-bot))
  
  ; store-update : store addr d -> store
  (define (store-update store addr value)
    (hash-update store addr ;(hash-update store addr 
                 (lambda (d) (d-join d value))
                 d-bot))
  
  ; store-update* : store list[addr] list[d] -> store
  (define (store-update* store addrs values)
    (for/fold ([store store])
      ([a (in-list addrs)]
       [v (in-list values)])
      (store-update store a v)))   
  
  ; store-join : store store -> store
  (define (store-join store1 store2)
    (for/fold ([new-store store1])
      ([(k v) (in-hash store2)])
      (store-update new-store k v)))
  
  (define (store-strong-update store addr value)
    (hash-update store addr
                 (lambda (d) value)
                 d-bot))
  (define (store-strong-update* store addrs values)
    (for/fold ([store store])
      ([a (in-list addrs)]
       [v (in-list values)])
      (store-strong-update store a v)))
  
  ;; measure update helpers
  (define (measure-update measure addr)
    (hash-update measure addr
                  (λ (c) (add1 c))
                  0))
  
  (define (measure-update* measure addrs)
    (foldr (λ (a nm)
             (measure-update nm a))
           measure
           addrs))
  
  ; the store update for the class-closure * closure tuple for any give class variable
  ; The way how class is translated determins that for any time, there will be
  ; only one closure value in the power set returned from the store! So here the purpose of this 
  ; function is to extend the class closure with its according class dict location
  ;; store-update-cls :: store addr value -> store
  (define (store-update-cls store addr oloc)
    (local ([define cls-clo-cnt 0]
            [define clses (store-lookup store addr)]
            [define (safe-substitutions store clo res)
              (hash-set store addr res)]
            [define res-set 
              (foldr 
               (λ (v res-set) 
                 (cond
                   [(closure? v)
                    ; for testing purpose, sorry for it is a little imperative style
                    (set! cls-clo-cnt (add1 cls-clo-cnt))
                    (DEBUG " cls closure found in the addr times: " cls-clo-cnt)
                    (set-add res-set (make-Cls v oloc))]
                   [(Cls? v)
                    (DEBUG " Cls structure found in store-upate-cls" v)
                    (set-add res-set v)]
                   [else
                    (DEBUG "in store-upate-cls: problem occurs! the addr is not mapping to a Cls nor closure!")
                    (error "store-upate-cls: redo your work!")]))
               (set)
               (set->list clses))])
      (safe-substitutions store addr res-set)))


(define obj-map-bot (list (make-immutable-hash empty) (make-immutable-hash empty)))

; ostore-lookup :: ostore -> OLoc -> Obj
(define (ostore-lookup ostore oloc)
  (hash-ref ostore oloc obj-map-bot))
; ostore-update :: working on the ostore which is 
;; mapping from location to a objmap, rather than a set!
;;; the obj-map is extended one.
(define (ostore-update ostore loc obj-map)
  (hash-set ostore loc obj-map))

;;;;;;;;;; Objmap insertions and update:

;; obj-update :: D -> D -> Obj -> Obj
(define (obj-update set-of-keys set-of-vals obj-to-update)
  (local ([define obj-map (first obj-to-update)]
          [define elem-set-map (second obj-to-update)])
    (list (hash-set obj-map set-of-keys set-of-vals)
          (foldr (λ (k m) 
                   (local ([define ks (set set-of-keys)])
                     (hash-update m k
                                  (λ (val) (set-union val ks))
                                  ks)))
                 elem-set-map
                 (set->list set-of-keys)))))

;; flat-lookup :: (Val -> D) -> D -> D
(define (flat-lookup omap set-of-keys)
  (foldr (λ (elem flat-res-set) (set-union flat-res-set (hash-ref omap elem (set))))
         (set)
         (set->list set-of-keys)))

;; obj-lookup :: D -> Obj -> D
(define (obj-lookup set-of-keys obj)
  (local ([define obj-map (first obj)]
          [define elem-set-map (second obj)])
    (flat-lookup obj-map (flat-lookup elem-set-map set-of-keys))))

;; obj-lookup* :: D -> [OLoc] -> D  
(define (obj-lookup* set-of-keys locs ostore)
  (foldr (λ (val res-set) 
           (match val
             [(struct OLoc (e t)) 
              (set-union res-set (obj-lookup set-of-keys (ostore-lookup ostore val)))]
             [else (set-union res-set (set))]))
         (set)
         (set->list locs)))

;; obj-update* :: [D] -> [D] -> obj -> obj
(define (obj-update* key-set-lst val-set-lst obj)
  (local ([define key-val-list (zip key-set-lst val-set-lst (list))])
    (foldr (λ (key-val-pair res-obj)
             (obj-update (car key-val-pair) (cdr key-val-pair) res-obj))
           obj
           key-val-list)))


;; time = (listof label)
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.
(define time-zero empty)

;; k-CFA parameters
; k : natural
(define k (make-parameter 1))

; take* is like take but allows n to be larger than (length l)
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-naturals)]
             #:when (i . < . n))
    e))

; tick : call cur-time -> time 
; the time will be last k call site!
(define (tick current-time time)   (take* (list* current-time time) (k)))


;****************************** helpers in atom-eval **********************************
  
  ; atom-evaluator
  ;; atom-eval :: Benv -> Store -> OStore -> D
  (define (atom-eval benv store)
    (match-lambda  
      [(? lam? lam)
       (set (make-closure lam benv))]
      [(? ae-list? l) (get-list-set l)]
      [(? void? v)     (set)]
      [(? symbol? sym)
       (cond 
         [(equal? sym '$halt)  (set '$halt)] ;;;;;;;;;;;;;;;;;;;;
         [else (store-lookup store (benv-lookup benv sym))])]
       [(? KAddr? ka)
        (store-lookup store ka)]
       [(? Cls? cls) (set cls)]
       [(? number? num) (set num)]  
       [(? string? str) (set str)]
       [(? boolean? b)  (set b)]
       [;;;;;;;;
        (? empty? mt) (DEBUG "empty list evaluated" mt)  (set '$halt)]))
       ;[(? set? s)   s]))
  
;; Various kinds of lambda form  
(define (lam? exp)
  (match exp
    [`(lambda (,formals ...) ,_ ...) #t]
    [`((lambda (,formals ..) ,_ ...) ,obj-dict) #t]
    [`(lambda ,formals ,_ ...) #t]
    [else #f])) 
;;
(define (get-list-set al)
  (match al
    [`(list ,e ...)  (set  e)]))
;;  
(define (ae-list? al)
  (match al
    [`(list ,e ...) #t]
    [else #f]))
;; currently just support symbol? to restrict the usage
(define (atom? e)
  (cond 
    [(or (symbol? e) (string? e) (number? e) (boolean? e) (void? e)) #t]
    [else #f]))
  
;; 
 (define (meta? k)
   ;; mro, base should also be meta data, but right now jsut keep class
    (equal? k "__class__") )
  
;; evalutes values according to the key
 (define (new-vals k-lst v-lst benv store)
   (define len-k (length k-lst))
   (define len-v (length k-lst))
   (cond 
     [(not (= len-k len-v)) (error "key - vals number not matched!" )]
     [else
      (for/list ([e1 k-lst]
                 [e2 v-lst])
        (cond 
          [(meta? e1) (set e2)]
          [else  ((atom-eval  benv store) e2)]))]))


;****************************** for test ***************************************** 
(define (DEBUG . args)
  
  (if debug-flag
      (if (equal? (length args) 1)
          (displayln (format "........ ~a ........" (car args)))
          (begin
            (pretty-write (format ".......... ~a: ~a" (car args) (second args)))))
      ; (pretty-write (second args))))
      (void)))


;; generate small counter as pointer
(define gensym
  (let ([counter 0])
    (lambda ([x 'c])
      (if (number? x)
          (set! counter x)
          (begin0 (string->unreadable-symbol
                   (format "~a~a" x counter))
                  (set! counter (add1 counter))))))) 

;; 
(define (get-elem-from-singleton-set  s)
  (cond 
    [(set-empty? s)  (DEBUG "Warning: Not an singleton set but an empty set")]
    [(> (set-count s) 1) (DEBUG "Warning: the set count is more than one!" s)]
    [else (car (set->list s))]))


;;; for statistc 
(define number-of-state 0)

;; label dict which attach unique label to every subterm of the program
;; use the hasheq to identify same expression structures appeared multiple times which are actually distinguished expression!
(define lb-dict (make-hasheq))

;; get-current-time :: Exp -> Label
(define (get-current-time callexp)
  (match callexp
    [`(,f ,args ...)
     (cond 
       [(hash-has-key? lb-dict callexp) (hash-ref lb-dict callexp)]
       [else 
        (local ([define cur-time (gensym)])
          (hash-set! lb-dict callexp cur-time)
          cur-time)])]
    [else (DEBUG "not a call, we don't get the time for non-call expression!") ]))


; ********************************* helpers get-field ************************************  
;; pull the location in a value that comes from set 
;; pull-loc :: D -> [OLoc]
(define (pull-locs res-set)
  (foldr (λ (res loc-lst)
           (match res
             [(struct Cls (cl lo))
              (DEBUG "in pull-loc: class" ) (append loc-lst (list lo))]
             [(struct OLoc (e t))
              (DEBUG "in pull-loc: oloc ") (append loc-lst (list res))]
             [else (DEBUG "in pull-loc: not an loc") (append empty loc-lst)]))
         (list)
         (set->list res-set)))

;; transform-methods-in-inst-fld-reference :: D -> OLoc  -> D
(define (transform-methods-in-inst-fld-reference res-set inst-loc)
  (foldr (λ (elem-val res-vals)
           (cond 
             [(closure? elem-val)
              (DEBUG " instance get methods" elem-val)
              (match (closure-lambda elem-val)
                [`(lambda v void)
                 (DEBUG "lambda v void matched in get-field ")
                 ;; sorry, side effect closure for not making a new closure struct (it is referenced by address)
                 (set-add res-vals (struct-copy closure elem-val [lambda `(lambda () void)]))]
                 ;(set-closure-lambda! elem-val 
                 ;(set-add res-vals elem-val)]
                [`(lambda (,formals ...) ,body ...)
                 (DEBUG "turn the normal lambda to partial applied lambda") 
                 (set-add res-vals (struct-copy closure elem-val [lambda `((lambda ,formals ,@body) ,inst-loc)]))]
                 ;(set-closure-lambda! elem-val `((lambda ,formals ,@body) ,inst-loc))
                 ;(set-add res-vals elem-val)]
                [else (DEBUG "can't match up with lambda form!" (closure-lambda elem-val))
                      (error " but can't match up with lambda form!")])]
             [else (DEBUG "get-field from instance: normal member reference " elem-val)
                   (set-add res-vals elem-val)]))
         (set)
         (set->list res-set)))

;; a new dict key-set list mapping to val-list
;; zip :: [Set val] -> [Set val] -> [(cons (Set val) (Set val))]
(define (zip lst1 lst2 res-lst)
  (local ([define len1 (length lst1)]
          [define len2 (length lst2)])
    (cond 
      [(not (equal? len1 len2)) (error "the length should be euqual!")]
      [(empty? lst1) res-lst]
      [else
       (zip (rest lst1) (rest lst2) (append res-lst (list (cons (car lst1) (car lst2)))))
       ])))


; ********************************* helpers in unop ************************************ 
(define test-unoperations (apply set '(integer? string? tuple? dict? py-list? set?)))
(define (test-unops? prim-op) (set-member? test-unoperations prim-op))

;; get-container-type :: Val -> store -> ostore -> string
(define (get-container-type base store ostore)
  (match base
    [(struct OLoc (e t)) 
     (DEBUG "get-container-type gets the base the OLOC struct!")
     (DEBUG "base" base)
     (DEBUG (ostore-lookup ostore base))
     (define type-str (get-elem-from-singleton-set 
                       (obj-lookup (set "__type__") (ostore-lookup ostore base))))
     (DEBUG "the type str is " type-str) ;for test purpose
     (cond
       [(string? type-str) type-str]
       [else "not a built-in class inst"])]
    [else (DEBUG "the conatiner base should be OLoc struct, but it is " base) #f]))

;; py-list?
(define (py-list? base store ostore)
  (string=? (get-container-type base store ostore) "instPyList"))
;; pyset?
(define (pyset? base store ostore)
  (string=? (get-container-type base store ostore) "instSet"))
;; tuple?
(define (tuple? base store ostore)
  (string=? (get-container-type base store ostore) "instTuple"))
;; dict?
;  the following version is for supporting the python dict compiles to dict  
;  (define (dict? base store ostore)
;    (string=? (get-container-type base store ostore) "instDict"))
; this version is just for temp use :)
(define (dict? base)
  (match base
    [(struct OLoc (e t)) #t]
    [else #f]))


; ***************** helpers in biop arithmetic operations and compare operatoins ******************************  
;; the container biops essentially dict-ref, but we use specific ref mark to gain a little more performance
(define container-bioperations (apply set '(py-list-ref 
                                            tuple-ref
                                            dict-ref)))
;py-list-remove! 
;dict-remove!)))
(define (container-biops? prim-op) (set-member? container-bioperations prim-op))
(define (comp-biops? prim-op) (set-member? comp-bioperations prim-op))
;;  quotient is tested seperately to give out type of float
(define arith-operations
  (apply set '(+ - * / >> << quotient modulo expt bitwise-and bitwise-or bitwise-xor bitwise-not)))
(define (arith-ops? prim-op) (set-member? arith-operations prim-op))
(define comp-bioperations (apply set '(> < >= <= equal? not-equal? in? not-in? eq? not-eq?))) 

;; helpers in unop, the function is applied to every member member in map-set-unop 
(define (compute-each ae-val unop store ostore)
  (match unop
    [`+               (+ ae-val)]
    [`-               (- ae-val)]
    [`string?         (string? ae-val)]
    [`integer?        (integer? ae-val)]
    [`not             (not ae-val)]
    [`py-list?         (py-list? ae-val store ostore)]
    [`tuple?           (tuple? ae-val store ostore)]
    [`dict?            (dict? ae-val)]
    [`set?             (pyset? ae-val store ostore)]
    [(or `py-print  `assert1)
     '(void)]
    [else  (error (format "~a not in unop" unop))]))

;; compute-each passed as f 
(define (map-set-uniop f s uniop store ostore)
  (for/fold ([ns empty-set])
    ([e (in-set s)])
    (set-add ns (f e uniop store ostore))))

;; copy-str-n-times :: str -> num -> [Char] -> string
(define (copy-str-n-time str n res)
  (cond
    [(> n 10) (DEBUG "warning :only allow maximum 10 times") str]
    [(negative? n) (DEBUG "not support yet") str]
    [(= n 0) (list->string res)]
    [else (copy-str-n-time str (sub1 n) (append res (string->list str)))]))

;; compute python string with arith ops + *
(define (compute-py-str str num op)
  (match op
    ['+ (DEBUG "type violation!! cant add an str to a num") 'void]
    ['* (DEBUG "meant to fainfully duplicate the string, but within a threshhold value in case of attacks")
        (DEBUG "dupilicate string times: " num)
        (copy-str-n-time str num (list))]))

;; the same version as the above, except do the best effort to generate values.
(define (best-effort-arith-value  v1 v2 op )
  (cond 
    [(arith-ops? op)
     (cond 
       [(and (number? v1) (number? v2))  (match-n-compute v1 v2 op)]
       [(and (integer? v1) (string? v2))
        (compute-py-str v2 v1 op)]
       [(and (integer? v2) (string? v1))
        (compute-py-str v1 v2 op)]
       [else (DEBUG "type violation!") 'void])]
    [(comp-biops? op)
     (match-n-comp v1 v2 op)]))

(define (match-n-compute v1 v2 op)
  ; + - * / >> << quotient modulo expt bitwise-and bitwise-or bitwise-xor bitwise-not
  (match op
    [`+  (+ v1 v2)]
    ['-  (- v1 v2)]
    [`*  (* v1 v2)]
    [`/  (/ v1 v2)]
    [`>> (arithmetic-shift v1 v2)]
    [`<< (arithmetic-shift v1 (- v2))]
    [`quotient (quotient v1 v2)]
    [`expt  (expt v1 v2)]
    [`modulo (modulo v1 v2)]
    [`bitwise-and (bitwise-and v1 v2)]
    [`bitwise-xor (bitwise-xor v1 v2)]
    [`quotient 
     (cond 
       [(and (integer? v1) (integer? v2)) 
        (quotient v1 v2)] ;racket quotient only supports integer and it actually can be any number in python!] ]
       [else 0.000001])] ; this might be incorrect for fine granularity type. but the overall type is correct.
    [else (error (format "~a not supported in arith! " op))]))

(define (match-n-comp v1 v2 op)
  (match op
    ; > < >= <= equal? not-equal? in? not-in? eq? not-eq?
    [`> (> v1 v2)]
    [`< (< v1 v2)]
    [`>= ( >= v1 v2)]
    [`<= (<= v1 v2)]
    [`equal? (equal? v1 v2)]
    [`not-equal? (not (equal? v1 v2))]
    ;[`in? ] 
    ;[`not-in?]
    [`eq? (eq? v1 v2)]
    [`not-eq? (not (eq? v1 v2)) ]
    [else (error (format "~a not supported in comp op! " op))]))

; map-set : (a -> b) (set a) -> (set b)
(define (map-set f s)
  (for/fold ([ns empty-set])
    ([e (in-set s)])
    (set-add ns (f e))))

; map-set2: apply  every permutation with element from s1 and s2 to f -> (set result)
(define (map-set2 f s1 s2 op)
  (for/fold ([ns (set)])
    ([e1 (in-set s1)])
    (for/fold ([ns2 (set)])
      ([e2 (in-set s2)])
      (set-union ns (set-add ns2 (f e1 e2 op)))
      )))


; ********************************* helpers in for-container ************************************  
;; used in the for container
;; get-conatainer-elem-set :: OLoc -> Store ->OStore -> [Val]]
(define (get-container-elem-lst loc ostore)
  (define obj (ostore-lookup ostore loc))
  (DEBUG "the obj of the forloop" obj)
  (DEBUG obj)
  (define nested-elem-dict-loc 
    (get-elem-from-singleton-set (obj-lookup (set "__containerdict__") obj)))
  (match nested-elem-dict-loc
    [(struct OLoc (e t)) 
     (DEBUG "the nested-elem-dict-loc")
     (DEBUG nested-elem-dict-loc)
     (define nest-dict-obj (ostore-lookup ostore nested-elem-dict-loc))
     (define-values (objm elem-setm) (values (first nest-dict-obj) (second nest-dict-obj)))
     (define container-keys (sort (hash-keys elem-setm) <))
     (DEBUG "the container keys sorted is " container-keys)
     (map (λ (x) (obj-lookup (set x) nest-dict-obj)) container-keys)]
    [else
     (define container-keys (sort (hash-keys (second obj)) <))
     (DEBUG "the container keys sorted is " container-keys)
     (map (λ (x) (obj-lookup (set x) obj)) container-keys)]))


;; for all the conatiner locs, we flatten the values to be applied to the proc
;; mostly  it is singleton set, but we do this for soundness.
;; flatten-containers-vals ::  Set Val -> store -> ostore -> [Val]
(define (flatten-container-vals set-vals ostore)
  (foldr (λ (val lst)
           (match val
             [(struct OLoc (e t)) 
              (append lst (get-container-elem-lst val ostore))]
             [else (DEBUG "flatten-container-vals: not container locations" val) lst]))
         (list)
         (set->list set-vals)))

; ********************************* helpers for initialization ********************************
;; test a statement is define or not
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

(define (not-define? sx) (not (define? sx)))

(define (get-defs stmts) (filter define? stmts))

(define (no-defs stmts) (car (filter not-define? stmts)))

(define (init-benv defs)
  (foldr (λ (def res-benv)
           (match def
             [`(define ,var ,_) (hash-set res-benv var ((alloc-bind time-zero) var))]))
         empty-benv
         defs))

;; init-store :: benv -> store
(define (init-store benv0)
  (foldr (λ (bdaddr res-store)
           (hash-set res-store bdaddr (set)))
         empty-store  ; which is an hash compares keys with eq
         (hash-values benv0)))

;; based on the defs, no new locations allocated, so just return empty ostore
;; empty ostore is also comparing keys using eq
;; init-ostore :: ostore
(define (init-ostore benv0) empty-ostore)
  
(define (init-measure benv0)
  (foldr (λ (bdaddr res-store)
           (hash-set res-store bdaddr 0))
         empty-measure  ; which is an hash compares keys with eq
         (hash-values benv0)))
  

;************************ helpers for summary ***********************
(define (void? v) (match v [(or `(void) `void) #t] [else #f]))
(define (halt? h) (if (equal? '$halt h) #t #f))
  
 
;********************** helper for apply ****************************
  (define (new-kont-exp fork-kont-addr)
   (string->symbol
                   (symbol->string (gensym "forloop"))))
  
;********************* helpers for strong update ********************
  ;; cardinality <=1 strong update; else ,update with join
  ;; proper-update:: ae-var -> D -> benv -> store  -> store 
  (define (proper-update ae-var val-set benv store measure)
    (define addr (benv-lookup benv ae-var))
    (define cardi (store-lookup measure addr))
    (cond
      [(<= cardi 1) 
       ;; sttrong-update
       (store-strong-update store addr val-set)]
      [else (store-update store addr val-set)]))
  

)