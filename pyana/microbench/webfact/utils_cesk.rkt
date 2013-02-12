
(module utils
  racket/base
  
  (provide (all-defined-out))
  (require racket/match)
  (require racket/promise)
  (require racket/list)
  (require racket/set)
  (require racket/pretty)
  
  (define debug-flag #f)
  
  (define (DEBUG . args)
    
    (if debug-flag
        (if (equal? (length args) 1)
            (displayln (format "-------- ~a --------" (car args)))
            (begin
              (displayln (format "-------- ~a --------" (car args)))
              (pretty-write (second args))))
        (void)))
  
  
  ;; generate small counter as pointer
  (define gensym
    (let ([counter 0])
      (lambda ([x 'l])
        (if (number? x)
            (set! counter x)
            (begin0 (string->unreadable-symbol
                     (format "~a~a" x counter))
                    (set! counter (add1 counter)))))))
  
  
  
  ;; call history to deal with recursive
  (define call-history-hs (make-hash))
  
  (define global-func-hs (make-hash))
  
  ;; label dict which attach unique label to every subterm of the program
  (define lb-dict (make-hash))
  
  ;;; for log 
  (define number-of-state 0)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define empty-set (set))
  
  ; map-set : (a -> b) (set a) -> (set b)
  (define (map-set f s)
    (for/fold ([ns empty-set])
      ([e (in-set s)])
      (set-add ns (f e))))
  
  (define (map-set-uniop f s uniop store ostore)
    (for/fold ([ns empty-set])
      ([e (in-set s)])
      (set-add ns (f e uniop store ostore))))
  
  ; map-set2: apply  every permutation with element from s1 and s2 to f -> (set result)
  (define (map-set2 f s1 s2 op)
    (for/fold ([ns (set)])
      ([e1 (in-set s1)])
      (for/fold ([ns2 (set)])
        ([e2 (in-set s2)])
        (set-union ns (set-add ns2 (f e1 e2 op)))
        )))
  
  
  
  ; take* is like take but allows n to be larger than (length l)
  (define (take* l n)
    (for/list ([e (in-list l)]
               [i (in-naturals)]
               #:when (i . < . n))
      e))
  
  ;; state ::= (make-state <context> <benv> <store> <time>)
  ;; the context component can be a call, set-then! and PrimOP for now.
  ;; construct call, set-then! and Primop will be represented as a list.
  (define-struct state (context benv store ostore kont-addr measure time) #:prefab)
  
  
  ;; benv = hash[var,addr]
  ;; A binding environment maps variables to addresses.
  (define empty-benv (make-immutable-hasheq empty))
  
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
  
  ;; store = hash[addr,d]
  ;; A store (or a heap/memory) maps address to denotable values.
  ; (define empty-store (make-immutable-hasheq empty)) ;-- in the abstract counting, the store will not be join, but with update.
  
  (define empty-store (make-hash empty))
  (define empty-ostore (make-hash))
  
  (define empty-mstore (make-hash))
  
  ; store-lookup : store addr -> d
  (define (store-lookup s a)
    (hash-ref s a d-bot))
  
  ; store-update : store addr d -> store
  (define (store-update store addr value)
    (hash-update store addr ;(hash-update store addr 
                 (lambda (d) (d-join d value))
                 d-bot))
  
  ; side effect store update
  (define (store-update! store addr value)
    (hash-update! store addr ;(hash-update store addr 
                  (lambda (d) (d-join d value))
                  d-bot);)
    store)
  ; strong update the store.
  (define (store-update!! store addr value)
    (hash-update! store addr ;(hash-update store addr 
                  (lambda (d) value)
                  d-bot);)
    store)
  
  ; store-update* : store list[addr] list[d] -> store
  (define (store-update* store addrs values)
    (for/fold ([store store])
      ([a (in-list addrs)]
       [v (in-list values)])
      (store-update store a v)))
  
  
  ; side-effect----
  ; store-update!* : store list[addr] list[d] -> store
  (define (store-update!* store addrs values)
    (for/fold ([nstore store])
      ([a (in-list addrs)]
       [v (in-list values)])
      (store-update! nstore a v));)
    store)
  
  ; store-update*-for-sstore : store list[addr] list[d] -> store based on whether the element is defined var or not. the values is just a list of vars. not list of set as store-update*
  ;  (define (store-update*-for-sstore benv sstore addrs values)    
  ;    (for/fold ([store sstore])     
  ;      ([a (in-list addrs)]
  ;       [v (in-list values)])      
  ;      (if (is-defined-var? v benv)        
  ;          (store-update store a (set v))
  ;          store
  ;          )))
  
  ; store-join : store store -> store 
  ; for immutable hash
  (define (store-join-imhs store1 store2)
    (for/fold ([new-store store1])
      ([(k v) (in-hash store2)])
      (store-update new-store k v)))
  
  ; store-join : store store -> store
  (define (store-join store1 store2)
    (for/fold ([new-store store1])
      ([(k v) (in-hash store2)])
      (store-update! new-store k v)))
  
  ; measure-update: store key add -> store
  ; if the address never used, its value will be set 1; 
  ; otherwise, the addres will keep increase
  ; conceptually, values > 1 is infity
  ; this function is used when there is new address allocated.
  (define (measure-update! measure addr)
    (hash-update! measure addr
                  (lambda (cur-cnt) (add1 cur-cnt))
                  0)
    measure)
  
  (define (measure-update!* measure addrs)
    (for/fold ([ms measure])
      ([a (in-list addrs)])
      (measure-update! measure a))
    measure)
  
  
  ;; d = set[value]
  ;; An abstract denotable value is a set of possible values.
  (define d-bot empty-set)
  
  ; d-join : d d -> d
  (define d-join set-union)
  
  ;; which will push or evaluated to a list of v e s k
  (define (letK v e s k)
    (list v e s k))
  
  (define (kontP addr)
    (list addr))
  
  
  
  
  
  ;; value = clo + kont
  ;; For pure CPS, closures are the only kind of value.
  ;; For extended CPS, it is not. It can have type information (string int bool proc)
  ;; clo ::= (make-closure <lambda> <benv>)
  ;; Closures pair a lambda term with a binding environment that
  ;; determinse the value of its free variables.
  ;(define-struct closure (lam benv) #:prefab)
  
  ;; addr = bind
  ;; Addresses can point to values in the store.
  ;; In pure CPS, the only kind of addresses are bindings.
  
  ;; bind ::= (make-binding <var> <time>)
  ;; A binding is minted each time a variable gets bound to a value.
  (define-struct binding (var time) #:prefab)
  ;(define (binding var time)
   ; (list var time))
  
  ;(define-struct membP (loc var) #:prefab)
  (define (membPl loc var)
    (list loc var))
  
  ;; time = (listof label)
  ;; In k-CFA, time is a bounded memory of program history.
  ;; In particular, it is the last k call sites through which
  ;; the program has traversed.
  (define time-zero empty)
  
  ;; k-CFA parameters
  
  ;; Change these to alter the behavior of the analysis.
  
  ; k : natural
  (define k (make-parameter 1))
  
  ; tick : call cur-time -> time 
  ; the time will be last k call site!
  (define (tick current-time time)   (take* (list* current-time time) (k)))
  
  ; alloc : time -> var -> addr
  ; can alloc cont address, where the var will be the a list of kont!
  (define (alloc time)
    (lambda (var)
      (make-binding var time)))
      ;(binding var time)))
  
  (define (alloc-membP loc)
    (lambda (var)
      (membPl loc var)))
  
  (define (get-current-time context)
    (define cur-time #f) ;(gensym))
    ; oh, I think( we can make the expression as the key in the lb-dict
    (match context
      [`(,f ,args ...)
       (if (hash-has-key? lb-dict context)
        (set! cur-time (hash-ref lb-dict context)) 
        (begin
          (set! cur-time (gensym)) ; if this let term has not been attached label before 
          (hash-set! lb-dict context cur-time)))
       cur-time]
      [else (error "not a call, we don't get the time for non-call expression!")]))
    
  
  ; summarize : set[state] -> store
  ; extracts and combines the type store from all states.
  (define (summarize states) 
    (for/fold ([store empty-store])
      ([state (in-set states)])
      (store-join (state-store state) store)))
  
  
  
  (define empty-mono-store (make-immutable-hasheq empty))
  ;
  (define empty-mono-mstore (make-hash))
  
  ; monovariant-store : store -> alist[var,exp]
  (define (monovariant-imstore store)
    (DEBUG "store")
    ;(pretty-write store)
    (for/fold ([mono-store empty-mono-store])
      ([(b vs) (in-hash store)])
      (hash-update mono-store
                   (binding-var b)  ;(car b);               
                   (lambda (b-vs)
                     (set-union 
                      b-vs                    
                      (map-set monovariant-value vs)))
                   empty-set)
      
      ))
  
  (define (monovariant-mstore store)
    (define mono-store empty-mono-mstore)
    
    (for ([(b vs) (in-hash store)])
      (when (binding? b)
        (hash-update! mono-store
                      (binding-var b) ;(car b);                
                      (lambda (b-vs)
                        (set-union 
                         b-vs                    
                         (map-set monovariant-value vs)))
                      empty-set)))
    mono-store)
  
;  (define (binding? b)
;    (match b
;      [`(,var ,time) #t]
;      [else #f]))
  
  
  ; monovariant-value : val -> exp
  (define monovariant-value
    (match-lambda
      [(? closure? c) 'class]
      ;[(? closure? c) 'proc]
      ; [(? closure? c) (car c)]
      ;[(? bas-types? ty) ty ]
      [(? string? s) s]
      [(? $halt? h) '$halt]
      [(? kont-addr? ka) (car ka)];binding-var]
      [(? void? v) v]
      [(? number? num) num]
      [(? hash? h) h]
      [(? symbol? s) ;;; will detect OOdict container type
       s] 
       [else
       'exp]
      ;[(? types-of-unknown-symb? uk) uk]
      ;      [else 
      ;       (error (format "no movariant value"))
      ;       ]
      )) 
  
  
  
  ; print-mono-summary : mono-summary -> void
  (define (print-mono-summary ms) 
    (for ([(i vs) (in-hash ms)])
      (when
          (and 
           (symbol? i)
           (not (or 
                 (equal? (substring (symbol->string i) 0 1) "_")
                 (equal? (substring (symbol->string i) 0 1) "b")
                 (equal? (substring (symbol->string i) 0 1) "l")
                 ))
           (not (or (equal? i 'void) (equal? i '(void)))))
        
        
        (printf "~a:~n" i)
        (for ([v (in-set vs)])
          (printf "\t~S~n" v))
        (printf "~n"))))
  
  
  ;; For special store
  (define (is-defined-var? v symb-record)
    (if (not (symbol? v))
        #f  
        (if (not-keywords-symbol? v)
            (hash-has-key? symb-record v)
            #f)))
  
  (define (is-var? v)
    (if (and (symbol? v) (not-keywords-symbol? v))
        #t
        #f))
  
  (define (type-of-unknown-symb aexp)
    (string->symbol (string-append "Type-of-" (symbol->string aexp))))
  (define (types-of-unknown-symb? s)
    (if (equal? #f (regexp-match #rx"Type-of-." (symbol->string s)))
        #f
        #t))
  
  ;; some types 
  (define bas-types
    (apply set '(int string proc boolean float void tuple py-list dict set pair)))
  
  (define (bas-types? ty)
    (set-member? bas-types ty))
  
  ;;  quotient is tested seperately to give out type of float
  (define arith-operations
    (apply set '(+ - * / >> << quotient modulo expt bitwise-and bitwise-or bitwise-xor bitwise-not)))
  
  ;; this operations gurantee to generate boolean type, also binary
  (define comp-bioperations (apply set '(> < >= <= equal? not-equal? in? not-in? eq? not-eq?))) 
  (define bool-operations  (apply set '( not )))  
  (define test-unoperations (apply set '(integer? string? tuple? dict? py-list? set?)))
  (define container-bioperations (apply set '(py-list-ref py-list-remove! tuple-ref tuple-remove! dict-ref dict-remove!)))
  (define cps-fun-operations (apply set '(for-set-k for-py-list-k for-tuple-k for-dict-k)))
  
  (define (arith-ops? prim-op) (set-member? arith-operations prim-op))
  (define (bool-ops? prim-op) (set-member? bool-operations prim-op))
  (define (comp-biops? prim-op) (set-member? comp-bioperations prim-op))
  (define (test-unops? prim-op) (set-member? test-unoperations prim-op))
  (define (container-biops? prim-op) (set-member? container-bioperations prim-op))
  (define (cps-fun-ops? prim-op) (set-member? cps-fun-operations prim-op))
  
  (define (not-keywords-symbol? s) 
    (match s
      [(or (? arith-ops?) (? bool-ops?) (? comp-biops?) (? test-unops?) (? container-biops?) '$halt 'py-list 'dict 'set 'tuple 'undef) #f]
      [_ #t]))
  
  (define ($halt? h) (if (equal? '$halt h) #t #f))
  
  
  (define (lam? exp)
    (match exp
      [`(lambda (,formals ...) ,_ ...) #t]
      [`((lambda (,formals ..) ,_ ...) ,obj-dict) #t]
      [`(lambda ,formals ,_ ...) #t]
      [else #f]))
  
  
  ;;; kont?
  (define (kont-addr? k)
    (binding? k))
  
  ;; watch for final state
  (define (is-final? st)
    (match-define (struct state (context benv store ostore kont-addr measure time)) st)
    ;(if (and (> (hash-count store) 0) (empty? kont-addr))
    (if (equal? '$halt context)
        #t
        #f))
  
  
  (define (closure? exp)
    (match exp
      [`((lambda (,_ ...) . ,_) ,_) #t]
      [`((lambda v void) ,_) #t]
      [else #f]))
  
  (define (void? v)
    (match v
      [(or `(void) `void) #t]
      
      [else #f]))
  
  (define (ae-list? al)
    (match al
      [`(list ,e ...) #t]
      [else #f]))
  
  (define (get-list-set al)
    (match al
      [`(list ,e ...)  (set  e)]))
  
  (define (infer-types-from-container-op op)
    (match op
      [(or 'py-list-set! 'py-list-ref 'py-list-remove! ) (set 'py-list)]
      [(or 'dict-set! 'dict-ref 'dict-remove!)   (set 'dict)]
      [(or 'tuple-set! 'tuple-ref 'tuple-remove!)   (set 'tuple)]))
  
  
  ;; added to deal with the p4 cps
  ; define? : term -> boolean
  (define (define? sx)
    (match sx
      [`(define . ,_) #t]
      [else           #f]))
  (define (not-define? sx) (not (define? sx)))
  
  ;; skips define in the p4 cps
  (define (kcfa-skip-def stmts) (car (filter not-define? stmts)))
  
  (define (get-defs stmts) (filter define? stmts))
  
  (define (init-benv defs)
    ;(define cur-time (gensym))
    ;(define time* (tick cur-time time-zero))
    
    (for/fold ([benv0 empty-benv])
      ([elem (in-list defs)])
      (match elem
        [`(define ,var ,_) (hash-set benv0 var ((alloc time-zero) var))]
        )))
  
  (define (init-measure intial-benv)
    (define measure0 empty-mstore)
    (for ([(k v) (in-hash intial-benv)])
      (hash-set! measure0 v 0)
      )
    measure0
    )
  
  (define (init-store benv)
    (define store0 empty-store)
    (for ([(k v) (in-hash benv)])
      (hash-set! store0 v (set))
      )
    store0
    )
  
  (define (init-ostore benv)
    (define ostore0 empty-ostore)
    (for ([(k v) (in-hash benv)])
      (hash-set! ostore0 v (set))
      )
    ostore0
    )
  
  
  
  (define (replace-lst-elm lst index new-value)
    (define len (length lst))
    (cond
      [(equal? index 0) (append (list new-value) (drop lst 1))]
      [(equal? index (sub1 len)) (append (drop-right lst 1) (list new-value))]
      [(and (> index 0) (< index len)) (append (drop-right lst (- len index)) (list new-value) (drop lst (add1 index)))]))
  
  
  (define (dict-ref d k)
    (match d
      [`(dict (,keys ,values) ...) 
       (define tmp-hs (make-hash empty))
       (for 
           ([key (in-list keys)]
            [val (in-list values)])
         (hash-set! tmp-hs key val)
         )        
       (if (hash-has-key? tmp-hs k)
           (hash-ref tmp-hs k) 
           ;; if we can not find the value for the k now, we give back it the value that was the same type of the previous record. Btest effort.
           (list-ref values 0))
       ]
      [else (error (format "~a  not found in ~a \n" k d))]))
  
  (define (get-elem-from-singleton-set s)
    (for/fold ([lst (list)])
      ([e (in-set s)])
      e))
  ;; addditional helper for get value from singleton set after looking up in the value store
  (define (get-value v ostore)
    ;(display "in get value:\n")
    (if (binding? v)
        (begin
          ;(display "in get value:\n")
          ;(display (get-elem-from-singleton-set (store-lookup ostore v)))
          (get-elem-from-singleton-set (store-lookup ostore v)))
        (get-elem-from-singleton-set v)))
  
  (define (copy-benv cc-benv)
    (for/fold ([hs (make-immutable-hasheq empty)])
      ([(k v) (in-hash cc-benv)])
      (hash-set hs k v)))
  
  ;;should not have side effect on the cc-benv! 
  ;; use merge-benv2!
  (define (merge-benv cc-benv next-benv)
    (for/fold ([hs cc-benv])
      ([(k v) (in-hash next-benv)])
      (if (hash-has-key? hs k)
          (if (or (equal? k 'cc) (equal? k 'return) )
              ;(if  (equal? k 'cc) 
              (hash-set hs k v)
              (hash-set hs k (hash-ref hs k)))
          (hash-set hs k v))))
  
  (define (merge-benv2 cc-benv next-benv)
    (for/fold ([hs (copy-benv cc-benv)])
      ([(k v) (in-hash next-benv)])
      (if (hash-has-key? hs k)
          (if (or (equal? k 'cc) (equal? k 'return) (equal? k 'x))
              ;(if  (equal? k 'cc) 
              (hash-set hs k v)
              (hash-set hs k (hash-ref hs k)))
          (hash-set hs k v))))
  
  ;;;; helper for container-k ops.
  (define (get-container-name s)
    (string->symbol (list->string (drop-right (string->list (symbol->string s)) 2))))
  
  (define (get-obj container-k ae-lst)
    (define container-name (get-container-name container-k))
    (append (list container-name) ae-lst))
  
  ;; helper function in the container ref and container set to build the obj.f
  ;; attention: the type of reference now is a number, string, symbol, or bool
  (define (build-obj-field obj index)
    (if (number? index)
        (string->symbol (string-append (symbol->string obj) "." (number->string index)))
        (if (string? index)
            (string->symbol (string-append (symbol->string obj) "."  index))
            (string->symbol (string-append (symbol->string obj) "." (format "~a"  index))))))
  
  ; a list of obj.f format
  (define (build-obj-field* obj-lst var)
    (for/list ([elem (in-list obj-lst)])
      (build-obj-field elem var)))
  
  ;; helper function to get the chaining alias objects
  ;; used in the set-field!
  (define (build-connectedVar-lst svar benv sstore)
    (let ((next-obj (store-lookup sstore (benv-lookup benv svar))))
      (if (set-empty? next-obj)
          '()
          (begin
            (let ((next-obj-v (get-elem-from-singleton-set next-obj)))
              (cons next-obj-v (build-connectedVar-lst next-obj-v benv sstore)))))))
  
  ;; used in object-set field! 
  ;; to get bindings for all a list of obj.f
  (define (build-bindings objflst time* benv)
    (for/list ([elem (in-list objflst)])
      (if (hash-has-key? benv elem)
          (benv-lookup benv elem)
          ((alloc time*) elem))))
  
;  (define (build-allocs binding-lst time* vstore)
;    (for/list ([b (in-list binding-lst)])
;      (if (set-empty? (store-lookup vstore b))
;          ((alloc time*) (binding-var b))
;          (get-elem-from-singleton-set (store-lookup vstore b)))))
  
  
  ; used in set-obj-field to synchronization
  (define (clone len)
    (lambda (v)
      (if (= len 0)
          '()
          (cons v ((clone (sub1 len)) v)))))
  
  ;for a set of values from obj-alias-d*, get the objs from its according ostore 
  (define (get-obj.f-objs obj.f-alias-d* ostore)
    (for/fold ([obj-s (set)])
      ([elem (in-set obj.f-alias-d*)])
      (if (binding? elem)
          (set-union obj-s (store-lookup ostore elem))
          (set-union obj-s (set)))))
  
  ; used in get-field
  (define (get-objs obj-addres ostore)
    (for/fold ([objset (set)])
      ([elem (in-set obj-addres)])
      (if (binding? elem)    ; safe to test if elem is address or not                    
          (set-union objset (store-lookup ostore elem))
          (set-union objset (set)))))
  
  ;;;; newly added to get the final type inference result with using store (type store)!!!
  (define (infer-bas-type v)
    (match v
      ['$halt (set 'proc)]    
      [(? closure?)   (set  'proc)]
      [(? number?) (set   'int)]
      [(? string? ) (set   'string)]
      [(? boolean? )  (set  'boolean)]
      [(? void? )   (set)]
      [(? types-of-unknown-symb?)  (set v)]
      [else (pretty-write (format "~a not matched in infer-bas-types!" v))]))
  
  
  (define (infer-obj-types-nocontour objs)
    (for/fold ([res (set)])
      ([obj (in-set objs)])
      (match obj
        [(? pyset?)      (set-add res  'set)]
        [(? tuple?)    (set-add res'tuple)]
        [(? py-list?)  (set-add res 'py-list)]
        [(? dict?)    (set-add res 'dict)]  )))
  
  (define (get-final-types vs ostore)
    (for/fold ([res (set)])
      ([v (in-set vs)])      
      (if (not (binding? v))
          (set-union res (infer-bas-type v))
          (set-union res (infer-obj-types-nocontour (store-lookup ostore v))))))
  
  
  
  (define (monovariant-store2 mono-vstore mono-ostore)
    (for/fold ([mono-store empty-mono-store])
      ([(b vs) (in-hash mono-vstore)])
      (hash-update mono-store
                   (binding-var b);(car b);
                   (lambda (b-vs)
                     (set-union 
                      b-vs                    
                      (get-final-types vs mono-ostore)))                   
                   empty-set)))
  
  ;; the same version as the above, except do the best effort to generate values.
  (define (best-effort-arith-value  v1 v2 op )
    (cond 
      [(arith-ops? op)
       (cond 
         [(integer? v1)  
          (if (integer? v2)
              (match-n-compute v1 v2 op);0; best effort
              (if (and (number? v2) (not (integer? v2)))
                  0.0001
                  (if (string? v2)
                      "string"
                      'void)))]
         [(string? v1)
          (if (integer? v2)
              "string"
              'void)]
         ;[(or (boolean? t1) (equal? t1 'proc)) 'void]
         [(and (number? v2) (not (integer? v2))) 0.0001]
         [else 'void])]
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
      [`bitwise-xor (bitwise-xor v1 v2) ]
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
  
  (define (py-list? base store ostore)
    ;(pretty-write base)
    (if (OO-loc? base)
        (begin
          (DEBUG (format "py-list?: ~a is a OO-loc" base))
          
          (let ((tmp1 (membPl base "__type__")))
            (if (hash-has-key? store tmp1)
                (begin
                  (let ((type-str (get-elem-from-singleton-set (store-lookup store tmp1))))
                    (if (equal? type-str "instPyList")
                        #t
                        #f)
                    ))
                (begin
                  (error (format "py-list?: ostore has no __type__ ~a " tmp1))
                  ) 
                )))
        (begin
          (DEBUG (format "py-list?: ~a is NOT a OO-loc" base))
          #f
          )
        ))
  
  (define (pyset? base store ostore)
    (if (OO-loc? base)
        (begin
          (DEBUG (format "pyset?: ~a is a OO-loc" base))
          
          (let ((tmp1 (membPl base "__type__")))
            (if (hash-has-key? store tmp1)
                (begin
                  (let ((type-str (get-elem-from-singleton-set (store-lookup store tmp1))))
                    (if (equal? type-str "instSet")
                        #t
                        #f)
                    ))
                (begin
                  (error (format "py-set?: ostore has no __type__ ~a " tmp1))
                  ))
            ))
        (begin
          (DEBUG (format "pyset?: ~a is NOT a OO-loc" base))
          #f
          )
        ))
  
  (define (tuple? base store ostore)
    (if (OO-loc? base)
        (begin
          (DEBUG (format "tuple?: ~a is a OO-loc" base))
          
          (let ((tmp1 (membPl base "__type__")))
            (if (hash-has-key? store tmp1)
                (begin
                  (let ((type-str (get-elem-from-singleton-set (store-lookup store tmp1))))
                    (if (equal? type-str "instTuple")
                        (begin
                          (DEBUG "ohhhhhhhhhhhhh inst tuple!!!")
                          #t)
                        #f)
                    ))
                (begin
                  (error (format "tuple?: ostore has no __type__ ~a " tmp1))
                  )
                )))
        (begin
          (DEBUG (format "tuple?: ~a is NOT a OO-loc" base))
          #f
          )
        ))
  
  
  
  ;; lessons --- you cannot test dict? like this! since all your dict is "desolved"!
  (define (dict?-wrong base)
    (DEBUG "here you are:")
    (DEBUG base)
    (match base
      [`(dict (,k ,v) ...) #t]
      [else #f]))
  
  ;; since the dict is not in the OO system, yet, then the testing dict normally testing whether it is ordinary dict or OO dict
  (define (dict? base)
    (not (OO-loc? base)))
  
  
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
  
  
  
  (define (atom? e)
    (cond 
      [(or (number? e) 
           (string? e)
           (symbol? e)
           (lam? e)
           (kont-addr? e)
           (boolean? e)
           (empty? e)
           (void? e)
           (set? e)
           (ae-list? e)
           )
       #t ]
      [else #f]))
  
  
  
  ; for container biop- ref
  (define (get-ref-vals base-set index store ostore biop)
    (for/fold ([res-set empty-set])
      ([base-l (in-set base-set)])
      (cond 
        [(or (equal? 'py-list-ref biop)
             (equal? 'tuple-ref  biop))
         (let ((membpcd (membPl base-l "__containerdict__")))
           (if (hash-has-key? store membpcd)
               (let  ((container-dict-l (get-elem-from-singleton-set (store-lookup store membpcd))))
                 (set-union res-set (store-lookup store (membPl container-dict-l index))))
               (error (format "get-ref-vals in biop: no __containerdict__: ~a" base-l))))
         ]
        [(equal? 'dict-ref biop )
         (let ((membp (membPl base-l index)))
           (set-union res-set (store-lookup store membp)))])))
  
  
  
  ;; abstract version of the travel-mro
  (define (travel-mro var-clses var-fld str-type l-inst l-super benv store ostore)
    (if (empty? var-clses)
        (error (format "~a not found in mro list ~a" var-fld var-clses))
        (let* ((var-cls (car var-clses))
               (l-cls (get-elem-from-singleton-set
                       (store-lookup ostore (benv-lookup benv var-cls))))
               (membp (membPl l-cls var-fld)))
          (if (not (hash-has-key? store membp))
              (travel-mro (drop var-clses 1) var-fld str-type l-inst l-super benv store ostore)
              (cond
                [(equal? str-type "class")
                 (store-lookup store membp)
                 ]
                ;; inst and super both have to consider function member or normal value member
                [(equal? str-type "inst")
                 (let ((member (get-elem-from-singleton-set (store-lookup store membp))))
                   (match member 
                     [`((lambda (,formals ...) ,body ...) ,benv*)
                      (set `(((lambda ,formals ,@body) ,l-inst) ,benv*))
                      ]
                     [`((lambda ,v ,body ...) ,benv*)
                      (DEBUG "in -mro")
                      (DEBUG member)
                      (set `((lambda () ,body) ,benv*))
                      ]
                     ; if not closure, then should be normal value memeber
                     [else
                      (store-lookup store membp)
                      ]
                     ))
                 ]
                [(equal? str-type "super")
                 (let ((member (get-elem-from-singleton-set (store-lookup store membp))))
                   (match member 
                     [`((lambda (,formals ...) ,body ...) ,benv*)
                      (set `(((lambda ,formals ,@body) ,l-super) ,benv*))
                      ]
                     [`((lambda ,v ,body ...) ,benv*)
                      (DEBUG "in -mro")
                      (DEBUG member)
                      (set `((lambda () ,body) ,benv*))
                      ]
                     [else
                      (store-lookup store membp)
                      ;(error (format "~a not matched in the travel-mro super closure!" member))
                      ]
                     ))
                 ]
                [else
                 (error (format ("~a not the type supported in travel-mro" str-type)))
                 ])))))
  
  
  ;; helper functions for OO dict.
  
  ;; examine th  key-value pair 
  ;; what about class dict?
  (define (OO-dict? fld-lst ae-lst)
    
    (define dicths (make-hash))
    (define lst-notused
      (map (λ (x y) (hash-set! dicths x y)) fld-lst ae-lst))
    
    (if (hash-has-key? dicths "__type__")
        
        (let ((val (hash-ref dicths "__type__")))
          (if (or (equal? val "class") 
                  (equal? (substring val 0 4) "inst")
                  )
              #t
              #f
              ))
        ; temperarily normal dict is without type  field, which is not OO dict.
        ; the above is wrong. the class dict is also should be OO-dict has __base__
        (begin
          (if (hash-has-key? dicths "__mro__")
              #t
              #f)
          )))
  
  ; second version of OO dict test based on location
  (define (OO-loc-dict? l-dict ostore)
    
    ;; OO dict all has the "__type__" field.
    ;; note the dict is temperarily not implemented in OO system
    
    (let ((membtp (membPl l-dict "__type__")))
      
      (cond
        [(hash-has-key? ostore membtp) #t]
        [else #f]
        )))
  
  (define (OO-loc? locsym)
    (if (and (symbol? locsym)  (equal? (substring (symbol->string locsym) 0 2) "OO"))
        #t
        #f))
  
  
  (define (OO-dict-set? atom-set)
    ;if singleton set and the element is oo-loc,
    (if (and (set? atom-set) 
             (equal? (set-count atom-set) 1)
             ;(or (closure? (get-elem-from-singleton-set atom-set)) 
                 (OO-loc? (get-elem-from-singleton-set atom-set)))
        #t
        #f))
  

  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;; Should compare the store!!!!;;;;;;;;;;;;;;;;;;;
  (define (cesk-equal? seen st)
    
    (define exp (state-context st))
    
    (define env (state-benv st))
    (define kont (state-kont-addr st))
               
    (define store (state-store st))
    (define ostore (state-ostore st))
    
    (let ((result 
           (for/fold ([res (list)])
             ([elem (in-set seen)])
             
             (if (and (equal? (state-context elem) exp)
                      (equal? (state-benv elem) env)
                      (equal? (state-kont-addr elem) kont)
                      (equal? (state-store elem) store)
                      (equal? (state-ostore elem) ostore)
                      )
                 (begin
                   (pretty-write (state-context elem))
                   (pretty-write exp)
                   (pretty-write (state-kont-addr elem) )
                   (pretty-write kont)
                   (pretty-write (state-benv elem))
                   (pretty-write env)
                   (append res (list elem))
                   )
                 (append res (list))))))
      (if (empty? result) #f #t)))
  
  (define (seen? seen st) 
    
    (set-member? seen st))
        
  
  
  
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
  
  
  ;; built-in class in glb-dict
  ;; which is predefined dict!
  
  (define (builtin-cls-in-dict? k-lst)
    (or (member 'object k-lst)
        (member 'List   k-lst)
        (member 'tuple  k-lst)
        (member 'set    k-lst)))
  
  
  ; help for for-container
   ; hashtable -> pattern -> [membPl]
   ; the complexity is linear.
  (define (get-raw-addrs store pattern)
    
      (for/fold ([res (list)])
        ([addr (in-list (hash-keys store))])
        (match addr
          [`(,p ,_) 
           (if (equal? p pattern)
               (append res (list addr))
               (append res (list)))
           ]
          [else
           (append res (list))]
    )))
  
 
  ;; [memPl] -> [memePl]
  ;; get the ordered addrs based on the second part of the addr, 
  ;; which is the numerical index for list set and tuple
  ;; the complexity here is linear
  (define (indexed-hash raw-addrs)
    (let ((new-hash (make-hash)))
      (map (lambda (e) 
             (hash-set! new-hash (second e) e))
           raw-addrs)
      new-hash))
  
  ;; hash -> hash -> [Val]
  ;; based on the ordered key addr, we can get the according values now.
  ;; the complexity is still linear
  
  (define (get-orderedkey-vals indexed-hash store)
     (let ((sorted-keys (sort (hash-keys indexed-hash) <)))
       (map (lambda (x) (hash-ref store (hash-ref indexed-hash x))) sorted-keys)))
       ;(map (lambda (x) (get-elem-from-singleton-set (hash-ref store (hash-ref indexed-hash x)))) sorted-keys)))
   ;; usage: (get-orderedKey-vals (index-hash (get-raw-addrs store pattern)) store)
 
  
  
  )
