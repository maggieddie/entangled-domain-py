#lang racket

(require "pycesk-taint.rkt")
(require "utils-cesk-taint.rkt")

(define empty-mono-store (make-immutable-hasheq empty))
(define empty-mono-ostore (make-immutable-hasheq empty))
(define empty-summarized-ostore (make-immutable-hash empty)) ; which is (OLoc -> Hash Val : Set Val)


;; collapsing stores of all the reachable states
;; collapse-store :: Set state -> store
(define (collapse-store states)
  (foldr (λ (s res-store) (store-join res-store (state-store s)))
         empty-store
         (set->list states)))

;; helper: given my new model of a curried Obj, we extract all possible fields and their correpondence val set
;; the Obj is transformed into a hash from field val to val sets
;; obj is from the domain of ostore
;; extract-field-vals :: Obj -> (Hash Val : Set Val)
(define (extract-field-vals obj)
  (local ([define obj-map (first obj)]
          [define elem-set-map (second obj)])
    (foldr (λ (k res-imhs)
             (hash-set res-imhs k (obj-lookup (set k) obj)))
           (make-immutable-hash empty)
           (hash-keys elem-set-map))))
;; this can be the same) for taint stores

;; helper: return a ostore with location mapping to object field to set of val map
;; summarize-ostore :: ostore -> summaized-ostore which is (OLoc -> Hash Val : Set Val)
(define (summarize-ostore ostore)
  (foldr (λ (k nos) (hash-set nos  k (extract-field-vals (hash-ref ostore k))))
         empty-summarized-ostore
         (hash-keys ostore)))
;; the same as above
(define (summarize-taint-ostore taint-ostore)
  (foldr (λ (k tnos) (hash-set tnos  k (extract-field-vals (hash-ref taint-ostore k))))
         empty-summarized-ostore
         (hash-keys taint-ostore)))
  

;; helper: summarized-ostore is the location mapping to field to val-set (which we call obj-map)
;; given an location and a correspondent obj-map, if the same location found, 
;; we use the store-join to merge the two obj-map. if no location found, we just extend
;; the entry of loc to obj-map
;; summarized-ostore-extend :: summarized-ostore -> OLoc -> (hash field (set val)) -> summarized-ostore
(define (summarized-ostore-extend tos loc obj-map)
  (hash-update tos loc 
                 (lambda (m) (store-join m obj-map))
                 obj-map))
;; this can be the same for taint store

;; helper: given two ostore, we first summarize the two stores; then we join the two summarized ostores.
;; the result summarize-ostore is every location mapping to the merged hash map from field to set of val
;; ostore-summarize-join ::  ostore -> ostore -> summarized-ostore
(define (ostore-summarize-join ostore1 ostore2)
  (define summarized-ostore1 (summarize-ostore ostore1))
  ( define summarized-ostore2 (summarize-ostore ostore2))
  ;(displayln "never reaching here")
  (for/fold ([new-ostore summarized-ostore1])
    ([(loc obj-map) (in-hash summarized-ostore2)])
    (summarized-ostore-extend new-ostore loc obj-map)))

;; for the taint-ostore-summarize
(define (taint-ostore-summarize-join tostore1 tostore2)
  (define summarized-tostore1 (summarize-taint-ostore tostore1))
  ( define summarized-tostore2 (summarize-taint-ostore tostore2))
  ;(displayln "never reaching here")
  (for/fold ([new-tostore summarized-tostore1])
    ([(loc obj-map) (in-hash summarized-tostore2)])
    (summarized-ostore-extend new-tostore loc obj-map)))

;; collapsing ostores of all the reachable states
;; collapse-ostore :: Set state -> summarized-store
;(define (collapse-ostore states)
;  (foldr (λ (s res-sostore) 
;           (ostore-summarize-join res-sostore  (state-ostore s)))
;         empty-ostore
;         (set->list states)))

;; turn the ostore components in a state into a list of summarized store, 
;; then you iterate teh store, accumulate the store using new version of 
;; ostore-summarize-join, which is just working on summarized ostore

;; to-summarized-stores :: (Set State) - [summarized-ostore]
(define (to-summarized-stores states)
 (foldr (λ (s sst)
          (append sst (list (summarize-ostore (state-ostore s)))))
        (list)
        (set->list states)))

(define (to-summarized-taint-stores states)
   (foldr (λ (s sst)
          (append sst (list (summarize-taint-ostore (state-taint-ostore s)))))
        (list)
        (set->list states)))
  

;; the new ostore-summarized-join :: summarized ostore1 -> summarized ostore2 -> summarzied store
(define (ostore-summarized-join sostore1 sostore2)
  (for/fold ([new-ostore sostore1])
    ([(loc obj-map) (in-hash sostore2)])
    (summarized-ostore-extend new-ostore loc obj-map)))
;; the above can be the same in taint stores

;; the collapsing ostores in set of states
;; collapse-ostore :: Set state -> summarized-store
(define (collapse-ostore states)
  (foldr (λ (sos res-sostore) 
           (ostore-summarized-join res-sostore  sos))
         empty-summarized-ostore
         (to-summarized-stores states)))

(define (collapse-taint-ostore states)
  (foldr (λ (sos res-sostore) 
           (ostore-summarized-join res-sostore  sos))
         empty-summarized-ostore
         (to-summarized-taint-stores states)))

;; sorry, the global label dict, using equal to compare keys, assuming 
;; that the program does not write dupilicated call like (f 1) and (f 1).
(define gbl-lbl-dict (make-hash empty))

;; this auxilary program is just walk over the syntax tree, 
;; and generate new labels to call sites expressions
;; the mapping from call site to label are stored in gbl-lbl-dict
(define (attach-lbl-to-calls prog-no-defs)
  (match prog-no-defs
    [`(if ,ae ,tce ,fce) (attach-lbl-to-calls tce)
                         (attach-lbl-to-calls fce)]
    [`(let ((,v ,ce)) ,e) 
     (hash-update! gbl-lbl-dict prog-no-defs 
                                   (λ (value) value);found the key
                                   (gensym))
     (attach-lbl-to-calls ce)
     (attach-lbl-to-calls e)]
    [`(call/ec ,f)  (hash-update! gbl-lbl-dict prog-no-defs 
                                   (λ (value) value);found the key
                                   (gensym))]
    [`(error ,ae) (void)]
    [`(set! ,_ ,_) (void)]
    [`(set-field! ,_ ,_ ,_) (void)]
    [`((anf dict-set!) gbl-dict ,_ ,_) (void)]
    [`((anf dict-ref) gbl-dict ,_) (void) ]
    [`((anf ,_) ,_ ,_) (void)]
    [`(dict (,_ ,_) ...) (void)]
    [`(get-field ,_ ,_) (void)]
    [`((anf ,_) ,_) (void)]
    [`((anf ,_) ,_ ,_ ,_) (void)]
    [(or `(void) `void) (void)]
    [`(list ,_ ...)  (void)]
    [(or `(lambda (,formals ...) ,exp) `(lambda ,formals ,exp))
     (attach-lbl-to-calls exp)]
    
    [`(,f ,args ...) (hash-update! gbl-lbl-dict prog-no-defs 
                                   (λ (value) value);found the key
                                   (gensym))] ; not found the expression, then new label
    [else (void)];(displayln "progam construct not matched during attach label!")  (displayln prog-no-defs)]
    ))

;; monovariant-store :: collecpsed-store -> store that maps variables to all its possible set
(define (monovariant-store cstore )
  ;(pretty-write cstore)
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash cstore)])
    (match b
      [(struct BAddr (v t)) 
       (hash-update mono-store 
                    v
                    (lambda (b-vs)
                      (set-union b-vs (map-set monovariant-value vs)))
                    empty-set)]
      [(struct KAddr (v t)) mono-store])))

(define monovariant-value 
  (match-lambda
    [(? closure? c) (list 'proc c)]
    [(? letK? letk) (list 'letK letk)]
    [(? OLoc? l) (list 'inst l)] ; will need to pull out the object field 
    [(? Cls? cls) (list 'class cls)] ;will need to pull out the class field 
    [(? kontP? kp) (list 'kontP kp)]
    [(? string? s) (list 'string s)]
    [(? symbol? sym) (list 'symbol sym)]
    [(? number? num) (list 'num num)]
    [(? boolean? b) (list 'bool b)]
    [(? void? v) (list 'void v)]
    [(? halt? h) (list 'halt h)]
    [else (list 'exp 'exp) ]))

(define (user-var? var)
  (cond 
    [(and (symbol? var) (or (equal? 'body var) (equal? 'resp var) (equal? 'b225 var) (equal? 'b226 var))) #f]
    [(and (symbol? var)
          (not (or 
                 (regexp-match #rx"self[0-9]*" (symbol->string var))
                 (regexp-match #rx"return[0-9]*" (symbol->string var))
                 (regexp-match #rx"continue[0-9]*" (symbol->string var))
                 (regexp-match #rx"break[0-9]*" (symbol->string var))
                 (regexp-match #rx"b[0-9]+" (symbol->string var))
                 (regexp-match #rx"i[0-9]+" (symbol->string var))
                 (regexp-match #rx"e[0-9]+" (symbol->string var))
                 (regexp-match #rx"t[0-9]+" (symbol->string var))
                 (regexp-match #rx"seq[0-9]+" (symbol->string var))
                 (regexp-match #rx"container[0-9]+" (symbol->string var))
                 ;(regexp-match #rx"_[0-9]*" (symbol->string var))
                 (equal? (substring (symbol->string var) 0 1) "_")
                 ;(regexp-match #rx"$[0-9]*" (symbol->string var))
                 (equal? (substring (symbol->string var) 0 1) "$")
                 ;(equal? (substring (symbol->string var) 0 1) "b")
                 ;(equal? (substring (symbol->string var) 0 1) "t")
                 ;(equal? (substring (symbol->string var) 0 1) "i")
                 ;(equal? (substring (symbol->string var) 0 1) "e")
;                 (regexp-match #rx"t[0-9]*" (symbol->string var))
;                 (regexp-match #rx"i[0-9]*" (symbol->string var))
;                 (regexp-match #rx"e[0-9]*" (symbol->string var))
;                 (regexp-match #rx"b[0-9]*" (symbol->string var))
                 ))
           (not (or (equal? var 'void) (equal? var '(void))))) #t] 
    [else #f]))

(define terms 0)
(define singleton-set 0)
(define num-inst 0)
(define cardi-other 0)
(define field-cardi-inst 0)
(define inst-to-cardi (make-hash))
(define inst-single-cardi 0)



(define (prt-hash hs)
  (for ([(k v) (in-hash hs)])
    (printf "\t\t\t\t~S ~S ~n" k v)))

;;for class, we would like to unfold the values and its according property
;; set, like tainted or not, or the literal values

(define (print-inst-summary str os tos loc)
  (define inst-hs (hash-ref os loc))
  (define inst-hash-taint (hash-ref tos loc))
  (cond
    [(equal? str "v") (printf "\t\t fields and values are: ~n") ]
    [(equal? str "t") (printf "\t\t fields and taints are: ~n") ])
  (for ([(kv vv) (in-hash inst-hs)]
        [(kt vt) (in-hash inst-hash-taint)])
    (cond 
      [ (or (OLoc? (get-elem-from-singleton-set vv))  (equal? "__containerdict__" kv))
       (define v-e (get-elem-from-singleton-set vv)) ; a location
       (define elems-hash #f)
       (cond 
         [(equal? "__containerdict__" kv) (printf "\t\t\t \"details:\" ~n")]
         [(OLoc? (get-elem-from-singleton-set vv)) (printf "\t\t\t ~S ~n" kv)])
       (cond
         [(equal? "v" str) 
          (set! elems-hash (hash-ref os v-e))]
         [(equal? "t" str) 
          (set! elems-hash (hash-ref tos v-e))])
       (prt-hash elems-hash)]
      [else
       (cond
         [(equal? "v" str) (printf "\t\t\t ~S ~S ~n " kv vv) ] 
         [(equal? "t" str) (printf "\t\t\t ~S ~S ~n " kt vt) ]
        )])))

(define (print-vulnerabilities)
  (printf "~n!!! Vulnerabilities #: ~a ~n " (hash-count vulnerabilities) )
  (for ([(k v) (in-hash vulnerabilities)])
    (printf "~n ~a: ~a ~n" k v))
  (displayln "--------"))

(define (print-mono-summary ms sostore staintostore)
  (for ([(k vs) (in-hash ms)])
    (when (user-var? k)
      (printf "~n ~a: ~n" k )
      (set! terms (add1 terms))
      (cond
        [(equal? (set-count vs) 1) (set! singleton-set (add1 singleton-set ))]
        [else  (set! cardi-other (add1 cardi-other))])
      
      (for ([lv (in-set vs)])
        (match (first lv)
          ['inst (printf "\t inst ") 
                   (newline)
                   (set! num-inst (add1 num-inst))
                   (print-inst-summary  "t" sostore staintostore (second lv))
                   (print-inst-summary "v" sostore staintostore (second lv))]
                   ;(printf "\t\t~S~n" (hash-ref staintostore (second lv)))
                  ; (printf "\t\t~S~n"(hash-ref sostore (second lv)))]
          ['cls 
           (printf "\t class ") 
           (newline)
           (printf "\t~S~n" (hash-ref staintostore (Cls-oloc (second lv))))
           (printf "\t~S~n" (hash-ref sostore (Cls-oloc (second lv))))]
          ['proc (printf "\t~a~n" (closure-lambda (second lv)))]
          [else (printf "\t~a~n" (second lv))]))))
  (printf "# of terms: ~a ~n" terms)
  (printf "# of singleton set of terms: ~a ~n" singleton-set)
   (printf "% of singleton set in all terms: ~a ~n" (/ singleton-set terms))
  (printf "# of inst: ~a ~n: " num-inst)

 
  )


;; given a progam, explores all the states, summarize stores, and print summary
(define (run-analysis program)
  (define prog (match program [`(program . ,stmts) stmts]))
  (define defs (get-defs prog))
  ;(DEBUG defs)
  (set! prog (no-defs prog)) ; yeah, I know, again, side effects.
  ;(DEBUG prog)
  ; attach new labels to programs before the analysis is run
  (attach-lbl-to-calls prog)
  (parameterize ([k 0])
    ; set up initial components
    (define benv0 (init-benv defs))
    (define store0 (init-store benv0))
    (define ostore0 (init-ostore benv0))
    (define measure0 (init-measure benv0))
    (define taint-store0 (init-taint-store benv0))
    (define taint-ostore0 (init-taint-ostore))
    ; explore all states given initial state injected
    (define reachable-states (time (explore (inject-cesk prog benv0 store0 ostore0 measure0 taint-store0 taint-ostore0))))
    ; summarize stores
    (define collapsed-store (collapse-store reachable-states))
    (define collapsed-ostore (collapse-ostore reachable-states))
    (define collapsed-taint-ostore (collapse-taint-ostore reachable-states))
    ; monovariant stores
    (define mono-store (monovariant-store collapsed-store))
    ; print out XSS
    (print-vulnerabilities)
    ; print out monovariant summary
    (print-mono-summary mono-store collapsed-ostore collapsed-taint-ostore)))

;; IO 
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
     ; (pretty-write (car lines))
      (run-analysis (car lines))
      )))
;
(define args (current-command-line-arguments))
;(match args
;  [(vector filename)   (pycesk-file filename) (exit)]  
;  [else  (pycesk-port (current-input-port))]) 

;(pycesk-file "./experiment/experiment1.anormal.rkt")
(pycesk-file "tests2/taint-backend-part11.py.anormalnt")
;(pycesk-file "microbench/pycfa.py.anormalnt")




  
     
  