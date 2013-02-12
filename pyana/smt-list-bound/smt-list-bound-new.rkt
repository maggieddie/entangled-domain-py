#lang racket

(provide (all-defined-out))
(require rackunit)
(require "utils_cesk3_smt.rkt")

(require "../../main.rkt"
         "../../examples/list-helpers.rkt")

(define-namespace-anchor top)
;; mapping from address to sym-meta
(define smt-symbol-table (make-hash))

(define (update-smt-symbol-table! addr sym-info)
  (hash-set! smt-symbol-table addr sym-info))

;; if exists, return the meta sym
;; else #f
(define (addr-in-smt-sym-table? addr)
  (if (hash-has-key? smt-symbol-table addr)
      (hash-ref smt-symbol-table addr)
      #f))
  
;; kinda symbolic execute len
(define (sym-len? elem)
  (match elem
    ['symbol-len #t]
    [else #f]))

(define (predicate-val? elem)
  (match elem
    ['precicate #t]
    [else #f]))


(define (get-meta  ae-or-addr? ae addr benv store)
  (define binding #f)
  (displayln (format "get-meta: ~a" ae))
  (cond
    [ae-or-addr? ;ae
     (set! binding (benv-lookup benv ae))]
    [else 
     (set! binding addr)])
     (define meta-res (addr-in-smt-sym-table? binding))
     (cond 
       [(not (equal? meta-res #f))
        (sym-meta-sym-name meta-res)]
    [else 
     (error (format "not found in the smtsymtabe! ~a" binding))]))


;; currnelt there are three types  
(define (normal-meta?  res)
  (cond
    [(sym-meta? res) #t]
    [else #f]))
                     

;; for old in val set
;; we would like to extract the singleton oloc,
; if yes, then return the oloc

;; if not singletong oloc, report error
(define (singleton-type-set? valset type)
  (define lstval (set->list valset))
  (define resval #f)
  (cond 
    [(equal? type 'oloc)  
     (set! resval (filter OLoc? lstval))]
    [(equal? type 'int)
     (set! resval (filter num? lstval))]
    [(equal? type 'addr)
     (set! resval (filter BAddr? lstval))]
    ;; There is something wrong there. maybe be careful!
    ;; no, deal with-sym-len used this
    [(equal? type 'symbol-len)
     (set! resval (filter sym-len? lstval))]
    [(equal? type 'predicate)
     (set! resval (filter predicate-val? lstval))]
    )
  
  (define cnt (length resval))
  (cond
    [(= cnt 1)
     (car lstval)]
    [(> cnt 1)
     (displayln "More than one entity  ")
     (car lstval)
     ]
    [else #f]))

;;
(define (symbolic-len? d)
  (cond
    [ (and 
       (= 1 (set-count d))
       (equal? (first (set->list d))
               'symbol-len))
      #t]
    [else #f]))

(define (predicate? d)
  (cond
   [ (and 
     (= 1 (set-count d))
      (equal? (first (set->list d))
               'predicate))
    #t]
   [else
    #f]))
  
    

;; deal with hte dict-set
(define (deal-with-dictset  ae-b ae-f ae-v benv store exp)
  (define base-bind (benv-lookup benv ae-b)) ;; 
   (define smt-sym-info (addr-in-smt-sym-table? base-bind))
  ;(define field-bind (benv-lookup benv ae-f))
 (define val-set ((atom-eval benv store) ae-v))
  
  (define single-int-val (singleton-type-set? val-set 'int)) 
  (cond
    [(not (equal? single-int-val #f))
     (displayln (format "dict-set!: the value to set is integer!!! ~a" single-int-val))
     (cond
       [(not (equal? smt-sym-info #f))
        (define smt-sym-name (sym-meta-sym-name smt-sym-info))
        (displayln (format "dict-set! the smt symbol type is: ~a" (sym-meta-type smt-sym-info)))
        (define new-smt-name (gen-smt-var))
        (add-formula! `(smt:declare-fun ,new-smt-name () IntList))
        (add-formula! `(smt:assert (=/s ,new-smt-name (append ,smt-sym-name (list->z3-list (list ,single-int-val))))))
        (add-formula! `(smt:assert (=/s (len ,new-smt-name) (+/s (len ,smt-sym-name) 1))))
        ;(pretty-write partial-assumption-base)
       ; (displayln (format "the current hash-bind is ~a" base-bind))
        (when (hash-has-key? smt-symbol-table base-bind)
            (begin
              (displayln (format "the base bind is there, ~a" (hash-ref smt-symbol-table base-bind)))))
        (define new-sym-info (sym-meta new-smt-name 'IntList))
        (update-smt-symbol-table! base-bind new-sym-info)
        (newListSym-propogation! smt-sym-info new-sym-info)
        
        (when (hash-has-key? smt-symbol-table base-bind)
            (begin
              (displayln (format "the base bind is there, ~a" (hash-ref smt-symbol-table base-bind)))))
        
        ]
       [else 
        (displayln (format "the smt symble information NOT found in dict set's base: ~a " exp))
        (void)])
     ]
    [else 
     (displayln (format "dict-set!: the value to set is NOT integer!!! ~a" single-int-val))
     (void)])
  
 
 ; (define index-sym-info (addr-in-smt-sym-table? field-bind))
  )

;; to deal with the function call binding
(define (deal-with-func-call-binding bindings argvals benv store args)
  (for ([argval argvals]
        [bind bindings]
        [ae args]
        )
    ;; usually singleton set for oloc list?
    (define oloc-val (singleton-type-set? argval 'oloc ))
   ; (displayln (format "in dealing with function binding ae: ~a ~a " ae argval))
    ;(displayln oloc-val)
    (define int-val (singleton-type-set? argval 'int))
    (define addr-val (singleton-type-set? argval 'addr))
    
    (deal-with-single-abstract-type #f #f ae benv bind oloc-val int-val addr-val)
;     (deal-with-single-oloc #f #f #f bind oloc-val)
;     (deal-with-single-int #f #f #f bind int-val)
;     (deal-with-single-addr #f #f #f bind addr-val)
    (deal-with-sym-len-or-pred #f #f benv store bind ae 'symbol-len)
    (deal-with-sym-len-or-pred #f #f benv store bind ae 'predicate)
    ))



(define (constr-property-formula govenor-base base-smt-var index-smt-var)
  (define conditions (set->list govenor-base))
  
  ;; currently let's do this:
  (define conditions2 (append conditions (list `(smt:assert (>=/s ,index-smt-var 0)))))
  
  `(smt:assert 
    ; validity
    (not 
     (=>/s
      (forall/s ((x Int))
                (and/s 
                 ,@conditions2))
                (inBound ,base-smt-var ,index-smt-var)))))
  
    
  
;; for pylist-set reference and pylist-ref 
(define (deal-with-list-ops-reference-and-verify ae-b ae-f benv exp govenor-base)
  
  (define base-bind (benv-lookup benv ae-b))
  (define field-bind (benv-lookup benv ae-f))
  (define smt-sym-info (addr-in-smt-sym-table? base-bind))
  (define index-sym-info (addr-in-smt-sym-table? field-bind))
  
  (cond
    [ (equal? smt-sym-info #f) 
      (displayln "base sym var not found in pylist-set!")
      (displayln base-bind)
      (void)
      ]
    [(equal? index-sym-info #f)
     (displayln "index sym var not found in pyliset-set! ")
     (void)]
    
    [(and (not (equal? smt-sym-info #f))
          (not (equal? index-sym-info #f)))
      (displayln "index sym var smt-smt info all found!!!")
      (displayln (format  "the index name is ~a" (sym-meta-sym-name index-sym-info)))
       
     (define sym-var (sym-meta-sym-name smt-sym-info))
     (define index-smt-var (sym-meta-sym-name index-sym-info))
     
     (cond
       [(set-empty? govenor-base) (print-proving-header exp 'sat)]
       [else
        (add-formula! (constr-property-formula govenor-base sym-var index-smt-var))
        (add-formula! `(smt:check-sat))
    ; (pretty-write smt-symbol-table)
   ;  (pretty-write partial-assumption-base)
        (define res  (fire-Z3 partial-assumption-base))
        (print-proving-header exp res)
        ( drop-last-two!)])
     ]
    [else 
     (displayln "ref or set problem ")
     (displayln exp)
     (void)])
  )

;;;;; for each abstrac type, we have generate the symbol table
(define (deal-with-single-oloc var-or-bind? var benv bind oloc-res)
  (cond
    [(not (equal? oloc-res #f)) 
     (define sym-res (addr-in-smt-sym-table? oloc-res))
     (cond
       [(not (equal? sym-res #f)) 
        (cond
          [var-or-bind? ; var = true
           (update-smt-symbol-table! (benv-lookup benv var) sym-res)]
          [else
           (update-smt-symbol-table! bind sym-res)
           ])]
       [else (void)])]
    [else (void)]))

(define (deal-with-single-int var-or-bind? var benv bind int-val)
  (cond
    [(not (equal? #f int-val))
     (define smt-var (gen-smt-var))
     (add-formula! `(smt:declare-fun ,smt-var  ()  Int))
    ; (add-formula! `(smt:assert (=/s ,smt-var ,int-val)))
     (cond
       [var-or-bind? ;var=true
        (update-smt-symbol-table! (benv-lookup benv var) (sym-meta smt-var 'Int))]
       [else
        (update-smt-symbol-table! bind (sym-meta smt-var 'Int))])
     ]
    [else (void)]))

(define (deal-with-single-addr var-or-bind? var benv bind addr-val)
  (cond
    [(not (equal? #f addr-val))
      (define sym-res (addr-in-smt-sym-table? addr-val))
      (cond
       [(not (equal? sym-res #f)) 
        (cond
          [var-or-bind? ; var = true
           (update-smt-symbol-table! (benv-lookup benv var) sym-res)]
          [else
           (update-smt-symbol-table! bind sym-res)
           ])]
       [else (void)])]
    [else (void)]))



(define (deal-with-single-abstract-type var-or-bind? var ae benv bind oloc-val int-val addr-val )
  (cond
    [(hash-has-key? benv ae)
     (displayln (format "deal-with-single-abstract-type : has has key of the ae ~a" ae)) 
      (define ae-bind (benv-lookup benv ae))
      (define ae-meta (addr-in-smt-sym-table? ae-bind))
      (cond 
        [(not (equal? ae-meta #f))
         (cond
          [var-or-bind? ; var = true
           (update-smt-symbol-table! (benv-lookup benv var) ae-meta)]
          [else
           (update-smt-symbol-table! bind ae-meta)
           ])])]
        
      [else
       (deal-with-single-int var-or-bind? var benv bind int-val) 
       (deal-with-single-addr var-or-bind? var benv bind addr-val)
       (deal-with-single-oloc var-or-bind? var benv bind oloc-val)]))


(define (get-len-sym len-arg benv)
  (cond
    [(hash-has-key? benv len-arg)
     (displayln (format "get-len-sym : has has key of the ae ~a" len-arg)) 
     (define lenarg-bind (benv-lookup benv len-arg))
     (define lenarg-meta (addr-in-smt-sym-table? lenarg-bind))
     (sym-meta-sym-name lenarg-meta)]
    [else
     (error (format "should not be possible that the len arg don't have binding in benv!~a" len-arg))]))
  

;; the symbol will dealth differently, 
;; if the value of a binding is symbol-len,
;; we will go to the symbol table to find its address
;; toget its meta formula.
;; then propafate the information to the new 
;; len-or-predi = symbol-len or predicate
(define (deal-with-sym-len-or-pred var-or-bind? var benv store bind ae  len-or-predi)
  (cond 

    [ (hash-has-key? benv ae)
      (displayln (format "deal-with-sym-len! : has has key of the ae ~a" ae)) 
      (define symset-val ((atom-eval benv store) ae))
      (define sym-res (singleton-type-set?  symset-val len-or-predi))
      (cond
        [(not (equal? sym-res #f))
         (define cur-ae-addr (benv-lookup benv ae))
         (define meta-res (addr-in-smt-sym-table? cur-ae-addr))
         (cond 
           [(not (equal? meta-res #f))
            (cond
              [var-or-bind? ; var = true
               (update-smt-symbol-table! (benv-lookup benv var) meta-res)]
              [else
               (update-smt-symbol-table! bind meta-res)
               ])]
           [else (void)])]
        [else (void)])]
     ;; if partially applied lambda, the ae will be location a
    [else
      (displayln (format "deal-with-sym-len! does not have the  ~a" ae)) 
     (void)]))
 
(define (smtop op)
   (string->symbol 
        (string-append (symbol->string op) "/s")))
;;
(define (smtop2 op e1 e2)
  (match op
    [`equal? `(=/s ,e1 ,e2)]
    [`not-equal? `(not (=/s ,e1 ,e2))]
    [else
     `(
       ,(string->symbol 
        (string-append (symbol->string op) "/s"))
       ,e1 ,e2)]))

;; generate small counter as pointer
(define gen-smt-var
  (let ([counter 0])
    (lambda ([x 'smt-var])
      (if (number? x)
          (set! counter x)
          (begin0 (string->unreadable-symbol
                   (format "~a~a" x counter))
                  (set! counter (add1 counter)))))))

;; only Addr or IntList
(struct sym-meta (sym-name type) #:prefab)

;; test whether it is list allocation
(define (number-keys? lst-keys)
  (cond
    [(empty? lst-keys) #f]
    [else 
     (define just-the-first (first lst-keys))
     (number? just-the-first)]))

(define (symIntLst->rktIntList sym-intLst)
  `(list ,@(map  identity sym-intLst)))

;; easy interface

;;; I think I will just make the assumption base the global at first
;;; rather than make it into the state component
;; to start an empty formula

(define partial-assumption-base (list))

; to append a formula
;; like the form (formula)
(define (add-formula! new-lst-elem)
  (set! partial-assumption-base (append partial-assumption-base (list new-lst-elem))))

;; after each assertion of the range checking, e.g.
; (smt:assert (>=/s smt-var18 (len smt-var2)))
; we will need to eliminate the formua 
; otherwise, there will be confusion of sat and unsat 
; for the reason of too many bound checking assertions
(define (drop-last-two! )
  (set! partial-assumption-base (drop-right partial-assumption-base 2)))


  ;(append base (list new-lst-elem)))
;; like the form (list (formula) (formua))
(define (add-formulas!  lst-forms)
  (set! partial-assumption-base (append partial-assumption-base lst-forms)))
  ;(append base lst-forms))

;; helpers to get all the z3 symbols for a given formula

;; helper to udpate some formula


  
  
;; detect the list pattern, aymaybe used
;; if yes, then the single argument will return
;; otherwise, return #f
(define (is-List-alloc-site? lst)
  (match lst
    [`(List ,arg) arg]
    [else #f]))

;;; the interface to fire Z3 with the accumulated formulas list passed at some point.
(define (fire-Z3 lst)
   (define base-lst
     `(smt:with-context
       (smt:new-context )
       (parameterize ((current-namespace (current-namespace)))
         (define len (make-length 10))
         (define append (make-append 10))
         ;;
         (smt:declare-fun LEN ((Array Int Int)) Int)
         (smt:define-fun inBound( (arr (Array Int Int)) (x Int)) Bool
                         (ite/s (and (>=/s x 0) (>=/s (LEN arr) x)) #t #f)
                         )
         ,@lst)))
  (log-proved-formula base-lst )
  (parameterize ((current-namespace (namespace-anchor->namespace top)))
     (eval  base-lst)))

(define (log-proved-formula lst )
   (define fn (last (regexp-split #rx"/" passed-in-fp)))
  (define fnp (string-append "sv-tests-part-res/" fn  (symbol->string (gensym)) ".z3rkt")) 
  (pretty-write lst)
  (display-lines-to-file (list lst) fnp #:mode 'text #:exists 'replace))
  

(define (print-proving-header exp res)
  (acc-lines! (list (format "\n")))
  (acc-lines! (list  (format "....proving the following assumption base in expression " )))
  (acc-lines! (list exp))
  ;(acc-lines! (list (format "~a" partial-assumption-base)))
  (acc-lines! (list ( format "*********** Theorem Prover is fired .......")))
  (cond
    [(equal? res 'unsat)
     (acc-lines! (list (format "Theorem prover says ~a:  VALID array list reference!" res)))
     ]
    [else
      (acc-lines! (list (format "Theorem prover says ~a: INVALID array list reference!" res)))])
  
  )

    
;; this is used to propagate the new smt-sym information to previous bindings.
;; the previous formulas still exits.
(define (newListSym-propogation! old-sym-meta new-sym-meta)
  (define entries (hash->list smt-symbol-table))
  (define entires-with-old-sym-meta (filter 
                                       (λ (en)
                                         (define sym-meta-info (cdr en))
                                         (cond
                                           [(equal? sym-meta-info old-sym-meta)
                                            (displayln (format "found old sym meta info! ~a" old-sym-meta))
                                            sym-meta-info]
                                           [else 
                                            (displayln (format "found old sym meta info! ~a" old-sym-meta))
                                            #f]))
                                        entries ))
  ;; propagte
  (for ([en entires-with-old-sym-meta])
    (define key (car en))
    (hash-set! smt-symbol-table key new-sym-meta))
  )
