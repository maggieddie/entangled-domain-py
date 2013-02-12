#lang racket

(require rackunit)
(require "../../main.rkt"
         "../../examples/list-helpers.rkt")



; failed case
;(smt:with-context
; (smt:new-context)
; (smt:declare-fun f ((x!1 Int) (x!2 Bool)) Int
;                  (ite (and (= x!1 11) (= x!2 false)) 21 0))
; )
 

;(smt:with-context
 ;   (smt:new-context #:print-success #t)
   
;(smt:with-context
;    (smt:new-context #:macro-finder #t )
;    (smt:define-fun maxmy ((a Int) (b Int)) Int 
;                     (ite/s (>=/s a b) a b))
;   ; (smt:assert (=/s (maxmy 4 7) 7)) 
;    
;    (smt:declare-fun x () Int)
;    (smt:declare-fun y () Int)
;    (smt:assert (maxmy x x))
;    (smt:check-sat)
;    ;(smt:get-model)
;    
;    ;(smt:eval p)
;    )

;; the above example just crushes.

;;;
;(smt:with-context
;    (smt:new-context #:macro-finder #t)
;    (smt:define-fun greatThan10 ((a Int))  Bool 
;                     (>/s a 10))
; 
;    
;    (smt:declare-fun x () Int)
;    (smt:declare-fun y () Int)
;    (smt:assert (greatThan10 x))
;    (smt:assert (greatThan10 y))
;    (smt:check-sat)
;    (smt:get-model)
;    
;    ;(smt:eval p)
;    )

;; The above example passed

;

;
;(smt:with-context
;    (smt:new-context #:macro-finder #t )
;    (smt:define-fun absolute ((x Int)) Int 
;                     (ite/s (>=/s x 0) x (-/s x)))
;    (smt:declare-fun a () (Array Int Int))
;    (smt:assert (=/s (+/s
;                    (select/s a 0)
;                   (select/s a 1)
;                   (select/s a 2)
;                   (select/s a 3)
;                   (select/s a 4)
;                   
;                   (select/s a 5)
;                   (select/s a 6)
;                   (select/s a 7)
;                   (select/s a 8)
;                   
;                   (select/s a 8)
;                   (select/s a 9)
;                   (select/s a 10)
;                   (select/s a 11)
;                   (select/s a 12)
;                   (select/s a 13))
;                   (- 0 13)))
;    (smt:check-sat)
;    )

; the above example can work nicely.

;(smt:with-context
;    (smt:new-context)
;    (smt:declare-datatypes () ((Addr addr)))
;    (smt:declare-fun equality-inequality (Addr Addr) Bool)
;    ;; the equality relation
;    (smt:assert (forall/s ((a Addr) (b Addr) (c Addr))
;                        (=>/s (and (equality-inequality a b) (equality-inequality b c))
;                              (equality-inequality a c))))
;    
;    ;; the inequality relation
;    (smt:assert (forall/s ((a Addr) (b Addr) (c Addr))
;                          (=>/s (and (equality a b) (equality b c))
;                                (equality a c))))
;    
;    (smt:declare-fun a1 () Addr)
;    (smt:declare-fun a2 () Addr)
;    (smt:declare-fun a3 () Addr)
;    (smt:assert (=/s (equality-inequality a1 a2) #t))
;    (smt:assert (=/s (equality-inequality a2 a3) #t))
;    (smt:assert (equality-inequality a1 a3))
;    (smt:check-sat))

  ;(define ns (make-base-namespace))
  ;(namespace-set-variable-value! 'smt:declare-fun smt:declare-fun #f ns)
;  (namespace-set-variable-value! 'make-length make-length #f ns)
;  ;(namespace-set-variable-value! 'Int Int #f ns)
;  (namespace-set-variable-value! 'smt:assert smt:assert #f ns)
;  
;  (namespace-set-variable-value! 'nil/s  nil/s #f ns)
;  (namespace-set-variable-value! '=/s  =/s #f ns)
;  
;  (namespace-set-variable-value! 'list->z3-list  list->z3-list #f ns)
;  (namespace-set-variable-value! 'smt:check-sat  smt:check-sat #f ns)
;  (namespace-set-variable-value! 'smt:eval  smt:eval #f ns)
;   (namespace-set-variable-value! 'smt:new-context  smt:new-context #f ns)
   ;(namespace-set-variable-value! 'smt:with-context   smt:with-context #f ns)
   (displayln "a")
  
     (displayln "b")
   
  (define lst2 
  `(smt:with-context
    (smt:new-context)
    (define len (make-length 10))
    (smt:declare-fun v1 () Int)
    (smt:assert (=/s v1 (len nil/s)))
    (smt:declare-fun v2 () Int)
    (smt:assert (=/s v2 (len (list->z3-list '(42 31 24 19)))))
    (smt:declare-fun v3 () Int)
    (smt:assert (=/s v3 (len (list->z3-list '(21 19 18 14 10 9 8 7 6 5)))))
    (displayln "cx")
    (check-eq? (smt:check-sat) 'sat)
    (check-equal? (smt:eval v1) 0)
    (check-equal? (smt:eval v2) 4)
    (check-equal? (smt:eval v3) 10)))
  
;  (eval lst2 ns)
  
(define lst1
  `(smt:with-context
    (smt:new-context )
    (smt:declare-datatypes () ((Addr addr)))
    (smt:declare-fun equality-inequality (Addr Addr) Bool)
    ;; the equality relation
    (smt:assert (forall/s ((a Addr) (b Addr) (c Addr))
                        (=>/s (and (equality-inequality a b) (equality-inequality b c))
                              (equality-inequality a c))))

    ;; a > 1, c>a => c>1 Or a = 1, c=a => c=1
    (smt:declare-fun abs-conc-relation (Addr Int) Bool)
    (smt:assert (forall/s ((a Addr) (i Int) (c Addr))
                          (=>/s (and (abs-conc-relation a i) 
                                     (equality-inequality c a))
                                (abs-conc-relation c i))))

    ;; test equality-inequality
    (smt:declare-fun a1 () Addr)
    (smt:declare-fun a2 () Addr)
    (smt:declare-fun a3 () Addr) 
    (smt:assert (=/s (equality-inequality a1 a2) #t))
    (smt:assert (=/s (equality-inequality a2 a3) #t))
    (smt:assert (not (equality-inequality a1 a3)))
    (smt:check-sat)
    ; test abs-conc-relation
    (smt:declare-fun i1 () Int)
    (smt:assert (=/s (abs-conc-relation a1 i1) #t))
    (smt:assert (=/s (equality-inequality a2 a1) #t))
    (smt:assert (not (abs-conc-relation a2 i1)))
    ;(smt:check-sat)
    )

)

;(define lst22 
  (smt:with-context
   (smt:new-context )
  ; (parameterize ((current-namespace (current-namespace)))
    (define len (make-length 10))
    (smt:declare-fun v1 () Int)
    (smt:assert (=/s v1 (len nil/s)))
    (smt:declare-fun v2 () Int)
    (smt:assert (=/s v2 (len (list->z3-list '(42 31 24 19)))))
      (smt:declare-fun v4 () Int)
    (smt:assert (=/s v4 (+/s 3 1)))
    (smt:declare-fun v3 () Int)
    (smt:assert (=/s v3 (len (list->z3-list '(21 19 18 14 10 9 8 7 6 5)))))
     ;   (smt:declare-fun v4 () Int)
    ;(smt:assert (=/s v4 (+/s 10 1)))
    (check-eq? (smt:check-sat) 'sat)
    (check-equal? (smt:eval v2) 4)
    ;(check-equal? (smt:eval v3) 10))
  ;)
)
;(eval lst22 ns)
;(define lst12 (append lst1 lst22))

;(eval lst12 ns)


 (smt:with-context
  (smt:new-context)
   (define len (make-length 10))
  (smt:declare-fun l2 () Int)
  (smt:assert (=/s l2 (len nil/s))) 
    ;(check-eq? (smt:check-sat) 'sat)
  (smt:check-sat)
  (check-equal?  (smt:eval l2) 0))

(smt:with-context
  (smt:new-context); #:macro-finder #t )

 
 (smt:declare-fun l11 () IntList)
 (smt:declare-fun l22 () IntList)
 (smt:declare-fun l33 () IntList) 
 (smt:declare-fun l44 () IntList) 
 
 (smt:declare-fun index () Int)
 (smt:assert (=/s index 2))
 (smt:declare-fun v2 () IntList)
 
  (smt:assert (=/s v2  (list->z3-list '(1 2 3 ))))
 (smt:declare-fun v3 () IntList)

 (smt:assert (=/s v2 v3))
  (smt:declare-fun v4 () IntList)

(smt:assert (=/s v4 (list->z3-list '(1 2 3))))
 
  (define len (make-length 10))
  (smt:declare-fun l2 () Int)
  (smt:assert (=/s l2 (len v2))) 
  
    (smt:declare-fun l3 () Int)
    ( smt:assert (=/s l3 (len v3))) 
        (smt:declare-fun l4 () Int)
    ( smt:assert (=/s l3 (len v4))) 
   (smt:assert (=/s  l2 l4))
    (smt:check-sat)
   
   (smt:assert (</s (smt:eval l3) index))
   (smt:check-sat)
; (check-equal?  (smt:eval l3) 3)

  

; (smt:check-sat)
 )


; (define (gen-formulas lst-defs) 
;  (smt:with-context
;    (smt:new-context)
;    (define len (make-length 10))
;    @lst-defs
;    ))
; 
; (define lst-defs 
;   `(
;     (smt:declare-fun v1 () Int)
;     (smt:assert (=/s v1 (len nil/s)))
;     (check-eq? (smt:check-sat) 'sat)
;    (check-equal? (smt:eval v1) 0)))
    


  (smt:with-context
   (smt:new-context )
    (smt:declare-fun v1 () Int)
    (smt:declare-fun v2 () Int)
    (smt:declare-fun v3 () Int)
    (smt:assert (=/s v1 5))
    (smt:assert (=/s v2 v1))
    (smt:assert (=/s v2 5))
    (smt:check-sat))
  
   (test-case
   "Test length"
   (smt:with-context
    (smt:new-context)
    (define len (make-length 10))
    (smt:declare-fun v1 () Int)
    (smt:assert (=/s v1 (len nil/s)))
    (smt:declare-fun v2 () Int)
    (smt:assert (=/s v2 (len (list->z3-list '(42 31 24 19)))))
    (smt:declare-fun v3 () Int)
    (smt:assert (=/s v3 (len (list->z3-list '(21 19 18 14 10 9 8 7 6 5)))))
     (smt:declare-fun v10 () IntList)
     (smt:assert (=/s v10 (list->z3-list '(42 31 24 19))))
    (smt:assert (not (=/s (len v10) 4)))
    (smt:check-sat)))
  ;  (check-eq? (smt:check-sat) 'sat)
   ;(check-equal? (smt:eval v1) 0)
    ;(check-equal? (smt:eval v2) 4)
    ;(check-equal? (smt:eval v3) 10)))
  
(smt:with-context (smt:new-context) (parameterize ((current-namespace (current-namespace))) 
                                      (define len (make-length 10)) 
                                      (define append (make-append 10)) 
                                      (smt:declare-fun LEN ((Array Int Int)) Int) 
                                      (smt:define-fun inBound ((arr (Array Int Int)) (x Int)) Bool (ite/s (and (>=/s x 0) (>=/s (LEN arr) x)) #t #f)) 
                                      (smt:declare-fun smt-var0 () (Array Int Int)) 
                                      (smt:declare-fun smt-var1 () (Array Int Int)) 
                                      (smt:declare-fun smt-var2 () Int) (smt:declare-fun smt-var3 () Int) 
                                      (smt:assert (not (=>/s 
                                                        (forall/s ((x Int)) (and/s  (</s smt-var3 x) (smt:assert (>=/s x 0)))) 
                                                        (inBound smt-var1 smt-var3)))) (smt:check-sat)))

    





