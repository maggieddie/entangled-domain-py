#lang racket

(require rackunit)
(require "../../main.rkt"
         "../../examples/list-helpers.rkt")


(smt:with-context
    (smt:new-context #:mbqi? #t)
    ;; A0, A1, A2, A3, A4 are "arrays" from Integers to Integers.
    (smt:declare-fun A0 (Int) Int)
    (smt:declare-fun A1 (Int) Int)
    (smt:declare-fun A2 (Int) Int)
    (smt:declare-fun A3 (Int) Int)
    (smt:declare-fun A4 (Int) Int)
    (smt:declare-fun n () Int)
    (smt:declare-fun l () Int)
    (smt:declare-fun k () Int)
    (smt:declare-fun x () Int)
    (smt:declare-fun y () Int)
    (smt:declare-fun w () Int)
    (smt:declare-fun z () Int)
    (smt:declare-fun LEN ((Array Int Int)) Int)
    (smt:define-fun inBound( (arr (Array Int Int)) (x Int)) Bool
                    (ite/s (and (>=/s x 0) (>=/s (LEN arr) x)) #t #f)
                    )
    (smt:declare-fun smt-var0 () (Array Int Int))
    (smt:declare-fun smt-var1 () (Array Int Int))
    (smt:declare-fun smt-var2 () Int)
    (smt:declare-fun smt-var3 () Int)
    (smt:assert
     (not
      (=>/s
       (forall/s
        ((x Int))
        (and/s #f (smt:assert (>=/s smt-var3 0))))
        (inBound smt-var1 smt-var3))))
    (smt:check-sat))
    
;    ;; A1 = A0[k <- w]
;    (smt:assert (=/s (A1 k) w))
;    (smt:assert (forall/s ((x Int)) (or/s (=/s x k) (=/s (A1 x) (A0 x)))))
;
;    ;; A2 = A1[l <- x] = A0[k <- w][l <- x]
;    (smt:assert (=/s (A2 l) x))
;    (smt:assert (forall/s ((x Int)) (or/s (=/s x l) (=/s (A2 x) (A1 x)))))
;
;    ;; A3 = A0[k <- y]
;    (smt:assert (=/s (A3 k) y))
;    (smt:assert (forall/s ((x Int)) (or/s (=/s x k) (=/s (A3 x) (A0 x)))))
;
;    ;; A4 = A3[l <- z] = A0[k <- y][l <- z] 
;    (smt:assert (=/s (A3 l) z))
;    (smt:assert (forall/s ((x Int)) (or/s (=/s x l) (=/s (A4 x) (A3 x)))))
;
;    (smt:assert (and/s (</s w x) (</s x y) (</s y z)))
;    (smt:assert (and/s (</s 0 k) (</s k l) (</s l n)))
;    (smt:assert (>/s (-/s l k) 1))
;
;    ;; A2 is sorted in the interval [0,n-1]
;    ; here is teh imply
;    ;; 
;    (smt:assert (forall/s ((i Int) (j Int))
;                          (=>/s (and/s (<=/s 0 i) (<=/s i j) (<=/s j (-/s n 1)))
;                                (<=/s (A2 i) (A2 j)))))
;    (check-eq? (smt:check-sat) 'sat)
;
;    ;; A4 is sorted in the interval [0,n-1]
;    (smt:assert (forall/s ((i Int) (j Int))
;                          (=>/s (and/s (<=/s 0 i) (<=/s i j) (<=/s j (-/s n 1)))
;                                (<=/s (A4 i) (A4 j)))))
;    (check-eq? (smt:check-sat) 'unsat))