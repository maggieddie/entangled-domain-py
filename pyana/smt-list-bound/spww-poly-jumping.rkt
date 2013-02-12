#lang racket

;; this file is modified point-widening from Phil and Steve's version
;; also add jumping

;(require "state-space.rkt")
(require "utils_cesk3_smt.rkt")
(provide point-wise-widen?
         point-wise-widen2?
         point-wise-widen
        ; point-wise-jump
         widen-store
         display-widening-stats
         get-vc
         reset-visit-count!
         reset-widening-table!
         )

(define widening-table (make-hash))
(define (reset-widening-table!)
  (set! widening-table (make-hash)))

(define cutoff 10)
(define visit-count (make-hash))
(define (reset-visit-count!) (set! visit-count (make-hash)))
(define (get-vc) visit-count)

;; update the current vc, if not exsits, then the value is 0.
;; next time if not widening, it will keep increasing.
(define (monitor-stmt st)
  (hash-update! visit-count st 
               (λ (d)
                 (add1 d))
               0))
               

(define (point-wise-widen? st)
  (cond
    [(empty? (state-context st)) 
     (DEBUG "empty list no widening")
     #f]
    [else
     (define current-statement (state-context st))
     (monitor-stmt current-statement)
     
     (define visited (hash-ref visit-count current-statement 0))
     (visited . >= . cutoff)]
    ))

(define (point-wise-widen2? st)
  (cond
    [(empty? (state-context st)) 
     (DEBUG "empty list!")
     #f]
    [else
     (define current-statement (car (state-context st)))
     (monitor-stmt current-statement)
     
     (define visited (hash-ref visit-count current-statement 0))
     (visited . >= . (* 2 cutoff))]
    ))
       
                   

;; return a state
(define (point-wise-widen st)
  
  (let*
      ([instr (state-context st)]
       [curr-σ (state-store st)]
       [point-σ (if (hash-has-key? widening-table instr)
                    (hash-ref widening-table instr)
                    (make-immutable-hash))]
       [widened-store (widen-store curr-σ point-σ)])
    (begin
      (hash-set! widening-table instr widened-store)
      (struct-copy state st [store widened-store]))))

;(define (point-wise-jump st)
;  (define rest-stmts  (state-context st))
;  (struct-copy state st [context rest-stmts]))


(define (display-widening-stats)
  (define widened
    (for/fold ([widened 0])
      ([(label count) visit-count])
      (if (count . >= . cutoff) (add1 widened) widened)))
  (displayln (format "~a total statements with ~a widened" (hash-count visit-count) widened)))


