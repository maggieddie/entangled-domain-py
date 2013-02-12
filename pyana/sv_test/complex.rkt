#lang racket
(define (iter index lst func)
   (if (= (length lst) 0)
          (list)
          (begin
                   (func index (first lst))
                   (iter (+ index 1) (cdr lst) func))))

(define (client vec1 mask-list)
   (define (g j y)
        (vector-set!  vec1 j (and y (list-ref mask-list j))))
   (if (equal? (vector-length vec1) (length mask-list))
          (begin
                   (iter 0 mask-list g)
                   vec1)
          (list)))
