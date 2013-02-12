(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$b (void))
 (define g$__name__ (void))
 (define g$b.x (void))
 (define g$y (void))
 (define g$z (void))
 (set-then!
  g$__name__
  "__main__"
  (set-then!
   g$a
   g$o
   (set-then!
    g$b
    g$o
    (set-then!
     g$b.x
     (py-list* 1 3 "hi")
     (set-then!
      g$y
      g$o.x
      ((lambda (e15 k14)
         ((lambda (i14 k15)
            ((cps py-list?)
             e15
             (lambda (rv16)
               (if rv16
                 ((cps py-list-ref) e15 i14 k15)
                 ((cps tuple?)
                  e15
                  (lambda (rv17)
                    (if rv17
                      ((cps tuple-ref) e15 i14 k15)
                      ((cps dict?)
                       e15
                       (lambda (rv18)
                         (if rv18
                           ((cps dict-ref) e15 i14 k15)
                           (error "cannot index object" k15)))))))))))
          2
          k14))
       g$y
       (lambda (rv19) (set-then! g$z rv19 ($halt (void)))))))))))

