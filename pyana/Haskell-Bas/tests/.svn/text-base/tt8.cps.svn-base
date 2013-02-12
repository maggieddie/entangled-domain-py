(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$foo (void))
 (define g$bar (void))
 (set-then!
  g$foo
  (lambda (a b k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15) ((lambda (k16) (return (tuple a b) k16)) k15))
     k14))
  (set-then!
   g$bar
   (lambda (k17)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k18)
        ((lambda (k19)
           (g$foo
            "string"
            6
            (lambda (rv25)
              ((lambda (e15 k20)
                 ((lambda (i14 k21)
                    ((cps py-list?)
                     e15
                     (lambda (rv22)
                       (if rv22
                         ((cps py-list-ref) e15 i14 k21)
                         ((cps tuple?)
                          e15
                          (lambda (rv23)
                            (if rv23
                              ((cps tuple-ref) e15 i14 k21)
                              ((cps dict?)
                               e15
                               (lambda (rv24)
                                 (if rv24
                                   ((cps dict-ref) e15 i14 k21)
                                   (error "cannot index object" k21)))))))))))
                  0
                  k20))
               rv25
               (lambda (rv26) (return rv26 k19))))))
         k18))
      k17))
   (g$bar (lambda (rv27) (set-then! g$a rv27 ($halt (void))))))))

