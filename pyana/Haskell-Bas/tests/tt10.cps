(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$bar3 (void))
 (define g$new_elem (void))
 (define g$changedlst (void))
 (define g$bar1 (void))
 (define g$bar2 (void))
 (set-then!
  g$bar1
  (lambda (a b k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (c k16)
          ((cps +)
           a
           b
           (lambda (rv17) (set-then! c rv17 (return (py-list* a b c) k16)))))
        (void)
        k15))
     k14))
  (set-then!
   g$bar2
   (lambda (lst newvalue index k18)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k19)
        ((lambda (k20)
           ((lambda (b15 k21)
              ((lambda (i14 k22)
                 ((cps tuple?)
                  b15
                  (lambda (rv23)
                    (if rv23
                      ((cps tuple-set!) b15 i14 newvalue k22)
                      ((cps py-list?)
                       b15
                       (lambda (rv24)
                         (if rv24
                           ((cps py-list-set!) b15 i14 newvalue k22)
                           ((cps dict?)
                            b15
                            (lambda (rv25)
                              (if rv25
                                ((cps dict-set!) b15 i14 newvalue k22)
                                (k22 (void))))))))))))
               index
               k21))
            lst
            (lambda (rv26) (return lst k20))))
         k19))
      k18))
   (set-then!
    g$bar3
    (lambda (k27)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k28)
         ((lambda (lst k29)
            (g$bar1
             1
             2
             (lambda (rv30)
               (g$bar2
                rv30
                "newvalue"
                0
                (lambda (rv31) (set-then! lst rv31 (return lst k29)))))))
          (void)
          k28))
       k27))
    (g$bar3
     (lambda (rv32)
       (set-then!
        g$changedlst
        rv32
        ((lambda (e17 k33)
           ((lambda (i16 k34)
              ((cps py-list?)
               e17
               (lambda (rv35)
                 (if rv35
                   ((cps py-list-ref) e17 i16 k34)
                   ((cps tuple?)
                    e17
                    (lambda (rv36)
                      (if rv36
                        ((cps tuple-ref) e17 i16 k34)
                        ((cps dict?)
                         e17
                         (lambda (rv37)
                           (if rv37
                             ((cps dict-ref) e17 i16 k34)
                             (error "cannot index object" k34)))))))))))
            0
            k33))
         g$changedlst
         (lambda (rv38) (set-then! g$new_elem rv38 ($halt (void))))))))))))

