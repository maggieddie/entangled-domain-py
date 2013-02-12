(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$bar (void))
 (define g$b2 (void))
 (define g$b1 (void))
 (define g$foo (void))
 (set-then!
  g$foo
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16) (return (dict ("a" 1) ("b" "special") ("c" 3)) k16))
        k15))
     k14))
  (set-then!
   g$bar
   (lambda (k17)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k18)
        ((lambda (k19) (g$foo (lambda (rv20) (return (tuple rv20 1) k19))))
         k18))
      k17))
   (g$bar
    (lambda (rv21)
      (set-then!
       g$b
       rv21
       ((lambda (e15 k22)
          ((lambda (i14 k23)
             ((cps py-list?)
              e15
              (lambda (rv24)
                (if rv24
                  ((cps py-list-ref) e15 i14 k23)
                  ((cps tuple?)
                   e15
                   (lambda (rv25)
                     (if rv25
                       ((cps tuple-ref) e15 i14 k23)
                       ((cps dict?)
                        e15
                        (lambda (rv26)
                          (if rv26
                            ((cps dict-ref) e15 i14 k23)
                            (error "cannot index object" k23)))))))))))
           1
           k22))
        g$b
        (lambda (rv27)
          (set-then!
           g$b1
           rv27
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
            g$b
            (lambda (rv38)
              ((lambda (e19 k28)
                 ((lambda (i18 k29)
                    ((cps py-list?)
                     e19
                     (lambda (rv30)
                       (if rv30
                         ((cps py-list-ref) e19 i18 k29)
                         ((cps tuple?)
                          e19
                          (lambda (rv31)
                            (if rv31
                              ((cps tuple-ref) e19 i18 k29)
                              ((cps dict?)
                               e19
                               (lambda (rv32)
                                 (if rv32
                                   ((cps dict-ref) e19 i18 k29)
                                   (error "cannot index object" k29)))))))))))
                  "b"
                  k28))
               rv38
               (lambda (rv39) (set-then! g$b2 rv39 ($halt (void))))))))))))))))

