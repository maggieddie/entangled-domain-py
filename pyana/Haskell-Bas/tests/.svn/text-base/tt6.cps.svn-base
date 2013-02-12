(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$a (void))
 (define g$b (void))
 (define g$g (void))
 (define g$c (void))
 (set-then!
  g$f
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps py-print) "called f" (lambda (rv17) (return 1 k16))))
        k15))
     k14))
  (set-then!
   g$g
   (lambda (k18)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k19)
        ((lambda (k20)
           ((cps py-print) "called g" (lambda (rv21) (return 0 k20))))
         k19))
      k18))
   (set-then!
    g$a
    (py-list* (py-list* 10 20) (py-list* 30 40) (py-list* 50 60))
    ((lambda (e15 k28)
       (g$f
        (lambda (rv33)
          ((lambda (i14 k29)
             ((cps py-list?)
              e15
              (lambda (rv30)
                (if rv30
                  ((cps py-list-ref) e15 i14 k29)
                  ((cps tuple?)
                   e15
                   (lambda (rv31)
                     (if rv31
                       ((cps tuple-ref) e15 i14 k29)
                       ((cps dict?)
                        e15
                        (lambda (rv32)
                          (if rv32
                            ((cps dict-ref) e15 i14 k29)
                            (error "cannot index object" k29)))))))))))
           rv33
           k28))))
     g$a
     (lambda (rv34)
       ((lambda (e17 k22)
          (g$g
           (lambda (rv27)
             ((lambda (i16 k23)
                ((cps py-list?)
                 e17
                 (lambda (rv24)
                   (if rv24
                     ((cps py-list-ref) e17 i16 k23)
                     ((cps tuple?)
                      e17
                      (lambda (rv25)
                        (if rv25
                          ((cps tuple-ref) e17 i16 k23)
                          ((cps dict?)
                           e17
                           (lambda (rv26)
                             (if rv26
                               ((cps dict-ref) e17 i16 k23)
                               (error "cannot index object" k23)))))))))))
              rv27
              k22))))
        rv34
        (lambda (rv35)
          (set-then!
           g$b
           rv35
           ((lambda (e19 k42)
              (g$g
               (lambda (rv47)
                 ((lambda (i18 k43)
                    ((cps py-list?)
                     e19
                     (lambda (rv44)
                       (if rv44
                         ((cps py-list-ref) e19 i18 k43)
                         ((cps tuple?)
                          e19
                          (lambda (rv45)
                            (if rv45
                              ((cps tuple-ref) e19 i18 k43)
                              ((cps dict?)
                               e19
                               (lambda (rv46)
                                 (if rv46
                                   ((cps dict-ref) e19 i18 k43)
                                   (error "cannot index object" k43)))))))))))
                  rv47
                  k42))))
            g$a
            (lambda (rv48)
              ((lambda (e21 k36)
                 (g$f
                  (lambda (rv41)
                    ((lambda (i20 k37)
                       ((cps py-list?)
                        e21
                        (lambda (rv38)
                          (if rv38
                            ((cps py-list-ref) e21 i20 k37)
                            ((cps tuple?)
                             e21
                             (lambda (rv39)
                               (if rv39
                                 ((cps tuple-ref) e21 i20 k37)
                                 ((cps dict?)
                                  e21
                                  (lambda (rv40)
                                    (if rv40
                                      ((cps dict-ref) e21 i20 k37)
                                      (error
                                       "cannot index object"
                                       k37)))))))))))
                     rv41
                     k36))))
               rv48
               (lambda (rv49)
                 ((cps +)
                  g$b
                  rv49
                  (lambda (rv50)
                    (set-then! g$c rv50 ($halt (void))))))))))))))))))
