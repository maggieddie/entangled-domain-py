(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 ((lambda (t14 k14)
    ((lambda (e16 k15)
       ((lambda (i15 k16)
          ((cps py-list?)
           e16
           (lambda (rv17)
             (if rv17
               ((cps py-list-ref) e16 i15 k16)
               ((cps tuple?)
                e16
                (lambda (rv18)
                  (if rv18
                    ((cps tuple-ref) e16 i15 k16)
                    ((cps dict?)
                     e16
                     (lambda (rv19)
                       (if rv19
                         ((cps dict-ref) e16 i15 k16)
                         (error "cannot index object" k16)))))))))))
        0
        k15))
     t14
     (lambda (rv20)
       (set-then!
        g$x
        rv20
        ((lambda (e18 k21)
           ((lambda (i17 k22)
              ((cps py-list?)
               e18
               (lambda (rv23)
                 (if rv23
                   ((cps py-list-ref) e18 i17 k22)
                   ((cps tuple?)
                    e18
                    (lambda (rv24)
                      (if rv24
                        ((cps tuple-ref) e18 i17 k22)
                        ((cps dict?)
                         e18
                         (lambda (rv25)
                           (if rv25
                             ((cps dict-ref) e18 i17 k22)
                             (error "cannot index object" k22)))))))))))
            1
            k21))
         t14
         (lambda (rv26) (set-then! g$y rv26 (k14 (void)))))))))
  (tuple "string" 3)
  $halt))
