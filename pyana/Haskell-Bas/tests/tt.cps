(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$b (void))
 (define g$__name__ (void))
 (define g$y (void))
 (set-then!
  g$__name__
  "__main__"
  (set-then!
   g$a
   (py-list* 0 1 2 3)
   ((lambda (b15 k14)
      ((lambda (i14 k15)
         ((cps tuple?)
          b15
          (lambda (rv16)
            (if rv16
              ((cps tuple-set!) b15 i14 (py-list* "A" "B" "C") k15)
              ((cps py-list?)
               b15
               (lambda (rv17)
                 (if rv17
                   ((cps py-list-set!) b15 i14 (py-list* "A" "B" "C") k15)
                   ((cps dict?)
                    b15
                    (lambda (rv18)
                      (if rv18
                        ((cps dict-set!) b15 i14 (py-list* "A" "B" "C") k15)
                        (k15 (void))))))))))))
       1
       k14))
    g$a
    (lambda (rv19)
      ((lambda (e17 k20)
         ((lambda (i16 k21)
            ((cps py-list?)
             e17
             (lambda (rv22)
               (if rv22
                 ((cps py-list-ref) e17 i16 k21)
                 ((cps tuple?)
                  e17
                  (lambda (rv23)
                    (if rv23
                      ((cps tuple-ref) e17 i16 k21)
                      ((cps dict?)
                       e17
                       (lambda (rv24)
                         (if rv24
                           ((cps dict-ref) e17 i16 k21)
                           (error "cannot index object" k21)))))))))))
          1
          k20))
       g$a
       (lambda (rv25)
         (set-then!
          g$y
          rv25
          ((lambda (e19 k26)
             ((lambda (i18 k27)
                ((cps py-list?)
                 e19
                 (lambda (rv28)
                   (if rv28
                     ((cps py-list-ref) e19 i18 k27)
                     ((cps tuple?)
                      e19
                      (lambda (rv29)
                        (if rv29
                          ((cps tuple-ref) e19 i18 k27)
                          ((cps dict?)
                           e19
                           (lambda (rv30)
                             (if rv30
                               ((cps dict-ref) e19 i18 k27)
                               (error "cannot index object" k27)))))))))))
              1
              k26))
           g$y
           (lambda (rv31) (set-then! g$b rv31 ($halt (void)))))))))))))

