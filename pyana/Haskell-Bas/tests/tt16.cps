(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$__name__ (void))
 (define g$x (void))
 (define g$y (void))
 (define g$z (void))
 (set-then!
  g$__name__
  "__main__"
  (set-then!
   g$f
   (lambda (a b k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15) ((lambda (k16) (return (py-list* a b) k16)) k15))
      k14))
   (g$f
    1
    "hi"
    (lambda (rv22)
      ((lambda (e15 k17)
         ((lambda (i14 k18)
            ((cps py-list?)
             e15
             (lambda (rv19)
               (if rv19
                 ((cps py-list-ref) e15 i14 k18)
                 ((cps tuple?)
                  e15
                  (lambda (rv20)
                    (if rv20
                      ((cps tuple-ref) e15 i14 k18)
                      ((cps dict?)
                       e15
                       (lambda (rv21)
                         (if rv21
                           ((cps dict-ref) e15 i14 k18)
                           (error "cannot index object" k18)))))))))))
          0
          k17))
       rv22
       (lambda (rv23)
         (set-then!
          g$x
          rv23
          (g$f
           "hi"
           #f
           (lambda (rv29)
             ((lambda (e17 k24)
                ((lambda (i16 k25)
                   ((cps py-list?)
                    e17
                    (lambda (rv26)
                      (if rv26
                        ((cps py-list-ref) e17 i16 k25)
                        ((cps tuple?)
                         e17
                         (lambda (rv27)
                           (if rv27
                             ((cps tuple-ref) e17 i16 k25)
                             ((cps dict?)
                              e17
                              (lambda (rv28)
                                (if rv28
                                  ((cps dict-ref) e17 i16 k25)
                                  (error "cannot index object" k25)))))))))))
                 0
                 k24))
              rv29
              (lambda (rv30)
                (set-then!
                 g$y
                 rv30
                 (g$f
                  #f
                  2
                  (lambda (rv36)
                    ((lambda (e19 k31)
                       ((lambda (i18 k32)
                          ((cps py-list?)
                           e19
                           (lambda (rv33)
                             (if rv33
                               ((cps py-list-ref) e19 i18 k32)
                               ((cps tuple?)
                                e19
                                (lambda (rv34)
                                  (if rv34
                                    ((cps tuple-ref) e19 i18 k32)
                                    ((cps dict?)
                                     e19
                                     (lambda (rv35)
                                       (if rv35
                                         ((cps dict-ref) e19 i18 k32)
                                         (error
                                          "cannot index object"
                                          k32)))))))))))
                        0
                        k31))
                     rv36
                     (lambda (rv37)
                       (set-then! g$z rv37 ($halt (void))))))))))))))))))))

