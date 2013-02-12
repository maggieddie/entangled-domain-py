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
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16) (return (dict ("a" 1) ("b" "horray!") ("c" 3)) k16))
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
    (lambda (rv31)
      ((lambda (e15 k26)
         ((lambda (i14 k27)
            ((cps py-list?)
             e15
             (lambda (rv28)
               (if rv28
                 ((cps py-list-ref) e15 i14 k27)
                 ((cps tuple?)
                  e15
                  (lambda (rv29)
                    (if rv29
                      ((cps tuple-ref) e15 i14 k27)
                      ((cps dict?)
                       e15
                       (lambda (rv30)
                         (if rv30
                           ((cps dict-ref) e15 i14 k27)
                           (error "cannot index object" k27)))))))))))
          0
          k26))
       rv31
       (lambda (rv32)
         ((lambda (e17 k21)
            ((lambda (i16 k22)
               ((cps py-list?)
                e17
                (lambda (rv23)
                  (if rv23
                    ((cps py-list-ref) e17 i16 k22)
                    ((cps tuple?)
                     e17
                     (lambda (rv24)
                       (if rv24
                         ((cps tuple-ref) e17 i16 k22)
                         ((cps dict?)
                          e17
                          (lambda (rv25)
                            (if rv25
                              ((cps dict-ref) e17 i16 k22)
                              (error "cannot index object" k22)))))))))))
             "b"
             k21))
          rv32
          (lambda (rv33) (set-then! g$a rv33 ($halt (void))))))))))))

