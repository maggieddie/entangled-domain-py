(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$foo (void))
 (set-then!
  g$foo
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16) (return (dict ("a" "hi") ("b" 2) ("c" 3)) k16)) k15))
     k14))
  (g$foo
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
         "a"
         k17))
      rv22
      (lambda (rv23) (set-then! g$b rv23 ($halt (void)))))))))

