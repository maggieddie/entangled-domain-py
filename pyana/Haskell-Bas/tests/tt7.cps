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
     (lambda (return k15) ((lambda (k16) (return 2 k16)) k15))
     k14))
  (set-then!
   g$bar
   (lambda (k17)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k18)
        ((lambda (k19) (g$foo (lambda (rv20) (return rv20 k19)))) k18))
      k17))
   (g$bar (lambda (rv21) (set-then! g$a rv21 ($halt (void))))))))

