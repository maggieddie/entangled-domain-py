(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$c (void))
 (set-then!
  g$f
  (lambda (a b k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15) ((lambda (k16) (return (tuple a b) k16)) k15))
     k14))
  (g$f 1 "hello" (lambda (rv17) (set-then! g$c rv17 ($halt (void)))))))
