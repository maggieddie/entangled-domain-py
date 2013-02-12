(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$d (void))
 (define g$b (void))
 (define g$a (void))
 (define g$__name__ (void))
 (define g$c (void))
 (define g$y (void))
 (set-then!
  g$__name__
  "__main__"
  (set-then!
   g$b
   (lambda (k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15) ((lambda (k16) (return "hi" k16)) k15))
      k14))
   (set-then!
    g$c
    (lambda (k17)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k18) ((lambda (k19) (return #f k19)) k18))
       k17))
    (set-then!
     g$d
     (lambda (k20)
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (return k21) ((lambda (k22) (return 2 k22)) k21))
        k20))
     (set-then!
      g$a
      (lambda (n k23)
        ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
         (lambda (return k24)
           ((lambda (k25)
              ((cps equal?)
               n
               1
               (lambda (rv26)
                 (if rv26
                   ((lambda (k27)
                      (g$b
                       (lambda (rv28)
                         (g$c
                          (lambda (rv29)
                            (g$a 0 (lambda (rv30) (return rv30 k27))))))))
                    k25)
                   ((lambda (k31) (g$d (lambda (rv32) (return "hi" k31))))
                    k25)))))
            k24))
         k23))
      (g$a 5 (lambda (rv33) (set-then! g$y rv33 ($halt (void)))))))))))

