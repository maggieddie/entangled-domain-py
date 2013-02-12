(module m racket/load


  
  (require rackunit)
(require "../../main.rkt"
         "../../examples/list-helpers.rkt")
 ; (provide len) 
  (smt:with-context
    (smt:new-context)
    (define len (make-length 10))
    (smt:check-sat)))