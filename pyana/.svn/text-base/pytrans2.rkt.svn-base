#! /usr/bin/env racket
#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 3: High level translation for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;
; The translator program, it can accept a filename
; argument from command line as input file.
; Without command line arguments, it
; reads from STDIN directly.
;
; The  program handles string. 
; (string in python 3 is immutable, no assign or set allowed
;
; Print is not allowed to be assgined in python 3. 
; Test cases with print assigned will emit error.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "utils_pytrans.rkt")

(define (match-stmt-onebyone stmts indef symtbl)
  (if (= 0 (length stmts))
      (list)
      (let* ((s (first stmts))
             (s-res (match-stmt s indef symtbl)))
        (if (and (list? s) (equal? 'begin (first s)))
            (append s-res (match-stmt-onebyone (drop stmts 1) indef symtbl))
            (append (list s-res) (match-stmt-onebyone (drop stmts 1) indef symtbl))))))

(define (match-program p)
  (match p
    [`(program) `(program) ]
    [`(program ,stmts ...) (let* ([raw-res (match-stmt-onebyone stmts #f global-symtbl) 
                                           ]
                                  [res (map force-prod raw-res)])
                             
                                     (append (list `program) 
                                             (list `(define gbl-dict (void)))
                                             ;(list `(define builtins (void)))
                                             ;(list `(define len (void)))
                                             (list `(define object (void)))
                                             (list `(define List (void)))
                                             (list `(define set  (void)))
                                             (list `(define tuple (void)))
                                             (list `(define len (void)))
                                             
                                               ;; the sort below used to sort the global header since define should be at the very front.
                                             (sort (hash-map global-symtbl 
                                                             (lambda (k v)
                                                               (if (and (not (equal? v 'local)) 
                                                                        (not (equal? v 'global))
                                                                        (not (equal? v 'nonlocal)))
                                                                     v
                                                                   `(define ,k (void)))
                                                                   ))
                                             #:key car-str string<?)
                                             
;                                             (list `(set-global! builtins
;                                                                 (dict 
;                                                                  "len" ,(gen-len-func))))
                                            ; (list `(set-global! len ,(gen-len-func)))
                                             (list `(set-global! len ,(gen-len)))
                                             (list `(set-global! object (lambda () (void))))
                                             (list (gen-set-gbl-container 'List "instPyList"))
                                             (list (gen-set-gbl-container 'set "instSet"))
                                             (list  (gen-set-gbl-container 'tuple "instTuple"))
                                            
                                             
(list `(set-global! gbl-dict (dict)))
                                               (list `(dict-set! gbl-dict object 
                                                                 (dict
                                                                  ("__base__" (list));(quasiquote ()));`())
                                                                  ("__mro__"  (list object));(quasiquote ((unquote object))));`(,object))
                                                                  ("__init__" (lambda v void)))))
                                               (list `(dict-set! gbl-dict List
                                                                 (dict 
                                                                  ("__base__"  (list object));(quasiquote ((unquote object))));`(,object))
                                                                  ("__mro__"  (list List object));(quasiquote ((unquote List) (unquote object))));`(,List ,object)) ;`( ,(get-global List) ,object))
                                                                  ("__init__" ,(gen-list-init));(void))
                                                                  ("append"  ,(gen-list-append))
                                                                  ;("index" ,(gen-list-index));(void))
                                                                  ;                                                                             ("count" ,(gen-list-count));(void))
;                                                                             ("append" ,(gen-list-append));(void))
                                                                  ;                                                                             ("insert" ,(gen-list-insert));(void))
                                                                  ;                                                                             ("reverse" (void))
                                                                  ;                                                                             ("sort"  (void))
                                                                  ;                                                                             ("pop"  (void))
                                                                  ;                                                                             ("extend" (void))
                                                                  )
                                                                 ))
                                               (list `(dict-set! gbl-dict set
                                                                            (dict 
                                                                             ("__base__" (list object));`(,object)); (quasiquote ((unquote object))));
                                                                             ("__mro__"  (list set object));`(,set ,object));;(quasiquote ((unquote set) (unquote object)))) ; ;`( ,(get-global set) ,object))
                                                                             ("__init__" ,(gen-list-init));(void))
                                                                             ;("index" ,(gen-list-index));(void))
                                                                             ;("count" ,(gen-list-count));(void))
                                                                             ;("append" ,(gen-list-append));(void))
                                                                             ;("insert" ,(gen-list-insert));(void))
                                                                             ;("sort"  (void))
                                                                             ;("pop"  (void))
                                                                             )
                                                                           ))
                                               (list `(dict-set! gbl-dict tuple
                                                                            (dict 
                                                                             ("__base__" (list object));(quasiquote ((unquote object))));`(,object))
                                                                             ("__mro__"  (list tuple object));(quasiquote ((unquote tuple) (unquote object))));`(,tuple ,object))
                                                                             ("__init__" ,(gen-tuple-init));(void))
                                                                             ;("index" ,(gen-list-index));(void))
                                                                             ;("count" ,(gen-list-count));(void))
                                                                             )
                                                                            ))  
;                                             (list `(set-global! gbl-dict 
;;                                                                 (make-hash 
;;                                                                           (list 
;;                                                                            (cons object 
;;                                                                                 (dict 
;;                                                                                    
;;                                                                                    ("__base__" (list))
;;                                                                                    ("__mro__" (list object))
;;                                                                                    ("__init__" (lambda v void)))))
;;;                                                                           (list ,@(hash-map 
;;;                                                                                    global-dict-symtbl
;;;                                                                                   (lambda (k v)
;;;                                                                                     `(cons ,k ,v)) ))
;;                                                                           )
;                                                                 (dict ;;; yeah, using the langauge in the hir-spec header!
;                                                                           (object 
;                                                                                 (dict 
;                                                                                    
;                                                                                    ("__base__" (list));(quasiquote ()));`())
;                                                                                    ("__mro__"  (list object));(quasiquote ((unquote object))));`(,object))
;                                                                                    ("__init__" (lambda v void))))
;;                                                                           (list ,@(hash-map 
;;                                                                                    global-dict-symtbl
;;                                                                                   (lambda (k v)
;;                                                                                     `(cons ,k ,v)) ))
;                                                                           (List
;                                                                            (dict 
;                                                                             ("__base__"  (list object));(quasiquote ((unquote object))));`(,object))
;                                                                             ("__mro__"  (list List object));(quasiquote ((unquote List) (unquote object))));`(,List ,object)) ;`( ,(get-global List) ,object))
;                                                                             ("__init__" ,(gen-list-init));(void))
;                                                                             ;("index" ,(gen-list-index));(void))
;;                                                                             ("count" ,(gen-list-count));(void))
;;                                                                             ("append" ,(gen-list-append));(void))
;;                                                                             ("insert" ,(gen-list-insert));(void))
;;                                                                             ("reverse" (void))
;;                                                                             ("sort"  (void))
;;                                                                             ("pop"  (void))
;;                                                                             ("extend" (void))
;                                                                             
;                                                                             )
;                                                                            )
;                                                                           
;                                                                           (set
;                                                                            (dict 
;                                                                             ("__base__" (list object));`(,object)); (quasiquote ((unquote object))));
;                                                                             ("__mro__"  (list set object));`(,set ,object));;(quasiquote ((unquote set) (unquote object)))) ; ;`( ,(get-global set) ,object))
;                                                                             ("__init__" ,(gen-list-init));(void))
;                                                                             ;("index" ,(gen-list-index));(void))
;                                                                             ;("count" ,(gen-list-count));(void))
;                                                                             ;("append" ,(gen-list-append));(void))
;                                                                             ;("insert" ,(gen-list-insert));(void))
;                                                                             ;("sort"  (void))
;                                                                             ;("pop"  (void))
;                                                                             )
;                                                                           )
;                                                                           
;                                                                           (tuple
;                                                                            (dict 
;                                                                             ("__base__" (list object));(quasiquote ((unquote object))));`(,object))
;                                                                             ("__mro__"  (list tuple object));(quasiquote ((unquote tuple) (unquote object))));`(,tuple ,object))
;                                                                             ("__init__" ,(gen-tuple-init));(void))
;                                                                             ;("index" ,(gen-list-index));(void))
;                                                                             ;("count" ,(gen-list-count));(void))
;                                                                             )
;                                                                            )
;                                                                 )))
;                                             
                                       res))]     
      [else (display "FAILED match-program. \n")]))


(define (match-stmt stmt indef symtbl)
  (match stmt
    ;; compound statment
    [`(cond (,tests ,suites) ...  (else ,suite))
     (let ([res-if (map (λ (x y) (cons (match-right-test x indef symtbl) (match-suite y indef symtbl))) tests suites)]
           [res-else (match-suite suite indef symtbl)])
       `(cond ,@(map (λ (ele) `(,(car ele) ,(cdr ele))) res-if)  (else ,res-else))  )]
       
    [`(cond (,tests ,suites) ...)  
      (let ([res-if (map (λ (x y) (cons (match-right-test x indef symtbl) (match-suite y indef symtbl))) tests suites)])
       `(cond ,@(map (λ (ele) `(,(car ele) ,(cdr ele))) res-if) )   )]

    [`(while ,test ,suite)
     (let ([res-test (match-right-test test indef symtbl)]
           [res-suite (match-suite suite indef symtbl)])
       `(while ,res-test ,res-suite))]
    
    [`(while ,test ,suite ,suite2) 
     (let ([res-test (match-right-test test indef symtbl)]
           [res-suite (match-suite suite indef symtbl)]
           [res-suite2 (match-suite suite2 indef symtbl)])
       `(while ,res-test ,res-suite ,res-suite2))]
    
    [`(for ,name ,test ,suite) 
     (let* ((res-test (match-right-test test indef symtbl))
           (res-suite (match-suite suite indef symtbl))
           ($res-name (gensym 'i)))
       (add-symbol name indef symtbl) ; ; yeah, I don't need this line
       `(for-each 
         ;,name
         ,$res-name 
         ,res-test 
                  (begin 
                    ,(gen-set-res name $res-name indef symtbl) ; yeah, I don't need this line
                    ,res-suite)
                  ))]
    
    [`(for ,name ,test ,suite ,suite2) 
     (let* ((res-test  (match-right-test indef symtbl))
           (res-suite (match-suite suite indef symtbl))
           (res-suite2 (match-suite suite2 indef symtbl))
           ($res-name (gensym 'i)))
       (add-symbol name indef symtbl) 
       `(for-each ,$res-name ,res-test (begin ,(gen-set-res name $res-name indef symtbl) ,res-suite) ,res-suite2 ))]
    
    [`(try ,suite ((,catch ,suite1)) #f #f)
     `(try ,(match-suite suite indef symtbl) (λ (ex) ,(match-suite suite1 indef symtbl)) )]

    [`(try ,suite ((,catchs ,suites) ... ) ,else ,finally)
     (error (format "cannot translate ~a\n" stmt))]
    
     ; <funcdef> ::= (def (<NAME> <NAME>*) <suite>)
    [`(def ,names ,suite) 
     (let* ((localsymtbl (make-hash))
            (params (drop names 1))
            (tmp (map (λ (x) (add-symbol x #t localsymtbl)) params))
            (res-suite (force-prod (match-suite suite #t localsymtbl)))
            (function-name (first names))       )
       
       (hash-set! symtbl function-name (if indef 'local 'global))       
       (let ((final-suite (append (list) (list (first res-suite))
                                  `(,(let ((locals (list))) 
                                     (hash-map localsymtbl 
                                              (λ (k v) 
                                                (when (and (equal? v 'local) (not (in-list? params k)))
                                                  (set! locals (append locals (list `(,k (void))))))))
                                         locals))
                                  (drop res-suite 2))))
         ;`(set! ,function-name (lambda ,params (call/ec (lambda (return) ,final-suite))))))]     
     (gen-set-res function-name  `(lambda ,params (call/ec (lambda (return) ,final-suite))) indef symtbl)))]
    
    [`(class ,cnames ,suite)
     (let* ((localsymtbl (make-hash))
            ;(base (drop cnames 1)) ; this part will be ste intot hte base field of newly created dict
            (base (map (lambda (x) (gen-get-res x indef symtbl)) (drop cnames 1)))
            ;(tmp (map (λ (x) (add-symbol x #t localsymtbl)) params))
            (res-suite (force-prod (match-suite suite #t localsymtbl)))
            (class-name (first cnames))
            ;(class-name (string->symbol (string-append "Class%" (symbol->string (first cnames))))) 
            (last2 (drop res-suite 2))
            (class-dict (string->symbol (string-append (symbol->string class-name) "_dict")))
            (return-dict (make-initial-obj-dict class-name))
            (init-args (get-init-args last2))
            (class-name-scope (gen-get-res class-name indef symtbl))
            )
       (hash-set! symtbl class-name (if indef 'local 'global))
       (hash-set! gbl-scope-map class-name class-name-scope)
       
       (gen-constr res-suite last2 class-name class-name-scope class-dict base localsymtbl return-dict init-args indef symtbl)
       )]
    
     
    [simplestmt (let ([ss (match-simplestmt simplestmt indef symtbl)])
                     ss)]
    [else (error "FAILED match-stmt. \n")]))



(define (match-suite s indef symtbl)
  (match s    
    [`(suite ,stmts ...)  
     (let ((res-stmts (match-stmt-onebyone stmts indef symtbl)))
       `(let () ,@res-stmts))]
    [simplestmt  
     (let ((res-ss (match-simplestmt simplestmt  indef symtbl)))
       `(let () ,res-ss))]
    [else (error "FAILED match-suite. \n")]))

(define (match-simplestmt ss indef symtbl)
  (match ss
    [`(begin ,small-stmts ...)
     (map (λ (x) (match-small-stmt  x indef symtbl)) small-stmts)]
    [s (match-small-stmt s indef symtbl)]
    [else (error "failed match-simplestmt ~a\n" ss)]))  

(define (match-small-stmt ss indef symtbl)
  (match ss
    [`(del ,del-indexed) (match-del-indexed del-indexed indef symtbl)]
    [`(pass) `(void)]
    [`(break) `(break)]
    [`(continue) `(continue)]
    [`(return ,tests ... ) (let* ((test-ress (map (λ (x) (match-right-test x indef symtbl)) tests))
                                  (dict-pairs (constr-pylist-dict-pairs test-ress))
                                  )
      
                                 
                        (cond
                          [(= (length tests) 0) `(return (void))]
                          [(= (length tests) 1) `(return ,@test-ress)]
                          [else  `(return  ((get-global tuple) 
                                             (dict ,@dict-pairs))   ;(tuple ,@test-ress)
                                          )]))] 
    [`(raise) `(throw current-exception)]
    [`(raise ,test) `(throw ,(match-right-test test indef symtbl))]
    
    [`(raise ,test ,test2) `(throw (chain-exception ,(match-right-test test indef symtbl) ,(match-right-test test2 indef symtbl)))]
    
;    [(or `(global ,names ...) `(nonlocal ,names ...)) (map (λ (x) (declare-global x indef symtbl)) names) `(void)]
    
    [`(global ,names ...) (map (λ (x) (declare-global x indef symtbl)) names) `(void)]
    [`(nonlocal ,names ...) (map (λ (x) (declare-nonlocal x indef symtbl)) names) `(void)]
    
    [`(assert ,test) (let ((test-res (match-right-test test indef symtbl)))    `(assert ,test-res))]
    [`(assert ,test ,test2) (let ((test-res (match-right-test test indef symtbl))
                                  (test2-res (match-right-test test2 indef symtbl)))
                              `(assert ,test-res ,test2-res))]
    [else (match-expr-stmt ss indef symtbl)]))

(define (match-del-indexed ss indef symtbl)
  (match ss
    [`(indexed ,atom ,trailers ...)
     (let* ((atom-res (match-right-atom atom indef symtbl)))
       (if (= 1 (length trailers))
           (match-del-trailer (first trailers) atom-res indef symtbl)
           (let ((gets (match-right-trailers (drop-right trailers 1) atom-res indef symtbl)))
             (match-del-trailer (last trailers) gets indef symtbl)))
       )]
    [else (error (format "failed match-del-indexed: no match for ~a\n" ss))]))

(define (match-del-trailer s at indef symtbl)
  (match s
    [`(dot ,name) (let ((base (gensym 'b)))
                    `(let ((,base ,at)) (remove-field! ,base ,(symbol->string name))))]
    [`(subscript ,tort) (let ((tort-res (match-right-tort tort indef symtbl))
                              (base (gensym 'b)) (idx (gensym 'i)))
                          `(let ((,base ,at)) 
                             (let ((,idx ,tort-res))
                               (cond
                                 ((tuple? ,base) (error "Cannot delete from tuples!"))
                                 ((py-list? ,base) (py-list-remove! ,base ,idx))
                                 ((dict? ,base) (dict-remove! ,base ,idx))
                                 ((string? ,base) (error "Cannot delete from string!"))))))]
    [`(called ,tests ... ) (error (format "cannot delete ~a\n" s))]
    [else (error (format "failed match-del-trailer on ~a\n" s))]))

(define (match-expr-stmt stx indef symtbl)
  (match stx
    [`(= ,tests ,tort) 
     (if (> (length tests) 1)
         (let* ((valsymb (gensym 't))  (indexed-tests (gen-indexed-ids tests))
                (left-res (map (λ (x) (match-left-test (car x) (gen-idxed-rhs  valsymb (cdr x)) indef symtbl)) 
                               indexed-tests))
                (right-res (match-right-tort tort indef symtbl)))
           `(let ((,valsymb ,right-res))  ,@left-res)
         )
         (let* ((right-res (match-right-tort tort indef symtbl)))
           (match-left-test (car tests) right-res indef symtbl)))
          ]
    
    [`(expr ,tort) (match-right-tort tort indef symtbl) ]
    [`(,augassign ,tests ,tort) 
     
     (if (> (length tests) 1)
         (error "can not transform multi-augassign\n")
         (if (and (list? (first tests)) (equal? 'indexed (first (first tests))))
             (match-indexed-augassign stx indef symtbl)
             (let* ( (right-res (match-right-tort tort indef symtbl))
                     (get-left-res (match-right-tort (car tests)
                                                     indef symtbl))
                     (op (hash-ref opsmap augassign))
                     (left-res 
                      (match-left-test 
                       (car tests) 
                       `(,(car op) ,get-left-res
                                   ,right-res ) indef symtbl )))
               left-res)))]
    [else (error (format "failed match-expr-stmt ~a\n" stx))]
    ))

(define (match-indexed-augassign s indef symtbl)
  (let* ((raw-op (first s)) (op (car (hash-ref opsmap raw-op)))
                            (raw-test (first (second s))) (raw-tort (last s)) (res-tort (match-right-tort raw-tort indef symtbl))
                            (raw-last-tr (last raw-test)))
    (match raw-last-tr
      [`(called ,_ ...) (error (format "cannot assign to ~a\n" raw-last-tr))]
      [else (void)])
    (let* ((raw-trailers (drop raw-test 2)))
      (if (equal? 1 (length raw-trailers))
          (gen-indexed-augas op res-tort (match-right-atom (second raw-test) indef symtbl) (first raw-trailers) indef symtbl)
          (gen-indexed-augas op res-tort 
                             (match-right-tort `(,(first raw-test) ,(second raw-test) ,@(drop-right raw-trailers 1)) 
                                               indef symtbl) raw-last-tr indef symtbl))           
      )
    ))

(define (gen-indexed-augas op rhs base raw-trailer indef symtbl)
  (match raw-trailer
    [`(dot ,name) (let ((bsym (gensym 'b)))
                    `(let ((,bsym ,base))
                       (set-field! ,bsym ,(symbol->string name) (,op ,rhs (get-field ,bsym ,(symbol->string name))))))]
    [`(subscript ,tort) 
     (let* ((isym (gensym 'i)) (bsym (gensym 'b)) (self-sym (gensym 'v))
                               (get-self-res (gen-idxed-rhs bsym isym))
                               (crhs `(,op ,self-sym ,rhs)))
       `(let ((,bsym ,base))
          (let ((,isym ,(match-right-tort tort indef symtbl)))
            (let ((,self-sym ,get-self-res))
              (cond
                ;((tuple? ,bsym)      (tuple-set! ,bsym ,isym ,crhs))
                ((py-list? ,bsym)    (py-list-set! ,bsym ,isym ,crhs))
                ((dict? ,bsym)       (dict-set! ,bsym ,isym ,crhs))
                ((string? ,bsym)     (error "cannot set string element"))))
                )))
       ]
    [else (error (format "cannot translate ~a\n" raw-trailer))])
  )
    

(define (match-left-trailers trs at rhs indef symtbl)
  (if (= (length trs) 1)
      (match-left-trailer (car trs) at rhs indef symtbl)
      (match-left-trailers (drop trs 1) 
                           (match-right-trailer (first trs) at indef symtbl)
                           rhs indef symtbl)))


(define (match-left-trailer tr at rhs indef symtbl)
  (match tr
    [`(subscript ,tort)
     (let ((tort-res (match-right-tort tort indef symtbl)))
       (gen-idxed-lhs at tort-res rhs)
       )
     ]
    [`(called ,arglist ...)
     (error (format "cannot set-index: ~a\n" tr))]
    [`(dot ,name)
     `(set-field! ,at ,(symbol->string name) ,rhs)]
    [else (error "failed match-left-trailer\n")]))

(define (match-right-trailer tr at indef symtbl)
  (match tr
    [`(subscript ,tort)   (let ((tort-res (match-right-tort tort indef symtbl)))
       (gen-idxed-rhs at tort-res))       
     ]
    [`(called ,arglists ... )
     (let ((argres (map (λ (x) (match-right-test x indef symtbl)) arglists)))  `(,at ,@argres))]
    [`(dot ,name)     `(get-field ,at  ,(symbol->string name) )]
    [else (error "failed match-left-trailer\n")])) 

(define (match-right-trailers trs at indef symtbl)
  (if (= (length trs) 1)
      (match-right-trailer (car trs) at indef symtbl)
      (match-right-trailers 
       (drop trs 1)
       (match-right-trailer (first trs) at indef symtbl)
       indef symtbl)))

(define (match-right-tort s indef symtbl)
  (match s
    [`(tuple ,tests ...)
     (let* ((rest-ress (map (λ (x)   (match-right-test x indef symtbl))  tests))
           (dict-pairs (constr-pylist-dict-pairs rest-ress))
           )
       `((get-global tuple) 
         (dict ,@dict-pairs)))]
       
        ; `(tuple ,@rest-ress))]
    [test
     (match-right-test test indef symtbl)]
    [else (error "failed match-right-tort\n")]
    ))

(define (match-left-atom s rhs indef symtbl)
  (match s
    [(or 'True 'False 'None)  (error (format "cannot assign to ~a\n" s))]
    ['Ellipsis 'Ellipsis]
    [nns
     (cond
       [(or (number? nns) (string? nns)) nns]
       [(symbol? nns)   (add-symbol nns indef symtbl) (let ((set-res (gen-set-res nns rhs indef symtbl)))
                                                        set-res)]
       [else (error (format "cannot match~a\n" nns))])]
    [else (error (format "cannot assign to ~a\n" s))]
    ))

;<comprehension> ::= (comprehension <test> <comp_for>)
;<comp_for>     ::= (compfor <NAME> <test> [comp_iter])
;<comp_if      ::= (compif <test> [<comp_iter>])
;<comp-iter>    ::= <comp_for> | <comp_if>


(define (match-comp-iter comp-iter indef symtbl)
  (match comp-iter
    [`(compfor ,name ,expr ,comp-iter) 
      (hash-set! symtbl name (if indef 'local 'global))   
      
      (let ((seq (match-right-test expr indef symtbl))
            (ci (match-comp-iter comp-iter indef symtbl)))
           
        `(compfor ,name ,seq ,ci) )
        
        ]
     [`(compif ,test ,comp-iter)
      (let ((if-test (match-right-test test indef symtbl))
            (ci (match-comp-iter comp-iter indef symtbl)))
        `(compif ,if-test ,ci))
      ]
     [`(compfor ,name ,expr) 
       (hash-set! symtbl name (if indef 'local 'global))
      (let ((seq (match-right-test expr indef symtbl)))
       
         `(compfor ,name ,seq))
        ]
     [`(compif ,test)
      (let ((if-test (match-right-test test indef symtbl)))
        `(compif ,if-test))
      ]
     [else  (error (format "failed in matching ~a!" comp-iter))]
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TO be deleted;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;[`(def ,names ,suite) 
;     (let* ((localsymtbl (make-hash))
;            (params (drop names 1))
;            (tmp (map (λ (x) (add-symbol x #t localsymtbl)) params))
;            (res-suite (force-prod (match-suite suite #t localsymtbl)))
;            (function-name (first names))       )
;       
;       (hash-set! symtbl function-name (if indef 'local 'global))       
;       (let ((final-suite (append (list) (list (first res-suite))
;                                  `(,(let ((locals (list))) 
;                                     (hash-map localsymtbl 
;                                              (λ (k v) 
;                                                (when (and (equal? v 'local) (not (in-list? params k)))
;                                                  (set! locals (append locals (list `(,k (void))))))))
;                                         locals))
;                                  (drop res-suite 2))))
;         ;`(set! ,function-name (lambda ,params (call/ec (lambda (return) ,final-suite))))))]     
;     (gen-set-res function-name  `(lambda ,params (call/ec (lambda (return) ,final-suite))) indef symtbl)))] 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (match-right-atom s indef symtbl)
  (match s
    [`(tuple) `(tuple)]
    [`(tuple ,tests ...) (match-right-tort s indef symtbl) ]
    
    [`(list (comprehension ,for-expr ,comp-iter))
     
      ;(define-values (comp-for comp-if) (partition compfor? comp-lst))
     
     (let* (
            (localsymtbl (make-hash))
           ;(comp-test (match-right-test for-expr indef symtbl))
             (comp-lst (constr-comp-lst (force-prod
                                         (if (equal? #t indef)
                                           (match-comp-iter comp-iter indef symtbl)  
                                           (match-comp-iter comp-iter #t localsymtbl))) (list)))
             (comp-test 
              (if (equal? #t indef) 
                  (match-right-test for-expr indef symtbl)
                  (match-right-test for-expr #t localsymtbl)))
             )
       ;(desugar-compre comp-lst comp-test))
      `((get-global List) ,(desugar-compre comp-lst comp-test)))
     ]
    
    [`(list ,tests ...)
     (let* ((test-ress (map (λ (x) (match-right-test x indef symtbl)) tests))
            (dict-pairs (constr-pylist-dict-pairs test-ress)))
        ;`(py-list* ,@test-ress)
       ;`((get-global List) ,@dict-pairs)
       `((get-global List) 
         (dict ,@dict-pairs)
;         (dict ("__type__" "instPyList")
;              ("__containerdict__" 
;               (dict ,@dict-pairs)))
         
         )
       
;       `(dict ("__type__" "pylist")
;              ("__containerdict__" 
;               (dict ,@dict-pairs))
;              ; the builtin functions! temperarily doing nothing
;              ("append" ,(gen-list-append))
;              ("insert" ,(gen-list-insert))
;              ("index"  ,(gen-list-index))
;              ("pop"    (void));,(gen-list-pop))
;              ("count"  ,(gen-list-count))
;              ("sort"   (void));,(gen-list-sort))
;              ("reverse" (void))
;              ("extend"  (void))
;              
;              ;("len" ,(gen-len-func))
;              )
              
       
       )]
    
    
    [`(dict (,test1 ,test2) ...)
     (let ((test1-ress (map (λ (x) (match-right-test x indef symtbl))  test1))
           (test2-ress (map (λ (x) (match-right-test x indef symtbl))  test2)))
       `(dict ,@(map (λ (k v) `(,k ,v))  test1-ress test2-ress)))]
    
    [`(dict (dict_compr (,k ,v) ,cp-it))
     (let* (
            (localsymtbl (make-hash))
           ;(comp-test (match-right-test for-expr indef symtbl))
             (comp-lst (constr-comp-lst (force-prod
                                         (if (equal? #t indef)
                                           (match-comp-iter cp-it indef symtbl)  
                                           (match-comp-iter cp-it #t localsymtbl))) (list)))
             (comp-test 
              (if (equal? #t indef) 
                  (begin
                    (let 
                        ((key (match-right-test k indef symtbl))
                         (val (match-right-test v indef symtbl)))
                      `(dict (,key ,val))
                    
                    ))
                  (begin
                    (let 
                        ((key (match-right-test k #t localsymtbl))
                         (val (match-right-test v #t localsymtbl)))
                      `(dict (,key ,val))
                    ))))
                 ; (match-right-test for-expr indef symtbl)
                 ; (match-right-test for-expr #t localsymtbl)))
             )
       ;(desugar-compre comp-lst comp-test))
      (desugar-compre comp-lst comp-test))
     ]
    
    [`(set (comprehension ,for-expr ,cp-it))
     (let* (
            (localsymtbl (make-hash))
           ;(comp-test (match-right-test for-expr indef symtbl))
             (comp-lst (constr-comp-lst (force-prod
                                         (if (equal? #t indef)
                                           (match-comp-iter cp-it indef symtbl)  
                                           (match-comp-iter cp-it #t localsymtbl))) (list)))
             (comp-test 
              (if (equal? #t indef) 
                  (match-right-test for-expr indef symtbl)
                  (match-right-test for-expr #t localsymtbl)))
             )
       ;(desugar-compre comp-lst comp-test))
      `((get-global set) ,(desugar-compre comp-lst comp-test)))
     
     ]
    
    
    [`(set ,tests ...)
     (let* ((test-ress (map (λ (x) (match-right-test x indef symtbl))   tests))
           (dict-pairs (constr-pylist-dict-pairs test-ress)))
       `((get-global set) 
         (dict ,@dict-pairs)))]
       
       ; `(set ,@test-ress))]
    
    ['Ellipsis  'Ellipsis]
    ['True true]
    ['False false]
    ['None  'None]
    [nns
     (cond
       [(or (number? nns) (string? nns))  nns]
       [(symbol? nns)
        ;(add-symbol nns indef symtbl)
        (let ((get-res (gen-get-res nns indef symtbl)))
           get-res)]
       [else
        (error (format "cannot match~a\n" nns))])]
    [else (error "failed match-right-atom\n")]
    ))

;on the left hand side, something like bla = .... the bla like lambada , abd , not, etc, can not be assigned
;but the indexed, atom ,trailers like A.fieldname = 5.
;If it is a atom (as a left atom), like a True, False, etc. you can not do True = something.

(define (match-left-test s rhs indef symtbl)
  (match s
    [(or `(if ,_ ...) `(lambda ,_ ...) `(or ,_ ...) `(and ,_ ...) `(not ,_ ...) `(comparison ,_ ...) 
         `(bitwise-or ,_ ...) `(bitwise-xor ,_ ...) `(bitwise-and ,_ ...) `(bitwise-not ,_ ...) `(shift ,_ ...) `(arith ,_ ...)
         `(term ,_ ...) `("+" ,_ ...) `("-" ,_ ...) `("~" ,_ ...) `(power ,_ ...))
         (error (format "cannot assign to ~a\n" s))]
    
    [`(indexed ,atom ,trailers ...)
     (let* ((atom-res (match-right-atom atom indef symtbl)))
       (match-left-trailers trailers atom-res rhs indef symtbl) )]
    [`(star ,test)
     (error (format "cannot translate ~a\n" s))]
    [atom (match-left-atom atom rhs indef symtbl)]
    [else (error "failed match-left-test\n")]))
    
(define (match-right-test s indef symtbl)
  (match s
    [`(if ,or-test1 ,or-test2 ,test)
     (let ((c-res (match-right-test or-test1 indef symtbl))
           (t-res (match-right-test or-test2 indef symtbl))
           (e-res (match-right-test test indef symtbl)))
     `(if ,c-res ,t-res ,e-res))]
    [`(lambda ,names ,test)
     (let ((conflicted-symbs (get-conflicted-symbs symtbl names)))       
       (map (λ (x) (hash-set! symtbl x 'local)) names)
       (let ((test-res (match-right-test test #t symtbl)))
         (map (λ (x) (hash-remove! symtbl x)) names)
         (map (λ (x) (hash-set! symtbl (car x) (cdr x))) conflicted-symbs)
         `(lambda ,names ,test-res)))]
    [`(indexed ,atom ,trailers ...)
     (let* ((atom-res (match-right-atom atom indef symtbl)))
       ;(if (equal? atom-res 'super)
        ;   (hash-set! symtbl 'super 'local)
           (match-right-trailers trailers atom-res indef symtbl)
       ;)           
       )]
    [(or `(or ,tests ...) `(and ,tests ...))
     (let ((test-ress (map (λ (x) (match-right-test x indef symtbl))
                           tests))
           (op (car s)))
        `(,op ,@test-ress))]
    [`(not ,test)
     (let ((test-ress (match-right-test test indef symtbl)))                        
       `(not ,test-ress))]
    [`(comparison ,test (,comp-ops ,tests) ...)
     (let ((test-res (match-right-test test indef symtbl))
           (cs-res (map (λ (o t) (cons (hash-ref opsmap o) (match-right-test t indef symtbl))) comp-ops tests)))
       (gen-comps test-res cs-res))]
       
    [`(star ,test)
     (error (format "cannot translate ~a\n" s))]
    [(or `(bitwise-or ,tests ...)
         `(bitwise-xor ,tests ...)
         `(bitwise-and ,tests ...))     
     `(,(first s) ,@(map (λ (x) (match-right-test x indef symtbl)) tests))]
    [`(bitwise-not ,test)
         `(bitwise-not ,(match-right-test test indef symtbl))]
    [(or `(shift ,test (,ops ,tests) ...)
         `(arith ,test (,ops ,tests) ...)
         `(term ,test (,ops ,tests) ...))
     (let ((test-res (match-right-test test indef symtbl))
           (ss-res (map (λ (o t) (cons (hash-ref opsmap o) (match-right-test t indef symtbl))) ops tests)))
       (gen-chain-ops test-res ss-res))]
    
    [(or `("+" ,test) `("-" ,test) `("~" ,test))
     `(,(hash-ref opsmap (first s)) ,(match-right-test test indef symtbl))]
    [`(power ,test1 ,test2)
     `(expt ,(match-right-test test1 indef symtbl) ,(match-right-test test2 indef symtbl))]
    [atom (match-right-atom atom indef symtbl)]
    [else (error "failed match-right-test\n")]))
    
;;;;;;;;;;;;;;;;;;
(define (read-all port)
  (let ((next (read port)))
    (if (eof-object? next)
        '()
        (cons next (read-all port)))))

(define (pytran-file filename)
  (pytran-port (open-input-file filename)))

(define (pytran-port port)
   (let* ([lines (read-all port)]
          [res (match-program (car lines))])
     (pretty-write res)))
  
(define args (current-command-line-arguments))
(match args
  [(vector filename)   (pytran-file filename) (exit)]  
  [else  (pytran-port (current-input-port))])
;(pytran-file  "./tests/tt.py.parsed")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Compiler Project 3: High level translation for Python
; Shuying Liang
; u0694891  shuying.liang@gmail.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


