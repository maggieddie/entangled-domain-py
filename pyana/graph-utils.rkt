#lang racket

(require "utils_cesk3_smt.rkt")

(provide visited->graph
         state->htmlfile
         graph-file
         build-state-id-mapping
         build-st-id-map
         state->id
         id->state
         gbl-st-nexts-map
         reset-gbl-st-nexts-map!
         states->graph2
         states->graph-web
         build-graph-dir
         set-graph-dir!
         set-gf-dir!
         prepare-directory)

; util functions to help with graphviz graph creation

(define (build-graph-dir proj-dir)
  (string-append proj-dir "/state-graph"))

;(define (get-class-path meth-path)
;    (match meth-path
;      [`(array (object ,_ ...)) meth-path]
;      [else
;       (string->symbol
;        (trim-last-slash
;         (constr-path-with-slash ; reconstruct path with slash at end
;          (drop-last (split-path meth-path)))))])) ; strip the methname


(define graph-directory #f)

(define (set-graph-dir! proj-dir)
  (set! graph-directory (build-graph-dir proj-dir))
  ;(secu-prnt "to see whether it is absolute path othe graph")(secu-prnt graph-directory)
)

(define graph-file #f)

(define (set-gf-dir! filename)
  (cond
    [(string? filename) 
     (set! graph-file (string-append graph-directory "/" (elim-dollor filename) ".dot"))]
    [else
     (set! graph-file (string-append graph-directory "/" (methname-format (elim-dollor filename)) ".dot"))])
  (secu-prnt "dot file path")
  (secu-prnt graph-file)
  )

(define (methname-format methpath)
  (cond 
    [(string? methpath)
     (regexp-replace* "/"  methpath "-")]
    [else 
     (regexp-replace* "/" (symbol->string methpath) "-")]))

(define (elim-dollor filename)
  (cond
    [(string? filename) 
      (regexp-replace* "\\$" filename "_")
     ]
    [else
     (regexp-replace* "\\$" (symbol->string filename) "_")
     ]))


(define gbl-st-nexts-map (make-hasheq))
(define (reset-gbl-st-nexts-map!)  (set! gbl-st-nexts-map (make-hasheq)))
; prepare directory
(define (prepare-directory)
  (secu-prnt graph-directory)
  (if (directory-exists? graph-directory)
    #f
    (begin
      ;(secu-prnt "the outfile graph prepared")
      (make-directory graph-directory))))

; -------------------------------------
; Graphviz building funcs
; -------------------------------------
; create graphviz .dot file from visited states
(define (visited->graph visited transition-function st-id-map)
  ;(secu-prnt "###############")
  ;(secu-prnt (hash-count st-id-map))
  (prepare-directory)
  (display-lines-to-file '("digraph states {\n") graph-file #:mode 'text #:exists 'replace)
  (displayln (visited->graph-body st-id-map visited transition-function 0))
  ;(secu-prnt (length visited))
  (display-lines-to-file '("}\n") graph-file #:mode 'text #:exists 'append))
  ; TODO: maybe create svg, ps, etc. here:
  ; $ dot -Tsvg visited-states.dot -o visited-states.html


; for each state in visited states, compute sucessors & write to .dot file
(define (visited->graph-body st-id-map visited transition-function c)
  (if (null? visited)
    c
    (let*
      ([current-state (car visited)]
       [next-states (transition-function current-state)]
       [hashed-current-state (state->id current-state st-id-map)]
       [hashed-states (map (λ (s) (state->id s st-id-map)) next-states)]
       [node-url-text (format
                        "\t\"~a\" [URL=\"~a.html\"]"
                        hashed-current-state
                         hashed-current-state)]
       [edge-text
         (map
           (λ (next-state) (format "\t\"~a\" -> \"~a\";" hashed-current-state next-state))
           hashed-states)])
      (begin
        (state->htmlfile current-state (state->id current-state st-id-map))
        (display-lines-to-file (cons node-url-text edge-text) graph-file #:mode 'text #:exists 'append)
        (visited->graph-body st-id-map (cdr visited) transition-function (add1 c))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; avoid exploring the states again 
; for each state in visited states, compute sucessors & write to .dot file

(define (visited->graph-body2 st-id-map visited c)
  (secu-prnt "in visited->graph-body2")
  (if (empty? visited)
      (begin
        (secu-prnt "empty visited?")
        c)
      (let*
      ([current-state (car visited)]
       [cur-st-attr (hash-ref gbl-st-nexts-map current-state)]
       [ppw (state-attrs-ppw cur-st-attr)]
       [next-states (state-attrs-successors cur-st-attr)]
       [color 
        (cond
          [(equal? (get-color-predi) #f) (colorof-cur-st cur-st-attr)]
          [else (get-user-defined-color current-state cur-st-attr)])]
       [hashed-current-state (state->id current-state st-id-map)]
       [hashed-states (map (λ (s) (state->id s st-id-map)) next-states)]
       [node-url-text (format
                        "\t\"~a\" [style=filled, fillcolor=~a, URL=\"~a.html\"]"
                        ;"\t\"~a\" [ URL=\"~a.html\"]"
                        hashed-current-state
                       color
                       ;(get-relative-apkprj-file (get-basedir))
                        hashed-current-state)]
       [edge-text
         (map
           (λ (next-state) 
             ;(cond 
              ; [ppw (format "\t\"~a\" - \"~a\";" hashed-current-state next-state)]
               ;[else 
                (format "\t\"~a\" -> \"~a\";" hashed-current-state next-state) ;])
           )
           hashed-states)])
      (begin
        ;(when (empty? next-states)
         ; (gen-final-report graph-directory ))
        (state->htmlfile current-state (state->id current-state st-id-map) cur-st-attr)
        (display-lines-to-file (cons node-url-text edge-text) graph-file #:mode 'text #:exists 'append)
        (visited->graph-body2 st-id-map (cdr visited) (add1 c))))))

(define (visited->graph2 st-id-map visited )
  (secu-prnt "visited->graph2 ")
  (secu-prnt graph-file)
  (display-lines-to-file '("digraph states {\n") graph-file #:mode 'text #:exists 'replace)
  (visited->graph-body2 st-id-map visited  0)
  (display-lines-to-file '("}\n") graph-file #:mode 'text #:exists 'append)
  (secu-prnt "closing the dot graph")
  )
;;;;;;;;;end of modification;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (allst->ids)
   (secu-prnt "in alist->ids")
   (define st-list
     (append (hash-keys gbl-st-nexts-map)
             (hash-values gbl-st-nexts-map)))
   (build-state-id-mapping st-list))

;; currently the state graph can't find the state to id mapping! 
 (define (states->graph2 states-explored metas sid-map); prj-dir)
   ;(set! graph-directory (build-graph-dir prj-dir))
   ;(set! graph-file (string-append graph-directory "/visited-states.dot"))
  ; (secu-prnt "in states->graph2")
   (define states-explored-list (set->list states-explored))
  ; (secu-prnt "about to visited")
   ;(define sid-map  (allst->ids))
   ;(secu-prnt sid-map)
   ;(secu-prnt states-explored-list)
   (visited->graph2
      sid-map
      states-explored-list
    ))
 
 (define (states->graph-web states-explored metas prj-dir)
   (define states-explored-list (set->list states-explored))
   (secu-prnt "about to visited")
   (define sid-map  (allst->ids))
   (report-information-flow-per-state! states-explored sid-map prj-dir)
   (states->graph2 states-explored metas sid-map)
   (dot->svg prj-dir))

; print state struct to a html file
(define (state->htmlfile state hashed-state cur-st-attr)
  (define widening? (state-attrs-ppw cur-st-attr))
  (define-values  (api-perms listof-str-vios) (problem-cur-st cur-st-attr))
  ;(secu-prnt "track") (secu-prnt api-perms) (secu-prnt listof-str-vios)
  (define text (list))
  (define filehash (format "~a/~a.html" graph-directory hashed-state))
  (secu-prnt "each graph html!") (secu-prnt filehash)
  
  ;; only output the part if there is problem
  ;(when (or api-perms  listof-str-vios)
    (set! text (append text (list
                             (format "<b> ------------------------------------- </b><br></br>~n~n"  )
                             (format "<b>Problem Desc >>>>>>>>>>>>>>>> </b><br>~n" )
                             (cond
                               [api-perms 
                                (format "<b>APIs-Perms:  </b><p> ~a </p><br></br>~n"  api-perms)]
                               [else (format "<b><p>APIs-Perms:  </b>no apis.perms here in this exploration</p></b><br>~n")])
                             (format "<b>Possible Resc Access Violation:  </b><br></br>~n")
                             (cond 
                               [listof-str-vios 
                                (format "<b><p> ~a </p></b><br></br>~n"  listof-str-vios)]
                               [else
                                (format "<b><p> No sensitive string found during this exploration. </p></b><br></br>~n")])
                             (format "<b> ------------------------------------- </b><br></br>~n~n"  )
                             (format "<p></p><br></br>~n~n"  )
                             (format "<b> ------------------------------------- </b><br></br>~n~n"  ))))
  ;)
  (set! text (append text 
                     (list
                      (format "<b> State Details-----------</b><br></br>~n" )
                      (format "<b>Frame Pointer:</b> ~a<br></br>~n" (state-FP state))
                      (format "<b>Time:</b> ~a<br></br>~n" (state-time state))
                      (format "<b>Statements:</b> <br></br>~a<br></br><br></br>~n"
                              (regexp-replace*
                               "\n"
                               (pretty-format (state-listof-stmt state))
                               "<br></br>\n"))
                      (format "<b>Continuation Address:</b> ~a <br></br>~n" (state-kont-addr state))
                      (format "<b>Store:</b><br></br>~a<br></br>~n"
                              (regexp-replace*
                               "\n"
                               (store->html (state-store state))
                               "<br></br>\n"))
                      (format "<b>Information Flow Store:</b><br></br>~a<br></br>~n"
                              (regexp-replace*
                               "\n"
                               (store->html (state-inff-store state))
                               "<br></br>\n"))
                      (format "<b>Container Store:</b><br></br>~a<br></br>~n"
                              (regexp-replace*
                               "\n"
                               (store->html (state-cstore state))
                               "<br></br>\n"))
                      (format "<b>Information Flow Container Store:</b><br></br>~a<br></br>~n"
                              (regexp-replace*
                               "\n"
                               (store->html (state-inff-cstore state))
                               "<br></br>\n")))))
  (display-lines-to-file text filehash #:mode 'text #:exists 'replace))

(define (get-basedir)
  (define-values (base file boolean) (split-path graph-file))
  (path->string base))
; -------------------------------------
; State identification functions
; -------------------------------------
(define (build-state-id-mapping states)
  (let*-values
    ([(st-id-map _) 
      (for/fold
        ([st-id-map (make-immutable-hash)]
         [counter 0])
        ([st states])
        (values (hash-set st-id-map st counter) (add1 counter)))])
    st-id-map))

(define (test-member im s)
  (hash-ref im s)
  im)

(define (build-st-id-map setof-state)
  (define counter -1)
  (for/fold ([res (make-immutable-hasheq)])
    ([s setof-state])
    (set! counter (add1 counter))
    ;(hash-set res s counter)
    (test-member (hash-set res s counter) s)
    ))



; search st-id-map for the state that corresponds with the given id
(define (id->state id st-id-map)
  (let ([kvs (filter (λ (kv) (equal? id (cdr kv))) (hash->list st-id-map))])
    (if (null? kvs)
      (error "id->state: id not found in st-id-map")
      (car (car kvs)))))

(define (dot->svg proj-dir)
  (define py-exec-path (find-executable-path "python"))
  (define dot-exec-path (find-executable-path "dot"))
  (cond
    [(path? dot-exec-path)
     (define tosvg (system* py-exec-path  "dot2svg.py" dot-exec-path proj-dir))
;     (cond
;       [(not tosvg) (secu-prnt "gen svg OK")]
;       [else (secu-prnt "gen svg failed")])
    (secu-prnt  "gen svg OK")
     ]
    [else
     (displayln (format "can't find dot file"))]))
     

; -------------------------------------
; Other helpers
; -------------------------------------
; display the state's store as html
(define (store->html store)
  (merge-strings
    (hash-map
      store
      (λ (k v)
         (format
           "<span style=\"color:blue;\">~a</span> \n~a\n\n"
           (pretty-format k) (pretty-format v))))))


(define (merge-strings lst)
  (foldl string-append "" lst))

(define (build-rel-path res lst)
  (cond
    [(empty? lst) res]
    [else
     (build-rel-path
      (build-path res (car lst))
      (rest lst))
     ]))

;; absolute-path-str -> relative apk project
(define (get-relative-apkprj-file abs-file-path)
  (define lst-of-str (member "apks" (regexp-split #rx"/" abs-file-path)))
  (path->string 
   (build-rel-path (build-path (car lst-of-str)) (rest lst-of-str))))
