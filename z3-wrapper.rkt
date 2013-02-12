#lang racket/base

(require ffi/unsafe
         ffi/unsafe/cvector
         ffi/vector
         ffi/unsafe/alloc)
(require racket/runtime-path
         racket/match
         (for-syntax racket/match)
         (for-syntax racket/base))
(require "utils.rkt")

; We let _list also support an output length of 0
; Workaround for https://github.com/plt/racket/pull/87
(define-fun-syntax _list*
  (syntax-rules (i o io)
    [(_ i  t  ) (type: _pointer
                 pre:  (x => (list->cblock x t)))]
    [(_ o  t n) (type: _pointer
                 pre:  (if (> n 0) (malloc n t) #f)
                 post: (x => (cblock->list x t n)))]
    [(_ io t n) (type: _pointer
                 pre:  (list->cblock x t)
                 post: (x => (cblock->list x t n)))]))

(define-runtime-path libz3-path
  (match (system-type 'os)
    ['unix "libz3.so"]
    ['windows "z3.dll"]
    ['macosx "libz3.dylib"]))

(define libz3-without-suffix (path-replace-suffix libz3-path ""))
(define libz3 (ffi-lib libz3-without-suffix))

(define-cpointer-type _z3-config)
(define-cpointer-type _z3-context)

;; We wrap all our pointers up with a z3-boxed-pointer. This serves two purposes:
;; - we hold a strong ref to the context so that it doesn't get GC'd
;; - we can attach pretty printers and other helpful utilities
(struct z3-boxed-pointer (ctx ptr))

(define-syntax define-z3-type
  (syntax-rules ()
    [(_ _TYPE)
     (define-z3-type _TYPE #f)]
    [(_ _TYPE ptr-tag)
     (define-z3-type _TYPE ptr-tag z3-boxed-pointer)]
    [(_ _TYPE ptr-tag ptr-struct)
     (define-cpointer-type _TYPE #f
       z3-boxed-pointer-ptr
       (λ (ptr)
         (when ptr-tag (cpointer-push-tag! ptr ptr-tag))
         (ptr-struct (ctx) ptr)))]))

(struct z3-func-decl-pointer z3-boxed-pointer ()
  #:property prop:procedure (λ (f . args) `(@app ,mk-app ,f ,@args)))

(define-z3-type _z3-symbol)

(define-z3-type _z3-ast)
(define-z3-type _z3-sort z3-ast-tag)
(define-z3-type _z3-app z3-ast-tag)
(define-z3-type _z3-func-decl z3-ast-tag z3-func-decl-pointer)

(define-z3-type _z3-constructor)
(define-z3-type _z3-pattern)
(define-z3-type _z3-model)

;; Enumerations
(define _z3-lbool (_enum '(false = -1 undef true) _int32))
(define _z3-sat-lbool (_enum '(unsat = -1 unknown sat) _int32))
(define _z3-ast-kind (_enum '(numeral app var quantifier unknown = 1000) _int32))
(define _z3-error-code (_enum '(ok sort-error iob invalid-arg parser-error
                                   no-parser invalid-pattern memout-fail
                                   file-access-error invalid-usage
                                   internal-fatal dec-ref-error) _int32))

(define _z3-error-handler (_fun #:keep #t _int -> _void))

;; XXX combine these two
(define-syntax defz3
  (syntax-rules (:)
    [(_ name : type ...)
     (begin
       (define (name . args)
         (apply (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")
                                                      (#rx"^" "Z3_")
                                                      (#rx"!$" "")))
                             libz3 (_fun type ...)) args))
       (provide name))]))
(define-syntax defz3-wrapped
  (syntax-rules (:)
    [(_ name wrapper : type ...)
     (begin
       (define name
         (wrapper
          (lambda args
            (apply (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")
                                                         (#rx"^" "Z3_")
                                                         (#rx"!$" "")))
                                libz3 (_fun type ...)) args))))
       (provide name))]))

;; Deallocators
(defz3 del-config : _z3-config -> _void)
(defz3 del-context : _z3-context -> _void)
(defz3 del-model : _z3-context _z3-model -> _void)

(defz3-wrapped mk-config (allocator del-config) : -> _z3-config)
(defz3 set-param-value! : _z3-config _string _string -> _void)

(define (keyword-arg->_z3-param kw kw-arg)
  (define kw-str (regexp-replaces (string-upcase (keyword->string kw))
                                  '((#rx"-" "_")
                                    (#rx"\\?$" ""))))
  (define kw-arg-str (match kw-arg
                       [#t "true"]
                       [#f "false"]
                       [(? number?) (number->string kw-arg)]
                       [_ kw-arg]))
  (values kw-str kw-arg-str))
(provide keyword-arg->_z3-param)
                                                                         
(defz3-wrapped mk-context (allocator del-context) : _z3-config -> _z3-context)

(defz3 set-logic : _z3-context _string -> _bool)

(defz3 mk-string-symbol : _z3-context _string -> _z3-symbol)
(defz3 mk-uninterpreted-sort : _z3-context _z3-symbol -> _z3-sort)
(defz3 mk-bool-sort : _z3-context -> _z3-sort)
(defz3 mk-int-sort : _z3-context -> _z3-sort)
(defz3 mk-real-sort : _z3-context -> _z3-sort)
(defz3 mk-bv-sort : _z3-context _uint -> _z3-sort)
(defz3 mk-array-sort : _z3-context _z3-sort _z3-sort -> _z3-sort)

(defz3 mk-list-sort : _z3-context _z3-symbol _z3-sort
  (nil-decl : (_ptr o _z3-func-decl))
  (is-nil-decl : (_ptr o _z3-func-decl))
  (cons-decl : (_ptr o _z3-func-decl))
  (is-cons-decl : (_ptr o _z3-func-decl))
  (head-decl : (_ptr o _z3-func-decl))
  (tail-decl : (_ptr o _z3-func-decl))
  -> (res : _z3-sort) ->
  (datatype-instance res (hash 'nil nil-decl
                               'is-nil is-nil-decl
                               'cons cons-decl
                               'is-cons is-cons-decl
                               'head head-decl
                               'tail tail-decl)))

(defz3 mk-true : _z3-context -> _z3-ast)
(defz3 mk-false : _z3-context -> _z3-ast)
(defz3 mk-eq : _z3-context _z3-ast _z3-ast -> _z3-ast)

;; Helper macro to define n-ary AST functions
(define-syntax define-nary
  (syntax-rules (: ->)
    [(_ fn : argtype -> rettype)
     (defz3 fn : (ctx . args) ::
       (ctx : _z3-context)
       (_uint = (length args))
       (args : (_list i argtype)) -> rettype)]))

(define-nary mk-distinct : _z3-ast -> _z3-ast)

;; Boolean operations
(defz3 mk-not : _z3-context _z3-ast -> _z3-ast)
(defz3 mk-ite : _z3-context _z3-ast _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-iff : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-implies : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-xor : _z3-context _z3-ast _z3-ast -> _z3-ast)
(define-nary mk-and : _z3-ast -> _z3-ast)
(define-nary mk-or : _z3-ast -> _z3-ast)

;; Arithmetic operations
(define-nary mk-add : _z3-ast -> _z3-ast)
(define-nary mk-mul : _z3-ast -> _z3-ast)
(define-nary mk-sub : _z3-ast -> _z3-ast)
(defz3 mk-div : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-mod : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-rem : _z3-context _z3-ast _z3-ast -> _z3-ast)

;; Comparisons
(defz3 mk-lt : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-le : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-gt : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-ge : _z3-context _z3-ast _z3-ast -> _z3-ast)

;; Numerals
(defz3 mk-numeral : _z3-context _string _z3-sort -> _z3-ast)

;; Uninterpreted constants, functions and applications
(defz3 mk-fresh-func-decl :
  (ctx prefix domain range) ::
  (ctx : _z3-context)
  (prefix : _string)
  (_uint = (length domain))
  (domain : (_list i _z3-sort))
  (range : _z3-sort)
  -> _z3-func-decl)

(defz3 mk-app : (ctx d . args) ::
  (ctx : _z3-context)
  (d : _z3-func-decl)
  (_uint = (length args))
  (args : (_list i _z3-ast)) -> _z3-ast)

(defz3 mk-fresh-const :
  (ctx prefix sort) ::
  (ctx : _z3-context)
  (prefix : _string)
  (sort : _z3-sort)
  -> _z3-app)

;; Array operations
(defz3 mk-select : _z3-context _z3-ast _z3-ast -> _z3-ast)
(defz3 mk-store : _z3-context _z3-ast _z3-ast _z3-ast -> _z3-ast)

;; Complex types
(defz3 mk-constructor :
  (ctx name recognizer names-sorts-refs) ::
  (ctx : _z3-context)
  (name : _z3-symbol)
  (recognizer : _z3-symbol)
  (_uint = (length names-sorts-refs))
  ((_list i _z3-symbol) = (map car names-sorts-refs))
  ((_list i _z3-sort/null) = (map cadr names-sorts-refs))
  ((_list i _uint) = (map caddr names-sorts-refs))
  -> _z3-constructor)

(defz3 query-constructor :
  (ctx constructor num-fields) ::
  (ctx : _z3-context)
  (constructor : _z3-constructor)
  (num-fields : _uint)
  (constructor-fn : (_ptr o _z3-func-decl))
  (tester-fn : (_ptr o _z3-func-decl))
  (accessor-fns : (_list* o _z3-func-decl num-fields))
  -> _void ->
  (values constructor-fn tester-fn accessor-fns))

(defz3 mk-datatype :
  (ctx name constructors) ::
  (ctx : _z3-context)
  (name : _z3-symbol)
  (_uint = (length constructors))
  (constructors : (_list i _z3-constructor))
  -> _z3-sort)

;; Quantifiers
(defz3 mk-forall-const :
  (ctx weight bound-consts patterns body) ::
  (ctx : _z3-context)
  (weight : _uint)
  (_uint = (length bound-consts))
  (bound-consts : (_list i _z3-app))
  (_uint = (length patterns))
  (patterns : (_list i _z3-pattern))
  (body : _z3-ast)
  -> _z3-ast)

;; -> string functions
(defz3 context-to-string : _z3-context -> _string)
(defz3 ast-to-string : _z3-context _z3-ast -> _string)
(defz3 model-to-string : _z3-context _z3-model -> _string)
(defz3 sort-to-string : _z3-context _z3-sort -> _string)
(defz3 func-decl-to-string : _z3-context _z3-func-decl -> _string)

;; error handling functions
(defz3 get-error-code : _z3-context -> _z3-error-code)
(defz3 get-error-msg : _z3-error-code -> _string)

(defz3 assert-cnstr : _z3-context _z3-ast -> _void)
(defz3 check : _z3-context -> _z3-sat-lbool)
(defz3 check-and-get-model : _z3-context (model : (_ptr o (_or-null _z3-model))) -> (rv : _z3-sat-lbool) -> (values rv model))
(defz3 eval : _z3-context _z3-model _z3-ast (v : (_ptr o (_or-null _z3-ast))) -> (rv : _bool) -> (values rv v))
(defz3 get-ast-kind : _z3-context _z3-ast -> _z3-ast-kind)
(defz3 get-numeral-string : _z3-context _z3-ast -> _string)
(defz3 to-app : _z3-context _z3-ast -> _z3-app)
(defz3 get-app-num-args : _z3-context _z3-app -> _uint)
(defz3 get-app-decl : _z3-context _z3-app -> _z3-func-decl)
