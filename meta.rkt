#lang racket

(require fulmar/standard-chunk
         fulmar/FulmarAbbreviations
         fulmar/private/fulmar-core
         (for-syntax racket))

(provide define/meta definitions definitions-chunk)


; Structs that hold the functions called to produce chunks for various parts
; of meta-functions and meta-structs. Using instances as procedures gets the ref.
(struct meta-struct (def-thunk ref-fn) #:transparent
        #:property prop:procedure (struct-field-index ref-fn))
(struct meta-function (decl-thunk def-thunk ref-fn) #:transparent
        #:property prop:procedure (struct-field-index ref-fn))


; Remove hyphens and titlecase to make a C++-like type name.
(define-for-syntax (make-cpp-id id)
  (string-replace (string-titlecase id) "-" ""))

(define-for-syntax (syntax->cpp-id stx)
  (make-cpp-id (symbol->string (syntax->datum stx))))


; Meta-struct implementation

(define (make-struct-def-thunk name-cpp-id field-cpp-ids)
  (lambda ()
    (if (null? field-cpp-ids)
      (stmt-def-struct
        name-cpp-id
        '())
      (stmt-def-template-struct
        name-cpp-id
        (map typename field-cpp-ids)
        '()))))

(define (make-struct-ref-fn name-cpp-id field-count)
  (define (reffun . args)
    (when (not (= (length args) field-count))
      (define arguments-list
        (string-join (map ~v args) "\n     "))
      (error (format
#<<END
meta-struct ~a: arity mismatch;
    the expected number of arguments does not match the given number
    expected: ~a
    given: ~a
    arguments:
     ~a
END
                     name-cpp-id
                     field-count
                     (length args)
                     arguments-list)))
    (apply (curry type-template name-cpp-id) args))
  reffun)

(define-for-syntax (meta-struct-stx name fields)
  (let* ([name-cpp-id (syntax->cpp-id name)]
        [field-cpp-ids (map syntax->cpp-id (syntax->list fields))]
        [field-count (length field-cpp-ids)])
    #`(define #,name (meta-struct
                       (make-struct-def-thunk #,name-cpp-id (quote #,field-cpp-ids))
                       (make-struct-ref-fn #,name-cpp-id #,field-count)))))


; Meta-function implementation.

; This sort of repeats make-struct-def-thunk. Need to abstract something out maybe?
; I guess this one isn't too bad.
(define (make-fn-decl-thunk name-cpp-id arg-cpp-ids)
  (lambda ()
    (stmt-dcl-template-struct
      name-cpp-id
      (map typename arg-cpp-ids)
      '())))

; But this one is almost totally repetetive. Abstract.
(define (make-fn-ref-fn name-cpp-id field-count)
  (define (reffun . args)
    (when (not (= (length args) field-count))
      (define arguments-list
        (string-join (map ~v args) "\n     "))
      (error (format
#<<END
meta-struct ~a: arity mismatch;
    the expected number of arguments does not match the given number
    expected: ~a
    given: ~a
    arguments:
     ~a
END
                     name-cpp-id
                     field-count
                     (length args)
                     arguments-list)))
    (typename (scope (apply (curry type-template name-cpp-id) args) 'result)))
  reffun)

; syntax phase structure to hold computed information abuot the match clause.
; * vars is a list of identifiers extracted from the match clause that need to get
; into the template arguments and bindings for which need to be available to the
; return expression.
; * ref is an expression that, given vars are bound appropriately, will produce a
; reference to this subexpression
(begin-for-syntax
  (struct transformed (vars ref) #:transparent))

; creates a transformed struct as described above based on the match clause
(define-for-syntax (transform-match stx)
  (syntax-case stx (quote _)
    [_
     (let ([sym (first (generate-temporaries '(gensym)))])
       (transformed (list sym) sym))]
    [(quote other) (transformed '() #'(quote other))]
    [(mstruct args ...)
     (let* ([transformed-args (map transform-match (syntax->list #'(args ...)))]
            [vars-set (remove-duplicates
                        (apply append (map transformed-vars transformed-args))
                        free-identifier=?)]
            [ref-expr #`(mstruct #,@(map transformed-ref transformed-args))])
       (transformed vars-set ref-expr))]
    [id
     (if (identifier? #'id)
       (transformed (list #'id) #'id)
       (transformed '() #'id))]))

(define-for-syntax (match-case name-cpp-id args matchargs return)
  (define transformed-args (map transform-match matchargs))
  (define vars-set (remove-duplicates
                     (apply append (map transformed-vars transformed-args))
                     free-identifier=?))
  (define tpl-params (map syntax->cpp-id vars-set))
  (define (with-params stx)
  #`(let (
        #,@(map (lambda (var cpp-id) #`[#,var #,cpp-id]) vars-set tpl-params)
          )
      #,stx))
  (define ref-chunks (map (compose with-params transformed-ref) transformed-args))
  (define (with-args stx)
    #`(let (
            #,@(map (lambda (arg expr) #`[#,arg #,expr]) args ref-chunks)
            )
        #,stx)
    )
  #`(
      stmt-def-template-struct
        #,name-cpp-id
        (map typename (list #,@tpl-params)) ;template args
        (list #,@ref-chunks) ; partial specialization
        (stmt-typedef #,(with-args (with-params return)) 'result)
      )
  )

(define-for-syntax (make-fn-def-thunk name-cpp-id args clauses)
  #`(lambda ()
      (top-list
    #,@(map (lambda (stx)
              (syntax-case stx ()
                [[(matchargs ...) return]
                 (match-case name-cpp-id args (syntax->list #'(matchargs ...)) #'return)]))
            clauses))))

(define-for-syntax (meta-fn-stx stx)
  (syntax-case stx ()
    [(_ (name args ...)
        [(matchargs ...) return] ...)
     (let* ([name-cpp-id (syntax->cpp-id #'name)]
           [arg-cpp-ids (map syntax->cpp-id (syntax->list #'(args ...)))]
           [arg-count (length arg-cpp-ids)])
       #`(define name (meta-function
                        (make-fn-decl-thunk #,name-cpp-id (quote #,arg-cpp-ids))
                        #,(make-fn-def-thunk name-cpp-id (syntax->list #'(args ...))
                                             (syntax->list #'([(matchargs ...) return] ...)))
                        (make-fn-ref-fn #,name-cpp-id #,arg-count)
                        ))
       )]))

(define-syntax (define/meta stx)
  (syntax-case stx ()
    [(_ name)
     (meta-struct-stx #'name #'())]
    [(_ (name args ...)
        [(matchargs ...) return] ...)
     (meta-fn-stx stx)]
    [(_ name (field fields ...))
     (meta-struct-stx #'name #'(field fields ...))]))


; Definitions chunk implementation.

(define (definitions-chunk args)
  (apply top-list
    (append (map
              (lambda (item)
                (cond
                  [(meta-struct? item) ((meta-struct-def-thunk item))]
                  [(meta-function? item) ((meta-function-decl-thunk item))]))
              args)
            (map
              (lambda (item)
                ((meta-function-def-thunk item)))
              (filter meta-function? args)))))

(define-syntax (definitions stx)
  (syntax-case stx ()
    [(_ subforms ...)
     (let* ([expanded-subforms (map (lambda (subform)
                                      (local-expand subform (syntax-local-context) (list #'define)))
                                    (syntax->list #'(subforms ...)))]
            [defined-ids (map (lambda (expanded-subform)
                                (syntax-case expanded-subform ()
                                  [(define id expr) #'id]))
                              expanded-subforms)])
       #`(begin
           subforms ...
           (definitions-chunk (list #,@defined-ids))))]))


#|

It would be better if these were unit tests.

(meta/define test (a b))
(write-chunk ((meta-struct-def-thunk test)))
(write-chunk ((meta-struct-ref-fn test) "a" "b"))
; Test<a, b >

(write-chunk ((meta-struct-ref-fn test) "a" "b" "c"))
; arity mismatch

; prop:procedure use test
(write-chunk (test "a" 'b))
; Test<a, b>
(write-chunk (test "a" "b" "c"))
; arity mismatch

; This shouldn't work - define with (meta/define test2) instead.
(meta/define test2 ())
; bad syntax

(meta/define test3)
(write-chunk ((meta-struct-def-thunk test3)))
; struct Test3 {};
(write-chunk ((meta-struct-ref-fn test3)))
; Test3

(meta/define (myf arg)
  [('double) (test 'a 'b)])
(write-chunk ((meta-function-decl-thunk myf)))
; template<Arg>
; struct Myf;
(write-chunk (myf 'a))
; Myf<a >::result
(write-chunk ((meta-function-def-thunk myf)))

(meta/define (myf2 arg)
  [('double) (myf 'a)])
(write-chunk ((meta-function-def-thunk myf2)))

|#
