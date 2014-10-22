#lang racket

(require fulmar/standard-chunk
         fulmar/FulmarAbbreviations
         fulmar/private/fulmar-core
         (for-syntax racket))

(provide define/meta definitions definitions-chunk)

; Reimplementation of template-use from standard-chunks to handle the
; extra space before the closing angle bracket.
(define-syntax if-empty-new
  (syntax-rules ()
    [(if-empty given then else)
     (if (empty*? given)
         then
         else)]))

(define (sur-anbr-new . chunks)
  (concat (immediate "<")
          chunks
          (immediate "> ")))

(define (template-list-new . chunks)
  (apply arg-list sur-anbr-new "," chunks))

(define (template-use-new name . args)
  (concat name (if-empty-new args empty (apply template-list-new args))))

(define type-template-new template-use-new)

; This should probably also be in fulmar's standard chunks or abbreviations
(define (stmt-def-struct name . body)
  (stmt (def-struct (dcl-struct name) body)))


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
    (apply (curry type-template-new name-cpp-id) args))
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
    (typename (scope (apply (curry type-template-new name-cpp-id) args) 'result)))
  reffun)


; Trying to figure out what this is meant to be doing. Some sort of transformation on the match pattern. Returning three things (which seem to be lists). Appears to be matching uses of unquoting. Did I go with unquoting in the final version? No! I went with explicit quoting and the default for non call-position being a variable binding, just like Racket's match.

; But I think the idea was to take the match clause and produce a list of identifiers the match clause binds. And maybe to transform it into a form from which to create the C++ specialization stuff too?

; Ah! We've also got to construct appropriate reference combinations of the matchargs for each arg. And we have to construct template parameters for _ matches without binding them in the result expression. And I was having terrible trouble matching _ with syntax case because it has it's own meaning. But adding _ to the () list seems to work.

; So the three lists were probably: 1. variables to add to the template parameters for this case. 2. pattern variables to bind in the result expression. 3. reference expression to use for reference to arg that this pattern is for.
(begin-for-syntax
  (struct transformed (vars ref) #:transparent))
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

(define-syntax (transform-match-test stx)
  (syntax-case stx ()
    [(_ arg) (transform-match #'arg)]))

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

(define-syntax (match-case-test stx)
  (syntax-case stx ()
    [(_ n (args ...) (matchargs ...) return)
     (match-case (syntax-e #'n) (syntax->list #'(args ...)) (syntax->list #'(matchargs ...)) #'return)]))

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


; Definitions chunk implementation. May be untested.

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
                (cond
                  [(meta-function? item) ((meta-function-def-thunk item))]))
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




  (meta/define zero)
  (meta/define succ (n))
  (meta/define m-lambda (name body))
  (meta/define app (fun arg))
  (meta/define ref (name))
  (meta/define lit (t))
  (meta/define emptyenv)
  (meta/define binding (name value env))
  (meta/define closure (lam env))
; functions
  (meta/define (env-lookup name env)
    [(name (binding name value env))  value]
    [(_    (binding name2 value env)) (env-lookup name env)])
  (meta/define (m-eval exp env)
    [((lit t)              _) t]
    [((ref name)           _) (env-lookup name env)]
    [((m-lambda name body) _) (closure (m-lambda name body) env)]
    [((app fun arg)        _) (m-apply (m-eval fun env)
                                       (m-eval arg env))])
  (meta/define (m-apply proc value)
    [((closure (m-lambda name body) env) _)
     (m-eval body (binding name value env))])


(display (string-join (reverse (write-chunk ((meta-function-def-thunk m-eval)))) "\n"))


(definitions
  ; structs
  (meta/define zero)
  (meta/define succ (n))
  (meta/define m-lambda (name body))
  (meta/define app (fun arg))
  (meta/define ref (name))
  (meta/define lit (t))
  (meta/define emptyenv)
  (meta/define binding (name value env))
  (meta/define closure (lam env))
; functions
  (meta/define (env-lookup name env)
    [(name (binding name value env))  value]
    [(_    (binding name2 value env)) (env-lookup name env)])
  (meta/define (eval exp env)
    [((lit t)              _) t]
    [((ref name)           _) (env-lookup name env)]
    [((m-lambda name body) _) (closure (m-lambda name body) env)]
    [((app fun arg)        _) (m-apply (m-eval fun env)
                                       (m-eval arg env))])
  (meta/define (m-apply proc value)
    [((closure (m-lambda name body) env) _)
     (m-eval body (binding name value env))]))





|#
