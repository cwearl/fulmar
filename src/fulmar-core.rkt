#lang racket

;basic structures and contracts for fulmar
; fulmar is:
;  - a rich code generation/macro system for C++ that uses S-expressions
;  - the name of two species (Northern and Southern) of seabirds of the family Procellariidae

;basic helper functions
(define/contract (flatten* . lst)
  (->* () #:rest (listof any/c) (listof any/c))
  (flatten lst))
(provide flatten*)
(define/contract (non-empty-list? lst)
  (-> any/c boolean?)
  (and (list? lst)
       (not (null? lst))))
(provide non-empty-list?)

;basic contracts
(define/contract (string-type? g)
  (-> any/c boolean?)
  (or (string? g)
      (symbol? g)))
(provide string-type?)
(define/contract (string-list? lst)
  (-> any/c boolean?)
  (or (string-type? lst)
      (and (non-empty-list? lst)
           (andmap string-list? lst))))
(provide string-list?)
(define/contract indent?
  (-> any/c boolean?)
  exact-nonnegative-integer?)
(provide indent?)
(define/contract (length-type? g)
  (-> any/c boolean?)
  (or (indent? g)
      (string-type? g)))
(provide length-type?)
(define/contract (length-list? lst)
  (-> any/c boolean?)
  (or (length-type? lst)
      (and (non-empty-list? lst)
           (andmap length-list? lst))))
(provide length-list?)
(define/contract line-length?
  (-> any/c boolean?)
  exact-positive-integer?)
(provide line-length?)
(define/contract (chunk-literal? g)
  (-> any/c boolean?)
  (or (symbol? g)
      (string? g)
      (char? g)
      (indent? g)))
(provide chunk-literal?)
(define/contract (chunk-literal-list? g)
  (-> any/c boolean?)
  (or (chunk-literal? g)
      (and (non-empty-list? g)
           (andmap chunk-literal-list? g))))
(provide chunk-literal-list?)
(define/contract sc-name?
  (-> any/c boolean?)
  symbol?)
(provide sc-name?)
(define/contract (sc-body? g)
  (-> any/c boolean?)
  #true)
(provide sc-body?)
(define/contract (environment-description? g)
  (-> any/c boolean?)
  (match g
    ['comment #true]
    ['macro #true]
    ['comment-macro #true]
    ['macro-comment #true]
    [_ #false]))
(provide environment-description?)
(define/contract (optional-environment-description? g)
  (-> any/c boolean?)
  (or (not g)
      (environment-description? g)))
(provide optional-environment-description?)
(define/contract position?
  (-> any/c boolean?)
  indent?)
(provide position?)
(define/contract (optional-position? g)
  (-> any/c boolean?)
  (or (not g)
      (position? g)))
(provide optional-position?)
(define/contract written-line?
  (-> any/c boolean?)
  string?)
(provide written-line?)
(define/contract (written-lines? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap written-line? g)))
(provide written-lines?)
(define (mode? g)
  (-> any/c boolean?)
  (match g
    ['normal #true]
    ['immediate #true]
    [_ #false]))
(provide mode?)

;chunk-struct
(struct chunk-struct (name body) #:transparent)
(provide/contract (struct chunk-struct ([name sc-name?]
                                        [body sc-body?])))

;general chunk definitions
(define/contract (chunk? c)
  (-> any/c boolean?)
  (or (chunk-struct? c)
      (chunk-literal? c)))
(provide chunk?)
(define/contract (chunk-list? g)
  (-> any/c boolean?)
  (or (chunk? g)
      (and (non-empty-list? g)
           (andmap chunk-list? g))))
(provide chunk-list?)
(define (nullable-chunk-list? g)
  (-> any/c boolean?)
  (or (chunk? g)
      (null? g)
      (and (non-empty-list? g)
           (andmap nullable-chunk-list? g))))
(provide nullable-chunk-list?)

;empty environment
(define/contract empty-env?
  (-> any/c boolean?)
  not)
(provide empty-env?)
(define/contract empty-env
  empty-env?
  #false)
(provide empty-env? empty-env)

;environment Structure
(struct environment-struct (description initial-position) #:transparent)
;(provide/contract (struct environment-struct ([description environment-description?]
;                                              [initial-position optional-position?])))

;comment block environment
(define/contract (comment-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'comment
               (environment-struct-description g))))
(provide comment-env?)
(define/contract (comment-env indent)
  (-> indent? comment-env?)
  (environment-struct 'comment indent))
(provide comment-env)

;macro definition environment
(define/contract (macro-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'macro
               (environment-struct-description g))))
(provide macro-env?)
(define/contract macro-env
  macro-env?
  (environment-struct 'macro #false))
(provide macro-env)

;commented macro defintion environment
(define/contract (comment-macro-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'comment-macro
               (environment-struct-description g))))
(provide comment-macro-env?)
(define/contract (comment-macro-env indent)
  (-> indent? comment-macro-env?)
  (environment-struct 'comment-macro indent))
(provide comment-macro-env)

;macro definition with embedded comment block environment
(define/contract (macro-comment-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'macro-comment
               (environment-struct-description g))))
(provide macro-comment-env?)
(define/contract (macro-comment-env indent)
  (-> indent? macro-comment-env?)
  (environment-struct 'macro-comment indent))
(provide macro-comment-env)

;predicate for environment
(define/contract (environment? g)
  (-> any/c boolean?)
  (or (empty-env? g)
      (comment-env? g)
      (macro-env? g)
      (comment-macro-env? g)
      (macro-comment-env? g)))
(provide environment?)

;predicate for user-definable environment
(define/contract (user-env? g)
  (-> any/c boolean?)
  (or (empty-env? g)
      (comment-env? g)
      (macro-env? g)))
(provide user-env?)

;combine two environments
(define/contract (combine-env old new)
  (-> environment? user-env? environment?)
  (cond [(empty-env? old) ;was in empty env
         new]
        [(empty-env? new) ;entering empty env
         old]
        [(and (comment-env? new)            ;entering: comment env
              (or (comment-env? old)        ;in: comment-type env (comment, macro-comment, or comment-macro)
                  (comment-macro-env? old)
                  (macro-comment-env? old)))
         old]
        [(and (comment-env? new)            ;entering: comment env
              (macro-env? old))             ;in: macro env
         (macro-comment-env (environment-struct-initial-position new))]
        [(and (macro-env? new)              ;entering: macro env
              (comment-env? old))           ;in: comment env
         (comment-macro-env (environment-struct-initial-position old))]
        [else ;else error...
         (error "Incompatible environments combined; given: " old new)]))
(provide combine-env)

;context Structure
(struct context-struct (indent line-length env) #:transparent)
(provide/contract (struct context-struct ([indent indent?]
                                          [line-length line-length?]
                                          [env environment?])))
(define/contract (context? context)
  (-> any/c boolean?)
  (and (context-struct? context)
       (indent? (context-struct-indent context))
       (line-length? (context-struct-line-length context))
       (environment? (context-struct-env context))))
(provide context?)

;initial context
(define/contract (initial-context line-length)
  (-> line-length? context-struct?)
  (context-struct 0
                  line-length
                  empty-env))
(provide initial-context)

;new environment context
(define/contract (enter-env new-env context)
  (-> user-env? context-struct? context-struct?)
  (struct-copy context-struct context [env (combine-env (context-struct-env context) new-env)]))
(provide enter-env)

;context accessors
(define/contract (context-indent context)
  (-> context-struct? indent?)
  (context-struct-indent context))
(provide context-indent)
(define/contract (context-line-length context)
  (-> context-struct? line-length?)
  (context-struct-line-length context))
(provide context-line-length)
(define/contract (context-env context)
  (-> context-struct? environment?)
  (context-struct-env context))
(provide context-env)
(define/contract (context-description context)
  (-> context? optional-environment-description?)
  (let ([env (context-env context)])
    (if env
        (environment-struct-description env)
        #false)))
(provide context-description)
(define/contract (context-initial-position context)
  (-> context? optional-position?)
  (let* ([env (context-env context)])
    (if env
        (environment-struct-initial-position env)
        #false)))
(provide context-initial-position)

;reset indent level context
(define/contract (reset-indent new-indent context)
  (-> indent? context? context?)
  (struct-copy context-struct context [indent new-indent]))
(provide reset-indent)

;increase indent level context
(define/contract (reindent additional-indent context)
  (-> indent? context? context?)
  (reset-indent (+ additional-indent
                   (context-indent context))
                context))
(provide reindent)

;new comment block
(define/contract (enter-comment-env context)
  (-> context? context?)
  (enter-env (comment-env (context-indent context))
             context))
(provide enter-comment-env)

;new macro definition
(define/contract (enter-macro-env context)
  (-> context? context?)
  (enter-env macro-env
             context))
(provide enter-macro-env)
