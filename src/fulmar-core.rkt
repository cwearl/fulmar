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
(define/contract indent?
  (-> any/c boolean?)
  exact-nonnegative-integer?)
(provide indent?)
(define/contract line-length?
  (-> any/c boolean?)
  exact-positive-integer?)
(provide line-length?)
(define/contract (chunk-literal? g)
  (-> any/c boolean?)
  (or (symbol? g)
      (string? g)
      (char? g)
      (exact-nonnegative-integer? g)))
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
(define/contract (sc-body? g)
  (-> any/c boolean?)
  #true)
(provide sc-name? sc-body?)
(define/contract nekot-name?
  (-> any/c boolean?)
  symbol?)
(define/contract (nekot-body? g)
  (-> any/c boolean?)
  #true)
(provide nekot-name? nekot-body?)
(define/contract (environment-description? g)
  (-> any/c boolean?)
  (match g
    ['comment #true]
    ['macro #true]
    ['comment-macro #true]
    ['macro-comment #true]
    [_ #false]))
(define/contract (optional-environment-description? g)
  (-> any/c boolean?)
  (or (not g)
      (environment-description? g)))
(provide environment-description? optional-environment-description?)
(define/contract position?
  (-> any/c boolean?)
  exact-nonnegative-integer?)
(define/contract (optional-position? g)
  (-> any/c boolean?)
  (or (not g)
      (position? g)))
(provide position? optional-position?)
(define/contract written-line?
  (-> any/c boolean?)
  string?)
(define/contract (written-lines? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap written-line? g)))
(provide written-line? written-lines?)
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
(define/contract (chunk-list? g)
  (-> any/c boolean?)
  (or (chunk? g)
      (and (non-empty-list? g)
           (andmap chunk-list? g))))
(define (nullable-chunk-list? g)
  (-> any/c boolean?)
  (or (chunk? g)
      (null? g)
      (and (non-empty-list? g)
           (andmap nullable-chunk-list? g))))
(provide chunk? chunk-list? nullable-chunk-list?)

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
(define/contract (environment? g)
  (-> any/c boolean?)
  (or (empty-env? g)
      (environment-struct? g)))
(define/contract (optional-environment? g)
  (-> any/c boolean?)
  (or (not g)
      (environment? g)))
(provide/contract (struct environment-struct ([description environment-description?]
                                              [initial-position optional-position?])))
(provide environment? optional-environment?)

;comment block environment
(define/contract (comment-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'comment
               (environment-struct-description g))))
(define/contract (comment-env indent)
  (-> indent? comment-env?)
  (environment-struct 'comment indent))
(provide comment-env? comment-env)

;macro definition environment
(define/contract (macro-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'macro
               (environment-struct-description g))))
(define/contract macro-env
  macro-env?
  (environment-struct 'macro #false))
(provide macro-env? macro-env)

;commented macro defintion environment
(define/contract (comment-macro-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'comment-macro
               (environment-struct-description g))))
(define/contract (comment-macro-env indent)
  (-> indent? comment-macro-env?)
  (environment-struct 'comment-macro indent))
(provide comment-macro-env? comment-macro-env)

;macro definition with embedded comment block environment
(define/contract (macro-comment-env? g)
  (-> any/c boolean?)
  (and (environment-struct? g)
       (equal? 'macro-comment
               (environment-struct-description g))))
(define/contract (macro-comment-env indent)
  (-> indent? macro-comment-env?)
  (environment-struct 'macro-comment indent))
(provide macro-comment-env? macro-comment-env)

;build resulting environment of old and new environments
(define/contract (user-env? g)
  (-> any/c boolean?)
  (or (empty-env? g)
      (comment-env? g)
      (macro-env? g)))
(define/contract (possible-env? g)
  (-> any/c boolean?)
  (or (user-env? g)
      (comment-macro-env? g)
      (macro-comment-env? g)))
(define/contract (combine-env old new)
  (-> possible-env? user-env? possible-env?)
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
(provide user-env? possible-env? combine-env)

;context Structure
(struct context (indent line-length env) #:transparent)
(provide/contract (struct context ([indent indent?]
                                   [line-length line-length?]
                                   [env optional-environment?])))

;construct context
(define/contract (construct-context line-length)
  (-> line-length? context?)
  (context 0
           line-length
           empty-env))
(provide construct-context)

;new environment context
(define/contract (enter-env new-env obj)
  (-> user-env? context? context?)
  (struct-copy context obj [env (combine-env (context-env obj) new-env)]))
(provide enter-env)

;context accessors
(define/contract (context-description context)
  (-> context? optional-environment-description?)
  (let ([env (context-env context)])
    (if env
        (environment-struct-description env)
        #false)))
(define/contract (context-initial-position context)
  (-> context? optional-position?)
  (let* ([env (context-env context)])
    (if env
        (environment-struct-initial-position env)
        #false)))
(provide context-description
         context-initial-position)

;increase indent level context
(define/contract (reindent new-indent obj)
  (-> indent? context? context?)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))
(provide reindent)

;reset indent level context
(define/contract (reset-indent new-indent obj)
  (-> indent? context? context?)
  (struct-copy context obj [indent new-indent]))
(provide reset-indent)

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

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)
(provide/contract (struct nekot ([name nekot-name?]
                                 [body nekot-body?]
                                 [context context?])))

;basic-line struct
(define/contract (basic-line-string? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap (λ (g) (or (char? g)
                          (exact-nonnegative-integer? g)))
               g)))
(struct basic-line (string length) #:transparent)
(provide/contract (struct basic-line ([string basic-line-string?]
                                      [length natural-number/c])))
(provide basic-line-string?)

;logical pivot
(struct pivot (sections length) #:transparent)
(define/contract (logical-line-string? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap (λ (g) (or (char? g)
                          (exact-nonnegative-integer? g)
                          (pivot? g)))
               g)))
(provide logical-line-string?)

(define/contract (logical-line-string-list? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap logical-line-string? g)))
(provide logical-line-string-list?)

(provide/contract (struct pivot ([sections logical-line-string-list?]
                                 [length natural-number/c])))

;logical line
(struct logical-line (sections length) #:transparent)
(provide/contract (struct logical-line ([sections logical-line-string-list?]
                                        [length natural-number/c])))
