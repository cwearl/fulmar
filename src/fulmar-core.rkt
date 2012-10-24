#lang racket

;basic structures and contracts for fulmar
; fulmar is:
;  - a rich code generation/macro system for C++ that uses S-expressions
;  - the name of two species (Northern and Southern) of seabirds of the family Procellariidae

;basic helper functions
(define (flatten* . lst)
  (flatten lst))
(provide (contract-out (flatten* (->* () #:rest (listof any/c) (listof any/c)))))

;basic contracts
(define indent/c natural-number/c)
(define line-length/c exact-positive-integer?)
(provide indent/c line-length/c)
(define string-value/c (or/c symbol? string?))
(define string-list/c (or/c string-value/c
                            (non-empty-listof (recursive-contract string-list/c))))
(provide string-value/c string-list/c)
(define length-value/c (or/c natural-number/c string-value/c))
(define length-list/c (or/c length-value/c
                            (non-empty-listof (recursive-contract length-list/c))))
(provide length-value/c length-list/c)
(define sc-name/c symbol?)
(define sc-body/c any/c)
(provide sc-name/c sc-body/c)
(define nekot-name/c symbol?)
(define nekot-body/c any/c)
(provide nekot-name/c nekot-body/c)
(define environment-description/c (or/c 'comment 'macro 'comment-macro 'macro-comment))
(define optional-environment-description/c (or/c environment-description/c #false))
(provide environment-description/c optional-environment-description/c)
(define position/c natural-number/c)
(define optional-position/c (or/c position/c #false))
(provide position/c optional-position/c)
(define empty-env/c #false)
(provide empty-env/c)
(define written-line/c string?)
(define written-lines/c (non-empty-listof written-line/c))
(provide written-line/c written-lines/c)
(define mode/c (or/c 'normal 'immediate))
(define null/c (one-of/c null))
(provide mode/c null/c)

;chunk Contract
(struct s-chunk (name body) #:transparent)
(define s-chunk/c (struct/c s-chunk sc-name/c sc-body/c))
(define chunk/c (or/c s-chunk/c string-value/c natural-number/c))
(define chunk-list/c (or/c chunk/c
                           (non-empty-listof (recursive-contract chunk-list/c))))
(define nullable-chunk-list/c (or/c chunk/c
                                    null/c
                                    (non-empty-listof (recursive-contract nullable-chunk-list/c))))
(provide s-chunk/c
         chunk/c
         chunk-list/c
         nullable-chunk-list/c
         (contract-out (struct s-chunk ([name sc-name/c]
                                        [body sc-body/c]))))

;environment Structure
(struct environment (description initial-position) #:transparent)
(define environment/c (struct/c environment environment-description/c optional-position/c))
(define optional-environment/c (or/c environment/c #false))
(provide environment/c
         optional-environment/c
         (contract-out (struct environment ([description environment-description/c]
                                            [initial-position optional-position/c]))))

;empty environment
(define empty-env #false)
(define (empty-env? env)
  (not env))
(provide empty-env/c
         (contract-out (empty-env empty-env/c)
                       (empty-env? (-> any/c boolean?))))

;comment block environment
(define comment-env/c (struct/c environment 'comment position/c))
(define (comment-env indent)
  (environment 'comment indent))
(define (comment-env? env)
  (and (environment/c env)
       (eq? (environment-description env) 'comment)))
(provide comment-env/c
         (contract-out (comment-env (-> indent/c comment-env/c))
                       (comment-env? (-> any/c boolean?))))

;macro definition environment
(define macro-env/c (struct/c environment 'macro #false))
(define macro-env
  (environment 'macro #false))
(define (macro-env? env)
  (and (environment/c env)
       (eq? (environment-description env) 'macro)))
(provide macro-env/c
         (contract-out (macro-env macro-env/c)
                       (macro-env? (-> any/c boolean?))))

;commented macro defintion environment
(define comment-macro-env/c (struct/c environment 'comment-macro position/c))
(define (comment-macro-env indent)
  (environment 'comment-macro indent))
(define (comment-macro-env? env)
  (and (environment/c env)
       (eq? (environment-description env) 'comment-macro)))
(provide comment-macro-env/c
         (contract-out (comment-macro-env (-> indent/c comment-macro-env/c))
                       (comment-macro-env? (-> any/c boolean?))))

;macro definition with embedded comment block environment
(define macro-comment-env/c (struct/c environment 'macro-comment position/c))
(define (macro-comment-env indent)
  (environment 'macro-comment indent))
(define (macro-comment-env? env)
  (and (environment/c env)
       (eq? (environment-description env) 'macro-comment)))
(provide macro-comment-env/c
         (contract-out (macro-comment-env (-> indent/c macro-comment-env/c))
                       (macro-comment-env? (-> any/c boolean?))))

;build resulting environment of old and new environments
(define user-env/c (or/c empty-env/c comment-env/c macro-env/c))
(define possible-env/c (or/c user-env/c comment-macro-env/c macro-comment-env/c))
(define (combine-env old new)
  (cond [(empty-env? old) ;was in empty env
         new]
        [(empty-env? new) ;entering empty env
         old]
        [(and (or (comment-env? old)        ;was in a comment-type env (comment, macro-comment, or comment-macro)
                  (comment-macro-env? old)
                  (macro-comment-env? old))
              (comment-env? new))           ; and entering comment env
         old]
        [(and (comment-env? old) ;was in comment env
              (macro-env? new))  ; and entering macro env
         (comment-macro-env (environment-initial-position old))]
        [(and (macro-env? old)    ;was in macro env
              (comment-env? new)) ; and entering comment env
         (macro-comment-env (environment-initial-position new))]
        [else ;else error...
         (error "Incompatible environments combined; given: " old new)]))
(provide user-env/c
         possible-env/c
         (contract-out (combine-env (-> possible-env/c user-env/c possible-env/c))))

;context Structure
(struct context (indent line-length env) #:transparent)
(define context/c (struct/c context indent/c line-length/c optional-environment/c))
(provide context/c
         (contract-out (struct context ([indent indent/c]
                                        [line-length line-length/c]
                                        [env optional-environment/c]))))

;construct context
(define (construct-context line-length)
  (context 0
           line-length
           empty-env))
(provide (contract-out (construct-context (-> line-length/c context/c))))

;new environment context
(define (enter-env new-env obj)
  (struct-copy context obj [env (combine-env (context-env obj) new-env)]))
(provide (contract-out (enter-env (-> user-env/c context/c context/c))))

;context accessors
(define (context-description context)
  (let ([env (context-env context)])
    (if env
        (environment-description env)
        #false)))
(define (context-initial-position context)
  (let* ([env (context-env context)])
    (if env (environment-initial-position env) #false)))
(provide (contract-out (context-description (-> context/c optional-environment-description/c))
                       (context-initial-position (-> context/c optional-position/c))))

;increase indent level context
(define (reindent new-indent obj)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))
(provide (contract-out (reindent (-> indent/c context/c context/c))))

;reset indent level context
(define (reset-indent new-indent obj)
  (struct-copy context obj [indent new-indent]))
(provide (contract-out (reset-indent (-> indent/c context/c context/c))))

;new comment block
(define (enter-comment-env context)
  (enter-env (comment-env (context-indent context))
             context))
(provide (contract-out (enter-comment-env (-> context/c context/c))))

;new macro definition
(define (enter-macro-env context)
  (enter-env macro-env
             context))
(provide (contract-out (enter-macro-env (-> context/c context/c))))

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)
(define nekot/c (struct/c nekot nekot-name/c nekot-body/c context/c))
(provide nekot/c
         (contract-out (struct nekot ([name nekot-name/c]
                                      [body nekot-body/c]
                                      [context context/c]))))
