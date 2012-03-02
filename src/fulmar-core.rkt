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
(provide/contract (struct s-chunk ([name sc-name/c]
                                   [body sc-body/c])))
(provide s-chunk/c chunk/c chunk-list/c nullable-chunk-list/c)

;environment Structure
(struct environment (description initial-position) #:transparent)
(define environment/c (struct/c environment environment-description/c optional-position/c))
(define optional-environment/c (or/c environment/c #false))
(provide/contract (struct environment ([description environment-description/c]
                                       [initial-position optional-position/c])))
(provide environment/c optional-environment/c)

;empty environment
(define/contract empty-env empty-env/c #false)
(define/contract (empty-env? env)
  (-> any/c boolean?)
  (not env))
(provide empty-env/c empty-env empty-env?)

;comment block environment
(define comment-env/c (struct/c environment 'comment position/c))
(define/contract (comment-env indent)
  (-> indent/c comment-env/c)
  (environment 'comment indent))
(define/contract (comment-env? env)
  (-> any/c boolean?)
  (and (environment/c env)
       (eq? (environment-description env) 'comment)))
(provide comment-env/c comment-env comment-env?)

;macro definition environment
(define macro-env/c (struct/c environment 'macro #false))
(define/contract macro-env
  macro-env/c
  (environment 'macro #false))
(define/contract (macro-env? env)
  (-> any/c boolean?)
  (and (environment/c env)
       (eq? (environment-description env) 'macro)))
(provide macro-env/c macro-env macro-env?)

;commented macro defintion environment
(define comment-macro-env/c (struct/c environment 'comment-macro position/c))
(define/contract (comment-macro-env indent)
  (-> indent/c comment-macro-env/c)
  (environment 'comment-macro indent))
(define/contract (comment-macro-env? env)
  (-> any/c boolean?)
  (and (environment/c env)
       (eq? (environment-description env) 'comment-macro)))
(provide comment-macro-env/c comment-macro-env comment-macro-env?)

;macro definition with embedded comment block environment
(define macro-comment-env/c (struct/c environment 'macro-comment position/c))
(define/contract (macro-comment-env indent)
  (-> indent/c macro-comment-env/c)
  (environment 'macro-comment indent))
(define/contract (macro-comment-env? env)
  (-> any/c boolean?)
  (and (environment/c env)
       (eq? (environment-description env) 'macro-comment)))
(provide macro-comment-env/c macro-comment-env macro-comment-env?)

;build resulting environment of old and new environments
(define user-env/c (or/c empty-env/c comment-env/c macro-env/c))
(define possible-env/c (or/c user-env/c comment-macro-env/c macro-comment-env/c))
(define/contract (combine-env old new)
  (-> possible-env/c user-env/c possible-env/c)
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
(provide user-env/c possible-env/c combine-env)

;context Structure
(struct context (indent line-length env) #:transparent)
(define context/c (struct/c context indent/c line-length/c optional-environment/c))
(provide/contract (struct context ([indent indent/c]
                                   [line-length line-length/c]
                                   [env optional-environment/c])))
(provide context/c)

;construct context
(define/contract (construct-context line-length)
  (-> line-length/c context/c)
  (context 0
           line-length
           empty-env))
(provide construct-context)

;new environment context
(define/contract (enter-env new-env obj)
  (-> user-env/c context/c context/c)
  (struct-copy context obj [env (combine-env (context-env obj) new-env)]))
(provide enter-env)

;context accessors
(define/contract (context-description context)
  (-> context/c optional-environment-description/c)
  (let ([env (context-env context)])
    (if env
        (environment-description env)
        #false)))
(define/contract (context-initial-position context)
  (-> context/c optional-position/c)
  (let* ([env (context-env context)])
    (if env (environment-initial-position env) #false)))
(provide context-description
         context-initial-position)

;increase indent level context
(define/contract (reindent new-indent obj)
  (-> indent/c context/c context/c)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))
(provide reindent)

;reset indent level context
(define/contract (reset-indent new-indent obj)
  (-> indent/c context/c context/c)
  (struct-copy context obj [indent new-indent]))
(provide reset-indent)

;new comment block
(define/contract (enter-comment-env context)
  (-> context/c context/c)
  (enter-env (comment-env (context-indent context))
             context))
(provide enter-comment-env)

;new macro definition
(define/contract (enter-macro-env context)
  (-> context/c context/c)
  (enter-env macro-env
             context))
(provide enter-macro-env)

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)
(define nekot/c (struct/c nekot nekot-name/c nekot-body/c context/c))
(provide/contract (struct nekot ([name nekot-name/c]
                                 [body nekot-body/c]
                                 [context context/c])))
(provide nekot/c)

;Logical line
(define line/c (listof (or/c char? natural-number/c)))
(define ll/c (cons/c 'll (cons/c line/c (cons/c natural-number/c null?))))
(define/contract (ll line length)
  (-> line/c natural-number/c ll/c)
  (list 'll line length))
(define/contract ll-line
  (-> ll/c line/c)
  second)
(define/contract ll-length
  (-> ll/c natural-number/c)
  third)

;Logical pivot
;(struct pivot (lines length) #:transparent)
;(define pivot/c (struct/c pivot (listof (or/c ll/c (recursive-contract pivot/c))) natural-number/c))
;(provide/contract (struct pivot ([lines (listof (or/c ll/c pivot/c))]
;                                 [length natural-number/c])))
;(provide pivot/c)
