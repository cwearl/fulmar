#lang racket

;basic structures and contracts for fulmar
; fulmar is:
;  - a rich code generation/macro system for C++ that uses S-expressions
;  - the name of two species (Northern and Southern) of seabirds of the family Procellariidae

;basic contracts
(define (context-delayed? x) (context? x))
;(provide context-delayed?)
(define indent/c natural-number/c)
(define line-length/c exact-positive-integer?)
(provide indent/c line-length/c)
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

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)
(define nekot/c (struct/c nekot nekot-name/c nekot-body/c context-delayed?))
(provide/contract (struct nekot ([name nekot-name/c]
                                 [body nekot-body/c]
                                 [context context-delayed?])))
(provide nekot/c)

;chunk Contract
(define chunk/c (-> context-delayed? nekot/c))
(provide chunk/c)

;error chunk
; this chunk raises an error when applied - this chunk is used for testing and filing in stubs/empty parameters
;   hence, it is in basic definitions section
(define/contract (error-chunk . error_content)
  (->* () #:rest any/c chunk/c)
  (λ (context)
    (apply error error_content)))
(provide error-chunk)

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
    (if env (environment-initial-position (context-env context)) #false)))
(provide context-description
         context-initial-position)

;new indent level context
(define/contract (reindent new-indent obj)
  (-> indent/c context/c context/c)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))
(provide reindent)

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
