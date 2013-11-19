#lang racket

;basic structures and contracts for fulmar
; fulmar is:
;  - a rich code generation/macro system for C++ that uses S-expressions
;  - the name of two species (Northern and Southern) of seabirds of the family Procellariidae

;basic helper functions
(define (flatten* . lst)
  (flatten lst))
(provide flatten*)

;basic contracts

;structure chunk definition
(struct s-chunk (name body) #:transparent)
(provide (struct-out s-chunk))

;chunk predicate
(define (chunk? chunk)
  (or (string? chunk)
      (symbol? chunk)
      (exact-nonnegative-integer? chunk)
      (s-chunk? chunk)))
(provide chunk?)

;environment Structure
(struct environment (description initial-position) #:transparent)
(define (environment-description? d)
  (or (eq? d 'comment)
      (eq? d 'macro)
      (eq? d 'comment-macro)
      (eq? d 'macro-comment)))
(define (optional-position? p)
  (or (and (exact-integer? p)
           (<= 0 p))
      (not p)))
(define (environment?? e)
  (and (environment? e)
       (environment-description? (environment-description e))
       (optional-position? (environment-initial-position e))))
(provide (struct-out environment))

;empty environment
(define empty-env #false)
(define (empty-env? env)
  (not env))
(provide empty-env empty-env?)

;comment block environment
(define (comment-env indent)
  (environment 'comment indent))
(define (comment-env? env)
  (and (environment?? env)
       (eq? (environment-description env) 'comment)))
(provide comment-env comment-env?)

;macro definition environment
(define macro-env
  (environment 'macro #false))
(define (macro-env? env)
  (and (environment?? env)
       (eq? (environment-description env) 'macro)))
(provide macro-env macro-env?)

;commented macro defintion environment
(define (comment-macro-env indent)
  (environment 'comment-macro indent))
(define (comment-macro-env? env)
  (and (environment?? env)
       (eq? (environment-description env) 'comment-macro)))
(provide comment-macro-env comment-macro-env?)

;macro definition with embedded comment block environment
(define (macro-comment-env indent)
  (environment 'macro-comment indent))
(define (macro-comment-env? env)
  (and (environment?? env)
       (eq? (environment-description env) 'macro-comment)))
(provide macro-comment-env macro-comment-env?)

;build resulting environment of old and new environments
(define (combine-env old new)
  (cond [(macro-comment-env? new)
         (error "Cannot combine macro with comment environment with any environment")]
        [(comment-macro-env? new)
         (error "Cannot combine environment with macro environment with any environment")]
        [(empty-env? old) ;was in empty env
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
(provide combine-env)

;context Structure
(struct context (indent line-length env) #:transparent)
(provide (struct-out context))

;construct context
(define (construct-context line-length)
  (context 0
           line-length
           empty-env))
(provide construct-context)

;new environment context
(define (enter-env new-env obj)
  (struct-copy context obj [env (combine-env (context-env obj) new-env)]))
(provide enter-env)

;context accessors
(define (context-description context)
  (let ([env (context-env context)])
    (if env
        (environment-description env)
        #false)))
(define (context-initial-position context)
  (let* ([env (context-env context)])
    (if env (environment-initial-position env) #false)))
(provide context-description
         context-initial-position)

;increase indent level context
(define (reindent new-indent obj)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))
(provide reindent)

;reset indent level context
(define (reset-indent new-indent obj)
  (struct-copy context obj [indent new-indent]))
(provide reset-indent)

;new comment block
(define (enter-comment-env context)
  (enter-env (comment-env (context-indent context))
             context))
(provide enter-comment-env)

;new macro definition
(define (enter-macro-env context)
  (enter-env macro-env
             context))
(provide enter-macro-env)

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)
(provide (struct-out nekot))
