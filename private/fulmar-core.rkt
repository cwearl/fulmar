#lang racket

(provide (all-defined-out))

;basic structures and contracts for fulmar
; fulmar is:
;  - a rich code generation/macro system for C++ that uses S-expressions
;  - the name of two species (Northern and Southern) of seabirds of the family Procellariidae

;basic helper functions
(define (flatten* . lst)
  (flatten lst))

;structure chunk definition
(struct s-chunk (name body) #:transparent)

;environment Structure
(struct environment (description initial-position) #:transparent)

;empty environment
(define (empty-env)
  (environment 'empty 0))

;comment block environment
(define (comment-env indent)
  (environment 'comment indent))

;build resulting environment of old and new environments
(define (combine-env old new)
  (match/values 
   (values (environment-description old) 
           (environment-description new))
   [('empty _) new]
   [(_ 'empty) old]
   [('comment 'comment) old]))

;context Structure
(struct context (indent line-length env) #:transparent)

;construct context
(define (construct-context line-length)
  (context 0
           line-length
           (empty-env)))

;new environment context
(define (enter-env new-env obj)
  (struct-copy context obj [env (combine-env (context-env obj) new-env)]))

(define (context-initial-position context)
  (let* ([env (context-env context)])
    (if env (environment-initial-position env) #false)))

;increase indent level context
(define (reindent new-indent obj)
  (struct-copy context obj [indent (+ new-indent
                                      (context-indent obj))]))

;new comment block
(define (enter-comment-env context)
  (enter-env (comment-env (context-indent context))
             context))

;nekot Structure (reverse token - token spelled backwards)
(struct nekot (name body context) #:transparent)