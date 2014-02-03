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

;macro definition environment
(define macro-env
  (environment 'macro #false))

;commented macro defintion environment
(define (comment-macro-env indent)
  (environment 'comment-macro indent))

;macro definition with embedded comment block environment
(define (macro-comment-env indent)
  (environment 'macro-comment indent))

;build resulting environment of old and new environments
(define (combine-env old new)
  (match/values 
   (values (environment-description old) 
           (environment-description new))
   [(_ 'macro-comment)
    (error "Cannot combine macro with comment environment with any environment")]
   [(_ 'comment-macro)
    (error "Cannot combine environment with macro environment with any environment")]
   [('empty _)
    new]
   [(_ 'empty)
    old]
   [((or 'comment 'comment-macro 'macro-comment) 'comment)
    old]
   [('comment 'macro)
    (comment-macro-env (environment-initial-position old))]
   [('macro 'comment)
    (macro-comment-env (environment-initial-position new))]
   [(_ _) (error "Incompatible environments combined; given: " old new)]))
  
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
  
  ;new macro definition
  (define (enter-macro-env context)
    (enter-env macro-env
               context))
  
  ;nekot Structure (reverse token - token spelled backwards)
  (struct nekot (name body context) #:transparent)