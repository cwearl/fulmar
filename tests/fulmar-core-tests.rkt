#lang racket

(require rackunit)
(require "../fulmar-core.rkt")

;unit tests for fulmar-core.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b '(c d e) 'f) '(a b c d e f))))

(define/provide-test-suite test-empty-env
 (test-case
  "Test empty-env and emtpy-env?"
  (check-true (empty-env? empty-env))
  (check-false (empty-env? 'other))))

(define/provide-test-suite test-comment-env
  (test-case
   "Test comment-env and comment-env?"
   (check-true (comment-env? (comment-env 4)))
   (check-false (comment-env? empty-env))))

(define/provide-test-suite test-macro-env
  (test-case
   "Test macro-env and macro-env?"
   (check-true (macro-env? macro-env))
   (check-false (macro-env? empty-env))))

(define/provide-test-suite test-comment-macro-env
  (test-case
   "Test comment-macro-env and comment-macro-env?"
   (check-true (comment-macro-env? (comment-macro-env 4)))
   (check-false (comment-macro-env? empty-env))))

(define/provide-test-suite test-macro-comment-env
  (test-case
   "Test macro-comment-env and macro-comment-env?"
   (check-true (macro-comment-env? (macro-comment-env 4)))
   (check-false (macro-comment-env? empty-env))))

(define/provide-test-suite test-combine-env
  (test-case
   "Test procedure combine-env - combine _ into empty environment"
   (check-equal? (combine-env empty-env empty-env) empty-env)
   (check-equal? (combine-env empty-env (comment-env 4)) (comment-env 4))
   (check-equal? (combine-env empty-env macro-env) macro-env)
   (check-exn exn:fail:contract? (λ () (combine-env empty-env (comment-macro-env 4))))
   (check-exn exn:fail:contract? (λ () (combine-env empty-env (macro-comment-env 4)))))
  (test-case
   "Test procedure combine-env - combine _ into comment environment"
   (check-equal? (combine-env (comment-env 8) empty-env) (comment-env 8))
   (check-equal? (combine-env (comment-env 8) (comment-env 4)) (comment-env 8))
   (check-equal? (combine-env (comment-env 8) macro-env) (comment-macro-env 8))
   (check-exn exn:fail:contract? (λ () (combine-env (comment-env 8) (comment-macro-env 8))))
   (check-exn exn:fail:contract? (λ () (combine-env (comment-env 8) (macro-comment-env 8)))))
  (test-case
   "Test procedure combine-env - combine _ into macro environment"
   (check-equal? (combine-env macro-env empty-env) macro-env)
   (check-equal? (combine-env macro-env (comment-env 4)) (macro-comment-env 4))
   (check-exn exn:fail? (λ () (combine-env macro-env macro-env)))
   (check-exn exn:fail:contract? (λ () (combine-env macro-env (comment-macro-env 4))))
   (check-exn exn:fail:contract? (λ () (combine-env macro-env (macro-comment-env 4)))))
  (test-case
   "Test procedure combine-env - combine _ into comment-macro environment"
   (check-equal? (combine-env (comment-macro-env 8) empty-env) (comment-macro-env 8))
   (check-equal? (combine-env (comment-macro-env 8) (comment-env 4)) (comment-macro-env 8))
   (check-exn exn:fail? (λ () (combine-env (comment-macro-env 8) macro-env)))
   (check-exn exn:fail:contract? (λ () (combine-env (comment-macro-env 8) (comment-macro-env 4))))
   (check-exn exn:fail:contract? (λ () (combine-env (comment-macro-env 8) (macro-comment-env 4)))))
  (test-case
   "Test procedure combine-env - combine _ into macro-comment environment"
   (check-equal? (combine-env (macro-comment-env 8) empty-env) (macro-comment-env 8))
   (check-equal? (combine-env (macro-comment-env 8) (comment-env 4)) (macro-comment-env 8))
   (check-exn exn:fail? (λ () (combine-env (macro-comment-env 8) macro-env)))
   (check-exn exn:fail:contract? (λ () (combine-env (macro-comment-env 8) (comment-macro-env 4))))
   (check-exn exn:fail:contract? (λ () (combine-env (macro-comment-env 8) (macro-comment-env 4))))))

(define/provide-test-suite test-construct-context
  (test-case
   "Test construct-context"
   (check-equal? (construct-context 80)
                 (context 0 80 empty-env))
   (check-equal? (construct-context 80)
                 (context 0 80 empty-env))))

(define/provide-test-suite test-enter-env
  (test-case
   "Test enter-env"
   (define test-context-1 (construct-context 80))
   (check-equal? (enter-env (comment-env 4) test-context-1) (context 0 80 (comment-env 4)))
   (check-equal? (enter-env macro-env test-context-1) (context 0 80 macro-env))))

(define/provide-test-suite test-context-accessors
  (test-case
   "Test non-standard context accessors - empty environment"
   (define test-context (construct-context 80))
   (check-false (context-description test-context))
   (check-false (context-initial-position test-context)))
  (test-case
   "Test non-standard context accessors - comment environment"
   (define comment-context (enter-env (comment-env 4) (construct-context 80)))
   (check-eq? (context-description comment-context) 'comment)
   (check-eq? (context-initial-position comment-context) 4))
  (test-case
   "Test non-standard context accessors - macro environment"
   (define macro-context (enter-env macro-env (construct-context 80)))
   (check-eq? (context-description macro-context) 'macro)
   (check-false (context-initial-position macro-context)))
  (test-case
   "Test non-standard context accessors - comment-macro environment"
   (define comment-macro-context (enter-env macro-env (enter-env (comment-env 4) (construct-context 80))))
   (check-eq? (context-description comment-macro-context) 'comment-macro)
   (check-eq? (context-initial-position comment-macro-context) 4))
  (test-case
   "Test non-standard context accessors - macro-comment environment"
   (define macro-comment-context (enter-env (comment-env 4) (enter-env macro-env (construct-context 80))))
   (check-eq? (context-description macro-comment-context) 'macro-comment)
   (check-eq? (context-initial-position macro-comment-context) 4)))

(define/provide-test-suite test-reindent
  (test-case
   "Test reindent"
   (define test-context (construct-context 80))
   (check-equal? (reindent 4 test-context) (context 4 80 empty-env))
   (check-equal? (reindent 2 (reindent 5 test-context)) (context 7 80 empty-env))))

(define/provide-test-suite test-enter-comment
  (test-case
   "Test enter-comment-env"
   (define test-context-1 (construct-context 80))
   (define test-context-2 (context 4 80 (comment-env 4)))
   (define test-context-3 (context 6 80 (comment-env 4)))
   (define test-context-4 (context 0 80 macro-env))
   (define test-context-5 (context 0 80 (macro-comment-env 0)))
   (check-equal? (enter-comment-env (reindent 4 test-context-1)) test-context-2)
   (check-equal? (enter-comment-env (reindent 2 test-context-2)) test-context-3)
   (check-equal? (enter-comment-env test-context-4) test-context-5)))

(define/provide-test-suite test-enter-macro
  (test-case
   "Test enter-macro-env"
   (define chunk (error-chunk "Testing - tried to apply a chunk you shouldn't have."))
   (define test-context-1 (construct-context 80))
   (define test-context-2 (context 0 80 macro-env))
   (define test-context-3 (context 4 80 (comment-env 4)))
   (define test-context-4 (context 4 80 (comment-macro-env 4)))
   (check-equal? (enter-macro-env test-context-1) test-context-2)
   (check-exn exn:fail? (λ () (enter-macro-env test-context-2)))
   (check-equal? (enter-macro-env test-context-3) test-context-4)))
;   (check-equal? (enter-macro-env test-context-3) test-context-3))) ;Causes an intentional error - this line is used to test the testing framework
