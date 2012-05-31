#lang racket

(require rackunit)
(require "../src/environment.rkt")

;unit tests for environment.rkt

(define/provide-test-suite test-env-description?
  (test-case
   "Test env-description?"
   (check-true (env-description? 'comment))
   (check-true (env-description? 'macro))
   (check-true (env-description? 'comment-macro))
   (check-true (env-description? 'macro-comment))
   (check-false (env-description? 'asdf))))

(define/provide-test-suite test-optional-env-description?
  (test-case
   "Test optional-env-description?"
   (check-true (optional-env-description? #false))
   (check-true (optional-env-description? 'comment))
   (check-true (optional-env-description? 'macro))
   (check-true (optional-env-description? 'comment-macro))
   (check-true (optional-env-description? 'macro-comment))
   (check-false (optional-env-description? 'asdf))))

(define/provide-test-suite test-env-type-constructors-and-predicates
  (test-case
   "Test empty-env?"
   (check-true (empty-env? empty-env))
   (check-false (empty-env? (comment-env 0)))
   (check-false (empty-env? macro-env))
   (check-false (empty-env? (comment-macro-env 0)))
   (check-false (empty-env? (macro-comment-env 0))))
  (test-case
   "Test comment-env?"
   (check-false (comment-env? empty-env))
   (check-true (comment-env? (comment-env 0)))
   (check-false (comment-env? macro-env))
   (check-false (comment-env? (comment-macro-env 0)))
   (check-false (comment-env? (macro-comment-env 0))))
  (test-case
   "Test macro-env?"
   (check-false (macro-env? empty-env))
   (check-false (macro-env? (comment-env 0)))
   (check-true (macro-env? macro-env))
   (check-false (macro-env? (comment-macro-env 0)))
   (check-false (macro-env? (macro-comment-env 0))))
  (test-case
   "Test comment-macro-env?"
   (check-false (comment-macro-env? empty-env))
   (check-false (comment-macro-env? (comment-env 0)))
   (check-false (comment-macro-env? macro-env))
   (check-true (comment-macro-env? (comment-macro-env 0)))
   (check-false (comment-macro-env? (macro-comment-env 0))))
  (test-case
   "Test macro-comment-env?"
   (check-false (macro-comment-env? empty-env))
   (check-false (macro-comment-env? (comment-env 0)))
   (check-false (macro-comment-env? macro-env))
   (check-false (macro-comment-env? (comment-macro-env 0)))
   (check-true (macro-comment-env? (macro-comment-env 0)))))

(define/provide-test-suite test-env?
  (test-case
   "Test env?"
   (check-true (env? empty-env))
   (check-true (env? (comment-env 0)))
   (check-true (env? macro-env))
   (check-true (env? (comment-macro-env 0)))
   (check-true (env? (macro-comment-env 0)))
   (check-false (env? 'other))))

(define/provide-test-suite test-user-env?
  (test-case
   "Test user-env?"
   (check-true (user-env? empty-env))
   (check-true (user-env? (comment-env 0)))
   (check-true (user-env? macro-env))
   (check-false (user-env? (comment-macro-env 0)))
   (check-false (user-env? (macro-comment-env 0)))
   (check-false (user-env? 'other))))

(define/provide-test-suite test-env-description
  (test-case
   "Test env-description"
   (check-false (env-description empty-env))
   (check-equal? (env-description (comment-env 0)) 'comment)
   (check-equal? (env-description macro-env) 'macro)
   (check-equal? (env-description (comment-macro-env 0)) 'comment-macro)
   (check-equal? (env-description (macro-comment-env 0)) 'macro-comment)))

(define/provide-test-suite test-env-comment-indent
  (test-case
   "Test env-comment-indent"
   (check-equal? (env-comment-indent (comment-env 0)) 0)
   (check-equal? (env-comment-indent (comment-macro-env 4)) 4)
   (check-equal? (env-comment-indent (macro-comment-env 8)) 8)
   (check-false (env-comment-indent empty-env))
   (check-false (env-comment-indent macro-env))))

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
