#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")

;unit tests for fulmar-core.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b '(c d e) 'f) '(a b c d e f))))

(define/provide-test-suite test-combine-env
  (test-case
   "Test procedure combine-env - combine _ into empty environment"
   (check-equal? (combine-env (empty-env) (empty-env)) (empty-env))
   (check-equal? (combine-env (empty-env) (comment-env 4)) (comment-env 4)))
  (test-case
   "Test procedure combine-env - combine _ into comment environment"
   (check-equal? (combine-env (comment-env 8) (empty-env)) (comment-env 8))
   (check-equal? (combine-env (comment-env 8) (comment-env 4)) (comment-env 8))))

(define/provide-test-suite test-construct-context
  (test-case
   "Test construct-context"
   (check-equal? (construct-context 80)
                 (context 0 80 (empty-env)))
   (check-equal? (construct-context 80)
                 (context 0 80 (empty-env)))))

(define/provide-test-suite test-enter-env
  (test-case
   "Test enter-env"
   (define test-context-1 (construct-context 80))
   (check-equal? (enter-env (comment-env 4) test-context-1) (context 0 80 (comment-env 4)))))

(define/provide-test-suite test-reindent
  (test-case
   "Test reindent"
   (define test-context (construct-context 80))
   (check-equal? (reindent 4 test-context) (context 4 80 (empty-env)))
   (check-equal? (reindent 2 (reindent 5 test-context)) (context 7 80 (empty-env)))))


(define/provide-test-suite test-enter-comment
  (test-case
   "Test enter-comment-env"
   (define test-context-1 (construct-context 80))
   (define test-context-2 (context 4 80 (comment-env 4)))
   (define test-context-3 (context 6 80 (comment-env 4)))
   (check-equal? (enter-comment-env (reindent 4 test-context-1)) test-context-2)
   (check-equal? (enter-comment-env (reindent 2 test-context-2)) test-context-3)))

