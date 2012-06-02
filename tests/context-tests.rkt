#lang racket

(require rackunit)
(require "../src/environment.rkt")
(require "../src/context.rkt")

;unit tests for environment.rkt

(define/provide-test-suite test-context-constructor-and-predicate
  (test-case
   "Test context?"
   (check-true (context? (initial-context 80)))
   (check-false (context? 'other))))

(define/provide-test-suite test-context-accessors
  (test-case
   "Test non-standard context accessors - empty environment"
   (define test-context (initial-context 80))
   (check-equal? (context-indent test-context) 0)
   (check-equal? (context-line-length test-context) 80)
   (check-false (context-env test-context))
   (check-false (context-env-description test-context))
   (check-false (context-comment-indent test-context)))
  (test-case
   "Test non-standard context accessors - comment environment"
   (define test-context (enter-comment-env (increase-indent 4 (initial-context 80))))
   (check-equal? (context-indent test-context) 4)
   (check-equal? (context-line-length test-context) 80)
   (check-equal? (context-env test-context) (comment-env 4))
   (check-equal? (context-env-description test-context) 'comment)
   (check-equal? (context-comment-indent test-context) 4)))

(define/provide-test-suite test-increase-indent
  (test-case
   "Test increase-indent"
   (define test-context (initial-context 80))
   (check-equal? (context-indent (increase-indent 4 test-context)) 4)
   (check-equal? (context-indent (increase-indent 2 (increase-indent 5 test-context))) 7)))

(define/provide-test-suite test-reset-indent
  (test-case
   "Test reset-indent"
   (define test-context (initial-context 80))
   (check-equal? (context-indent (reset-indent 4 test-context)) 4)
   (check-equal? (context-indent (reset-indent 5 (reset-indent 2 test-context))) 5)))

(define/provide-test-suite test-enter-comment-env
  (test-case
   "Test enter-comment-env"
   (check-true (comment-env? (context-env (enter-comment-env (initial-context 80)))))
   (check-true (comment-env? (context-env (enter-comment-env (increase-indent 4 (initial-context 80))))))
   (check-true (comment-env? (context-env (increase-indent 3 (enter-comment-env (increase-indent 4 (initial-context 80)))))))
   (check-true (comment-env? (context-env (enter-comment-env (increase-indent 3 (enter-comment-env (increase-indent 4 (initial-context 80))))))))
   (check-false (comment-macro-env? (context-env (enter-comment-env (enter-macro-env (initial-context 80))))))
   (check-false (macro-comment-env? (context-env (enter-macro-env (enter-comment-env (initial-context 80))))))))

(define/provide-test-suite test-enter-macro-env
  (test-case
   "Test enter-macro-env"
   (check-true (macro-env? (context-env (enter-macro-env (initial-context 80)))))
   (check-true (macro-env? (context-env (enter-macro-env (increase-indent 4 (initial-context 80))))))
   (check-true (macro-env? (context-env (increase-indent 3 (enter-macro-env (increase-indent 4 (initial-context 80)))))))
   (check-exn exn:fail? (λ () (enter-macro-env (increase-indent 3 (enter-macro-env (increase-indent 4 (initial-context 80)))))))
   (check-false (macro-comment-env? (context-env (enter-macro-env (enter-comment-env (initial-context 80))))))
   (check-false (comment-macro-env? (context-env (enter-comment-env (enter-macro-env (initial-context 80))))))))

