#lang racket

(require rackunit)
(require "../fulmar-core.rkt")
(require "../writer.rkt")

;unit tests for writer.rkt

(define/provide-test-suite test-unknown-nekot-type
  (test-case
   "Test unknown-nekot-type (writes unknown nekot)"
   (define test-context (construct-context initial-properties 80))
   (check-exn exn:fail? (λ () (unknown-nekot-type "FAIL" test-context '(""))))))

(define/provide-test-suite test-add-empty
  (test-case
   "Test add-empty (writes empty nekot)"
   (define test-context (construct-context initial-properties 80))
   (check-equal? (add-empty 'test test-context '("")) '(""))
   (check-equal? (add-empty 'test test-context '("a")) '("a"))
   (check-equal? (add-empty 'test test-context '("b" "a")) '("b" "a"))))

(define/provide-test-suite test-build-indentation
  (test-case
   "Test build-indentation (begins a new line correctly)"
   (define test-context-1 (context 0 80 #f initial-properties))
   (check-equal? (build-indentation test-context-1) "")
   (define test-context-2 (context 3 80 #f initial-properties))
   (check-equal? (build-indentation test-context-2) "   ")
   (define test-context-3 (context 6 80 (comment-env 4) initial-properties))
   (check-equal? (build-indentation test-context-1) "     * ")))

