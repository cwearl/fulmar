#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "fulmar-core-tests.rkt")
(require "writer-tests.rkt")

;sequentially check if any tests fail
; returns true if all given tests pass
;         false if any of the given tests fail
; prints out summary as well
(define/contract (sequential-run-tests verbosity . tests)
  (->* (symbol?) #:rest (listof test-suite?) boolean?)
  (andmap (λ (test) (= 0 (run-tests test verbosity)))
          tests))

;check if any tests fail with text-based output
; returns true if all given tests pass
;         false if any of the given tests fail
; prints out summary as well
(define/contract (run-tests-text . tests)
  (->* () #:rest (listof test-suite?) boolean?)
  (andmap (λ (test) (= 0 (run-tests test 'verbose)))
          tests))

;check if any tests fail quietly
; returns true if all given tests pass
;         false if any of the given tests fail
(define/contract (test-fail? . tests)
  (->* () #:rest (listof test-suite?) boolean?)
  (andmap (λ (test) (= 0 (run-tests test 'quiet)))
          tests))

;check if any tests fail
; if any of the given tests fail,
;    - run gui on all the tests
;    - return true
; else
;    - return false
(define/contract (test-fail-with-gui? . tests)
  (->* () #:rest (listof test-suite?) boolean?)
  (let ([result (apply test-fail? tests)])
    (begin (if (not result)
               (apply (make-gui-runner) tests)
               (void))
           result)))

;fulmar-core tests:
(define/contract fulmar-core-tests
  (listof test-suite?)
  (list test-empty-env
        test-comment-env
        test-macro-env
        test-comment-macro-env
        test-macro-comment-env
        test-combine-env
        test-construct-context
        test-enter-env
        test-context-accessors
        test-reindent
        test-enter-comment
        test-enter-macro))

(apply test-fail-with-gui? fulmar-core-tests)
;(apply run-tests-text fulmar-core-tests)

;writer tests:
(define/contract writer-tests
  (listof test-suite?)
  (list test-unknown-nekot-type
        test-add-empty
        test-build-indentation))

(apply test-fail-with-gui? writer-tests)
;(apply run-tests-text writer-tests)