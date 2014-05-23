#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "fulmar-core-tests.rkt")
(require "core-chunk-tests.rkt")
(require "standard-chunk-tests.rkt")

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

;fulmar-core tests:
(define/contract fulmar-core-tests
  (listof test-suite?)
  (list test-flatten*))

(apply run-tests-text fulmar-core-tests)

;core chunk tests:
(define/contract core-chunk-tests
  (listof test-suite?)
  (list test-combine-strings
        test-length-equals-one
        test-chunk-transform
        test-literal
        test-space
        test-new-line
        test-pp-directive
        test-empty
        test-concat
        test-immediate
        test-speculative
        test-position-indent
        test-indent
        test-comment-env-chunk))

(apply run-tests-text core-chunk-tests)

;standard chunk tests:
(define/contract standard-core-tests
  (listof test-suite?)
  (list test-basic-chunks
        test-attach-list-separator
        test-between
        test-between/attach
        test-arg-list
        test-paren-list
        test-template-list
        test-constructor-assignment-list
        test-body
        test-smt-list
        test-pp-define
        test-pp-include
        test-pp-includes
        test-pp-ifdef
        test-pp-ifndef
        test-pp-else
        test-pp-endif
        test-pp-conditional
        test-pp-conditional-ifdef
        test-pp-conditional-ifndef
        test-pp-header-file
        test-macro-define
        test-namespace-define
        test-described-smts
        test-constize
        test-template-define
        test-template-use
        test-function-declare
        test-static-function-declare
        test-void-function-declare
        test-function-define
        test-void-function-define
        test-returning-function-define
        test-constructor-assignment
        test-constructor
        test-struct-declare
        test-template-struct-declare
        test-section-define
        test-struct-define
        test-template-struct-define
        test-scope-resolution-operator
        test-typedef-smt
        test-function-call
        test-member-function-call))

(apply run-tests-text standard-core-tests)
