#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "fulmar-core-tests.rkt")
(require "core-chunk-tests.rkt")
(require "writer-tests.rkt")
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
  (list test-flatten*
        
        
        
        
        
        test-combine-env
        test-construct-context
        test-enter-env
        
        test-reindent
        
        test-enter-comment
        test-enter-macro))

(apply run-tests-text fulmar-core-tests)

;writer tests:
(define/contract writer-tests
  (listof test-suite?)
  (list test-is-whitespace?
        test-make-whitespace
        test-remove-whitespace
        test-build-indentation
        test-finish-line
        test-check-speculative-line-length
        test-add-hash-character
        test-add-literal
        test-add-spaces
        test-add-new-line
        test-add-pp-directive
        test-add-empty
        test-add-concatenated
        test-add-immediate
        test-add-speculative
        test-change-indent-to-current
        test-unknown-nekot-type
        test-write-nekot))

(apply run-tests-text writer-tests)

;core chunk tests:
(define/contract core-chunk-tests
  (listof test-suite?)
  (list test-combine-lengths
        test-combine-strings
        test-length-equals-one
        test-chunk-transform
        test-literal
        test-spaces
        test-new-line
        test-pp-directive
        test-empty
        test-concat
        test-immediate
        test-speculative
        test-position-indent
        test-modify-context
        test-indent
        test-comment-env-chunk
        test-macro-env-chunk))

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
