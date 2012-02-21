#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "fulmar-core-tests.rkt")
(require "core-chunk-tests.rkt")
(require "writer-tests.rkt")
(require "standard-chunk-tests.rkt")
(require "io-tests.rkt")

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
  (list test-flatten*
        test-empty-env
        test-comment-env
        test-macro-env
        test-comment-macro-env
        test-macro-comment-env
        test-combine-env
        test-construct-context
        test-enter-env
        test-context-accessors
        test-reindent
        test-reset-indent
        test-enter-comment
        test-enter-macro))

(apply test-fail-with-gui? fulmar-core-tests)
;(apply run-tests-text fulmar-core-tests)

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

(apply test-fail-with-gui? writer-tests)
;(apply run-tests-text writer-tests)

;core chunk tests:
(define/contract core-chunk-tests
  (listof test-suite?)
  (list test-combine-lengths
        test-combine-strings
        test-length-equals-one
        test-chunk-transform
        test-literal-chunk
        test-spaces-chunk
        test-new-line-chunk
        test-pp-directive-chunk
        test-empty-chunk
        test-concat-chunk
        test-immediate-chunk
        test-speculative-chunk
        test-position-indent-chunk
        test-modify-context-chunk
        test-indent-chunk
        test-comment-env-chunk
        test-macro-env-chunk))

(apply test-fail-with-gui? core-chunk-tests)
;(apply run-tests-text core-chunk-tests)

;standard chunk tests:
(define/contract standard-core-tests
  (listof test-suite?)
  (list test-character-chunks
        test-keyword-chunks
        test-attach-list-separator
        test-between-chunk
        test-between/attach-chunk
        test-arg-list-chunk
        test-paren-list-chunk
        test-template-list-chunk
        test-constructor-assignment-list-chunk
        test-body-chunk
        test-smt-list-chunk
        test-top-smt-list-chunk
        test-pp-define-chunk
        test-pp-include-chunk
        test-pp-includes-chunk
        test-pp-ifdef-chunk
        test-pp-ifndef-chunk
        test-pp-else-chunk
        test-pp-endif-chunk
        test-pp-conditional-chunk
        test-pp-conditional-ifdef-chunk
        test-pp-conditional-ifndef-chunk
        test-pp-header-file-chunk
        test-macro-define-chunk
        test-namespace-define-chunk
        test-described-smts-chunk
        test-constize-chunk
        test-template-define-chunk
        test-template-use-chunk
        test-function-declare-chunk
        test-static-function-declare-chunk
        test-void-function-declare-chunk
        test-function-define-chunk
        test-void-function-define-chunk
        test-returning-function-define-chunk
        test-constructor-assignment-chunk
        test-constructor-chunk
        test-struct-declare-chunk
        test-template-struct-declare-chunk
        test-section-define-chunk
        test-struct-define-chunk
        test-template-struct-define-chunk
        test-typedef-smt-chunk
        test-function-call-chunk
        test-member-function-call-chunk))

(apply test-fail-with-gui? standard-core-tests)
;(apply run-tests-text standard-core-tests)
