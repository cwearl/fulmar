#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "basics-tests.rkt")
(require "environment-tests.rkt")
(require "context-tests.rkt")
(require "chunk-tests.rkt")
(require "sequence-tests.rkt")
(require "line-tests.rkt")
(require "block-tests.rkt")

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

;basics tests:
(define/contract basics-tests
  (listof test-suite?)
  (list test-flatten*
        test-non-empty-list?
        test-or?
        test-and?
        test-list-of?
        test-indent?
        test-optional-indent?
        test-line-length?))

(apply test-fail-with-gui? basics-tests)
;(apply run-tests-text basics-tests)

;environment tests:
(define/contract environment-tests
  (listof test-suite?)
  (list test-env-description?
        test-optional-env-description?
        test-env-type-constructors-and-predicates
        test-env?
        test-user-env?
        test-env-description
        test-env-comment-indent
        test-combine-env))

(apply test-fail-with-gui? environment-tests)
;(apply run-tests-text environment-tests)

;context tests:
(define/contract context-tests
  (listof test-suite?)
  (list test-divide?
        test-abut?
        test-mode?
        test-context-constructor-and-predicate
        test-context-accessors
        test-increase-indent
        test-reset-indent
        test-enter-comment-env
        test-enter-macro-env))

(apply test-fail-with-gui? context-tests)
;(apply run-tests-text context-tests)

;chunk tests:
(define/contract chunk-tests
  (listof test-suite?)
  (list test-literal-chunk?
        test-indent-chunk-body?
        test-chunk-nl-internal?
        test-chunk-nl?
        test-chunk-name?
        test-chunk-body?
        test-chunk?
        test-chunk-list?
        test-nullable-chunk-list?
        test-literal-chunk
        test-spaces-chunk
        test-new-line-chunk
        test-pp-directive-chunk
        test-concat-chunk
        test-no-line-chunk
        test-indent-chunk
        test-comment-chunk
        test-macro-chunk
        test-chunk-name
        test-chunk-body))

(apply test-fail-with-gui? chunk-tests)
;(apply run-tests-text chunk-tests)

;sequence tests:
(define/contract sequence-tests
  (listof test-suite?)
  (list test-seq-IR?
        test-seq-input?
        test-seq?
        test-seq-output?
        test-seq
        test-seq-IR
        test-build-seq
        test-simplify-seq-IR))

(apply test-fail-with-gui? sequence-tests)
;(apply run-tests-text sequence-tests)

;line tests:
(define/contract line-tests
  (listof test-suite?)
  (list test-print-item?
        test-line-IR?
        test-line-input?
        test-line?
        test-line
        test-line-IR
        test-block-last
        test-block-rest
        test-build-line-IR
        test-simplify-line-IR
        test-pivot-IR?
        test-pivot-input?
        test-pivot?
        test-pivot
        test-pivot-IR
        test-pivot-length
        test-build-pivot
        test-pivot-full-line-length
        test-full-line-length))

(apply test-fail-with-gui? line-tests)
;(apply run-tests-text line-tests)

;block tests:
(define/contract block-tests
  (listof test-suite?)
  (list test-block-IR?
        test-block-input?
        test-block?
        test-block
        test-block-IR
        test-block-last
        test-block-rest
        test-build-block-IR))

(apply test-fail-with-gui? block-tests)
;(apply run-tests-text block-tests)

;writer tests:
;(define/contract writer-tests
;  (listof test-suite?)
;  (list test-is-whitespace?
;        test-make-whitespace
;        test-remove-whitespace
;        test-build-indentation
;        test-finish-line
;        test-check-speculative-line-length
;        test-add-hash-character
;        test-add-literal
;        test-add-spaces
;        test-add-new-line
;        test-add-pp-directive
;        test-add-empty
;        test-add-concatenated
;        test-add-immediate
;        test-add-speculative
;        test-change-indent-to-current
;        test-unknown-nekot-type
;        test-write-nekot))

;(apply test-fail-with-gui? writer-tests)
;(apply run-tests-text writer-tests)

;standard chunk tests:
;(define/contract standard-core-tests
;  (listof test-suite?)
;  (list test-character-chunks
;        test-keyword-chunks
;        test-attach-list-separator
;        test-between-chunk
;        test-between/attach-chunk
;        test-arg-list-chunk
;        test-paren-list-chunk
;        test-template-list-chunk
;        test-constructor-assignment-list-chunk
;        test-body-chunk
;        test-smt-list-chunk
;        test-top-smt-list-chunk
;        test-pp-define-chunk
;        test-pp-include-chunk
;        test-pp-includes-chunk
;        test-pp-ifdef-chunk
;        test-pp-ifndef-chunk
;        test-pp-else-chunk
;        test-pp-endif-chunk
;        test-pp-conditional-chunk
;        test-pp-conditional-ifdef-chunk
;        test-pp-conditional-ifndef-chunk
;        test-pp-header-file-chunk
;        test-macro-define-chunk
;        test-namespace-define-chunk
;        test-described-smts-chunk
;        test-constize-chunk
;        test-template-define-chunk
;        test-template-use-chunk
;        test-function-declare-chunk
;        test-static-function-declare-chunk
;        test-void-function-declare-chunk
;        test-function-define-chunk
;        test-void-function-define-chunk
;        test-returning-function-define-chunk
;        test-constructor-assignment-chunk
;        test-constructor-chunk
;        test-struct-declare-chunk
;        test-template-struct-declare-chunk
;        test-section-define-chunk
;        test-struct-define-chunk
;        test-template-struct-define-chunk
;        test-scope-resolution-operator-chunk
;        test-typedef-smt-chunk
;        test-function-call-chunk
;        test-member-function-call-chunk))

;(apply test-fail-with-gui? standard-core-tests)
;(apply run-tests-text standard-core-tests)
