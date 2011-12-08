#lang racket

(require rackunit)
(require rackunit/gui)
(require rackunit/text-ui)
(require "fulmar-core-tests.rkt")
(require "chunk-core-tests.rkt")
(require "writer-tests.rkt")
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

;core chunk tests:
(define/contract chunk-core-tests
  (listof test-suite?)
  (list test-combine-lengths
        test-combine-strings
        test-length-equals-one
        test-literal-chunk
        test-spaces-chunk
        test-new-line-chunk
        test-pp-directive-chunk
        test-empty-chunk
        test-concat-chunk
        test-immediate-chunk
        test-speculative-chunk
        test-position-indent-chunk
        test-indent-chunk
        test-comment-env-chunk
        test-comment-line-chunk
        test-macro-env-chunk))

(apply test-fail-with-gui? chunk-core-tests)
;(apply run-tests-text chunk-core-tests)

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