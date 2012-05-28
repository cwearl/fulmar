#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")

;unit tests for fulmar-core.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b (list 'c 'd 'e) 'f) (list 'a 'b 'c 'd 'e 'f))))

(define/provide-test-suite test-non-empty-list?
  (test-case
   "Test non-empty-list?"
   (check-true (non-empty-list? (list 'a)))
   (check-false (non-empty-list? (list)))))

(define/provide-test-suite test-indent?
  (test-case
   "Test indent?"
   (check-true (indent? 3))
   (check-true (indent? 0))
   (check-false (indent? -1))))

(define/provide-test-suite test-string-type?
  (test-case
   "Test string-type?"
   (check-true (string-type? "asdf"))
   (check-true (string-type? 'asdf))
   (check-false (string-type? -1))))

(define/provide-test-suite test-literal-type?
  (test-case
   "Test literal-type?"
   (check-true (literal-type? 3))
   (check-true (literal-type? 'asdf))
   (check-false (literal-type? -1))))

(define/provide-test-suite test-literal-list?
  (test-case
   "Test literal-list?"
   (check-true (literal-list? 3))
   (check-true (literal-list? 'asdf))
   (check-true (literal-list? (list 3 "asdf" 'asdf (list 0 2))))
   (check-false (literal-list? -1))))

(define/provide-test-suite test-line-length?
  (test-case
   "Test line-length?"
   (check-true (line-length? 3))
   (check-false (line-length? 0))
   (check-false (line-length? -1))))

(define/provide-test-suite test-chunk-literal?
  (test-case
   "Test chunk-literal?"
   (check-true (chunk-literal? 'asdf))
   (check-true (chunk-literal? "asdf"))
   (check-true (chunk-literal? #\a))
   (check-true (chunk-literal? 3))
   (check-false (chunk-literal? -1))))

(define/provide-test-suite test-sc-name?
  (test-case
   "Test sc-name?"
   (check-true (sc-name? 'asdf))
   (check-false (sc-name? "asdf"))))

(define/provide-test-suite test-sc-body?
  (test-case
   "Test sc-body?"
   (check-true (sc-body? 'asdf))
   (check-true (sc-body? "asdf"))
   (check-true (sc-body? 3))))

(define/provide-test-suite test-environment-description?
  (test-case
   "Test environment-description?"
   (check-true (environment-description? 'comment))
   (check-true (environment-description? 'macro))
   (check-true (environment-description? 'comment-macro))
   (check-true (environment-description? 'macro-comment))
   (check-false (environment-description? 'asdf))))

(define/provide-test-suite test-optional-environment-description?
  (test-case
   "Test optional-environment-description?"
   (check-true (optional-environment-description? #false))
   (check-true (optional-environment-description? 'comment))
   (check-true (optional-environment-description? 'macro))
   (check-true (optional-environment-description? 'comment-macro))
   (check-true (optional-environment-description? 'macro-comment))
   (check-false (optional-environment-description? 'asdf))))

(define/provide-test-suite test-position?
  (test-case
   "Test position?"
   (check-true (position? 0))
   (check-true (position? 3))
   (check-false (position? -1))))

(define/provide-test-suite test-optional-position?
  (test-case
   "Test optional-position?"
   (check-true (optional-position? #false))
   (check-true (optional-position? 0))
   (check-true (optional-position? 3))
   (check-false (optional-position? -1))))

(define/provide-test-suite test-mode?
  (test-case
   "Test mode?"
   (check-true (mode? 'normal))
   (check-true (mode? 'immediate))
   (check-false (mode? 'anything_else))))

(define/provide-test-suite test-chunk?
  (test-case
   "Test chunk?"
   (check-true (chunk? 'asdf))
   (check-true (chunk? "asdf"))
   (check-true (chunk? #\a))
   (check-true (chunk? 3))
   (check-true (chunk? (chunk-struct 'name 'body)))
   (check-false (chunk? -1))))

(define/provide-test-suite test-chunk-list?
  (test-case
   "Test chunk-list?"
   (check-true (chunk-list? 'asdf))
   (check-true (chunk-list? (chunk-struct 'name 'body)))
   (check-true (chunk-list? (list 'asdf "asdf" 33 #\a)))
   (check-true (chunk-list? (list 'asdf "asdf" 33 #\a (chunk-struct 'name 'body))))
   (check-false (chunk-list? null))
   (check-false (chunk-list? (list 'asdf "asdf" 33 #\a -1)))))

(define/provide-test-suite test-nullable-chunk-list?
  (test-case
   "Test nullable-chunk-list?"
   (check-true (nullable-chunk-list? 'asdf))
   (check-true (nullable-chunk-list? (chunk-struct 'name 'body)))
   (check-true (nullable-chunk-list? (list 'asdf "asdf" 33 #\a)))
   (check-true (nullable-chunk-list? (list 'asdf "asdf" 33 #\a (chunk-struct 'name 'body))))
   (check-true (nullable-chunk-list? null))
   (check-false (nullable-chunk-list? (list 'asdf "asdf" 33 #\a -1)))))

(define/provide-test-suite test-empty-env
 (test-case
  "Test empty-env and empty-env?"
  (check-true (empty-env? #false))
  (check-false (empty-env? 'other))))

(define/provide-test-suite test-comment-env
  (test-case
   "Test comment-env and comment-env?"
   (check-true (comment-env? (comment-env 4)))
   (check-false (comment-env? empty-env))))

(define/provide-test-suite test-macro-env
  (test-case
   "Test macro-env and macro-env?"
   (check-true (macro-env? macro-env))
   (check-false (macro-env? empty-env))))

(define/provide-test-suite test-comment-macro-env
  (test-case
   "Test comment-macro-env and comment-macro-env?"
   (check-true (comment-macro-env? (comment-macro-env 4)))
   (check-false (comment-macro-env? empty-env))))

(define/provide-test-suite test-macro-comment-env
  (test-case
   "Test macro-comment-env and macro-comment-env?"
   (check-true (macro-comment-env? (macro-comment-env 4)))
   (check-false (macro-comment-env? empty-env))))

(define/provide-test-suite test-environment?
  (test-case
   "Test environment?"
   (check-true (environment? #false))
   (check-true (environment? (comment-env 0)))
   (check-true (environment? macro-env))
   (check-true (environment? (comment-macro-env 0)))
   (check-true (environment? (macro-comment-env 0)))
   (check-false (environment? 'asdf))))

(define/provide-test-suite test-user-env?
  (test-case
   "Test user-env?"
   (check-true (user-env? #false))
   (check-true (user-env? (comment-env 0)))
   (check-true (user-env? macro-env))
   (check-false (user-env? (comment-macro-env 0)))
   (check-false (user-env? (macro-comment-env 0)))
   (check-false (user-env? 'asdf))))

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

(define/provide-test-suite test-context?
  (test-case
   "Test context?"
   (check-true (context? (initial-context 80)))
   (check-false (context? 'asdf))))

(define/provide-test-suite test-enter-env
  (test-case
   "Test enter-env"
   (define test-context-1 (initial-context 80))
   (check-equal? (enter-env (comment-env 4) test-context-1) (context-struct 0 80 (comment-env 4)))
   (check-equal? (enter-env macro-env test-context-1) (context-struct 0 80 macro-env))))

(define/provide-test-suite test-context-accessors
  (test-case
   "Test non-standard context accessors - empty environment"
   (define test-context (initial-context 80))
   (check-equal? (context-indent test-context) 0)
   (check-equal? (context-line-length test-context) 80)
   (check-false (context-env test-context))
   (check-false (context-description test-context))
   (check-false (context-initial-position test-context)))
  (test-case
   "Test non-standard context accessors - comment environment"
   (define test-context (enter-env (comment-env 4) (initial-context 80)))
   (check-equal? (context-indent test-context) 0)
   (check-equal? (context-line-length test-context) 80)
   (check-equal? (context-env test-context) (comment-env 4))
   (check-eq? (context-description test-context) 'comment)
   (check-eq? (context-initial-position test-context) 4)))

(define/provide-test-suite test-reset-indent
  (test-case
   "Test reset-indent"
   (define test-context (initial-context 80))
   (check-equal? (reset-indent 4 test-context) (context-struct 4 80 empty-env))
   (check-equal? (reset-indent 2 (reindent 5 test-context)) (context-struct 2 80 empty-env))))

(define/provide-test-suite test-reindent
  (test-case
   "Test reindent"
   (define test-context (initial-context 80))
   (check-equal? (reindent 4 test-context) (context-struct 4 80 empty-env))
   (check-equal? (reindent 2 (reindent 5 test-context)) (context-struct 7 80 empty-env))))

(define/provide-test-suite test-enter-comment
  (test-case
   "Test enter-comment-env"
   (define test-context-1 (initial-context 80))
   (define test-context-2 (context-struct 4 80 (comment-env 4)))
   (define test-context-3 (context-struct 6 80 (comment-env 4)))
   (define test-context-4 (context-struct 0 80 macro-env))
   (define test-context-5 (context-struct 0 80 (macro-comment-env 0)))
   (check-equal? (enter-comment-env (reindent 4 test-context-1)) test-context-2)
   (check-equal? (enter-comment-env (reindent 2 test-context-2)) test-context-3)
   (check-equal? (enter-comment-env test-context-4) test-context-5)))

(define/provide-test-suite test-enter-macro
  (test-case
   "Test enter-macro-env"
   (define test-context-1 (initial-context 80))
   (define test-context-2 (context-struct 0 80 macro-env))
   (define test-context-3 (context-struct 4 80 (comment-env 4)))
   (define test-context-4 (context-struct 4 80 (comment-macro-env 4)))
   (check-equal? (enter-macro-env test-context-1) test-context-2)
   (check-exn exn:fail? (λ () (enter-macro-env test-context-2)))
   (check-equal? (enter-macro-env test-context-3) test-context-4)))
