#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")

;unit tests for fulmar-core.rkt

(define/provide-test-suite test-flatten*
 (test-case
  "Test flatten*"
  (check-equal? (flatten*) null)
  (check-equal? (flatten* 'a 'b '(c d e) 'f) '(a b c d e f))))

(define/provide-test-suite test-non-empty-list?
  (test-case
   "Test non-empty-list?"
   (check-true (non-empty-list? '(a)))
   (check-true (non-empty-list? '(a b c)))
   (check-false (non-empty-list? '()))))

(define/provide-test-suite test-indent?
  (test-case
   "Test indent?"
   (check-true (indent? 0))
   (check-true (indent? 3))
   (check-true (indent? 10))
   (check-false (indent? -1))
   (check-false (indent? -10))))

(define/provide-test-suite test-line-length?
  (test-case
   "Test line-length?"
   (check-true (line-length? 3))
   (check-true (line-length? 10))
   (check-false (line-length? 0))
   (check-false (line-length? -1))
   (check-false (line-length? -10))))

(define/provide-test-suite test-chunk-literal?
  (test-case
   "Test chunk-literal?"
   (check-true (chunk-literal? 'asdf))
   (check-true (chunk-literal? 'jkl))
   (check-true (chunk-literal? "asdf"))
   (check-true (chunk-literal? "jkl"))
   (check-true (chunk-literal? "123"))
   (check-true (chunk-literal? #\A))
   (check-true (chunk-literal? #\a))
   (check-true (chunk-literal? #\0))
   (check-true (chunk-literal? 0))
   (check-true (chunk-literal? 3))
   (check-true (chunk-literal? 10))
   (check-false (chunk-literal? -1))
   (check-false (chunk-literal? -10))
   (check-false (chunk-literal? null))
   (check-false (chunk-literal? chunk-literal?))))

(define/provide-test-suite test-chunk-literal-list?
  (test-case
   "Test chunk-literal-list?"
   (check-true (chunk-literal-list? 'asdf))
   (check-true (chunk-literal-list? (list "asdf")))
   (check-true (chunk-literal-list? (list 3 #\A)))
   (check-true (chunk-literal-list? (list 'asdf "asdf" 33 #\a)))
   (check-false (chunk-literal-list? (cons 'asdf 4)))
   (check-false (chunk-literal-list? null))
   (check-false (chunk-literal-list? -1))
   (check-false (chunk-literal-list? -10))
   (check-false (chunk-literal-list? chunk-literal?))))

(define/provide-test-suite test-sc-name?
  (test-case
   "Test sc-name?"
   (check-true (sc-name? 'asdf))
   (check-true (sc-name? 'jkl))
   (check-false (sc-name? 0))
   (check-false (sc-name? 10))
   (check-false (sc-name? "asdf"))))

(define/provide-test-suite test-sc-body?
  (test-case
   "Test sc-body?"
   (check-true (sc-body? 'asdf))
   (check-true (sc-body? 'jkl))
   (check-true (sc-body? 0))
   (check-true (sc-body? 10))
   (check-true (sc-body? "asdf"))
   (check-true (sc-body? #true))
   (check-true (sc-body? #false))))

(define/provide-test-suite test-nekot-name?
  (test-case
   "Test nekot-name?"
   (check-true (nekot-name? 'asdf))
   (check-true (nekot-name? 'jkl))
   (check-false (nekot-name? 0))
   (check-false (nekot-name? 10))
   (check-false (nekot-name? "asdf"))))

(define/provide-test-suite test-nekot-body?
  (test-case
   "Test nekot-body?"
   (check-true (nekot-body? 'asdf))
   (check-true (nekot-body? 'jkl))
   (check-true (nekot-body? 0))
   (check-true (nekot-body? 10))
   (check-true (nekot-body? "asdf"))
   (check-true (nekot-body? #true))
   (check-true (nekot-body? #false))))

(define/provide-test-suite test-environment-description?
  (test-case
   "Test environment-description?"
   (check-true (environment-description? 'comment))
   (check-true (environment-description? 'macro))
   (check-true (environment-description? 'comment-macro))
   (check-true (environment-description? 'macro-comment))
   (check-false (environment-description? 'asdf))
   (check-false (environment-description? 'jkl))
   (check-false (environment-description? 0))
   (check-false (environment-description? 10))
   (check-false (environment-description? "asdf"))))

(define/provide-test-suite test-optional-environment-description?
  (test-case
   "Test optional-environment-description?"
   (check-true (optional-environment-description? #false))
   (check-true (optional-environment-description? 'comment))
   (check-true (optional-environment-description? 'macro))
   (check-true (optional-environment-description? 'comment-macro))
   (check-true (optional-environment-description? 'macro-comment))
   (check-false (optional-environment-description? 'asdf))
   (check-false (optional-environment-description? 'jkl))
   (check-false (optional-environment-description? 0))
   (check-false (optional-environment-description? 10))
   (check-false (optional-environment-description? "asdf"))))

(define/provide-test-suite test-position?
  (test-case
   "Test position?"
   (check-true (position? 0))
   (check-true (position? 3))
   (check-true (position? 10))
   (check-false (position? -1))
   (check-false (position? -10))))

(define/provide-test-suite test-optional-position?
  (test-case
   "Test optional-position?"
   (check-true (optional-position? 0))
   (check-true (optional-position? 3))
   (check-true (optional-position? 10))
   (check-false (optional-position? -1))
   (check-false (optional-position? -10))))

(define/provide-test-suite test-written-line?
  (test-case
   "Test written-line?"
   (check-true (written-line? "asdf"))
   (check-true (written-line? "jkl"))
   (check-true (written-line? "123"))
   (check-false (written-line? 0))
   (check-false (written-line? 'a))
   (check-false (written-line? '()))
   (check-false (written-line? '(a b)))))

(define/provide-test-suite test-written-lines?
  (test-case
   "Test written-lines?"
   (check-true (written-lines? '("asdf")))
   (check-true (written-lines? '("asdf" "jkl")))
   (check-false (written-lines? "asdf"))
   (check-false (written-lines? 0))
   (check-false (written-lines? 'a))
   (check-false (written-lines? '()))
   (check-false (written-lines? '(a b)))))

(define/provide-test-suite test-mode?
  (test-case
   "Test mode?"
   (check-true (mode? 'normal))
   (check-true (mode? 'immediate))
   (check-false (mode? 'anything_else))
   (check-false (mode? #true))
   (check-false (mode? #false))))

(define/provide-test-suite test-chunk?
  (test-case
   "Test chunk?"
   (check-true (chunk? 'asdf))
   (check-true (chunk? 'jkl))
   (check-true (chunk? "asdf"))
   (check-true (chunk? "jkl"))
   (check-true (chunk? "123"))
   (check-true (chunk? #\A))
   (check-true (chunk? #\a))
   (check-true (chunk? #\0))
   (check-true (chunk? 0))
   (check-true (chunk? 3))
   (check-true (chunk? 10))
   (check-true (chunk? (chunk-struct 'name 'body)))
   (check-false (chunk? -1))
   (check-false (chunk? -10))
   (check-false (chunk? chunk-literal?))))

(define/provide-test-suite test-chunk-list?
  (test-case
   "Test chunk-list?"
   (check-true (chunk-list? 'asdf))
   (check-true (chunk-list? (chunk-struct 'name 'body)))
   (check-true (chunk-list? (list "asdf")))
   (check-true (chunk-list? (list 3 #\A)))
   (check-true (chunk-list? (list 'asdf "asdf" 33 #\a)))
   (check-true (chunk-list? (list 'asdf "asdf" 33 #\a (chunk-struct 'name 'body))))
   (check-false (chunk-list? (cons 'asdf 4)))
   (check-false (chunk-list? null))
   (check-false (chunk-list? -1))
   (check-false (chunk-list? -10))
   (check-false (chunk-list? chunk-literal?))))

(define/provide-test-suite test-nullable-chunk-list?
  (test-case
   "Test nullable-chunk-list?"
   (check-true (nullable-chunk-list? 'asdf))
   (check-true (nullable-chunk-list? (chunk-struct 'name 'body)))
   (check-true (nullable-chunk-list? (list "asdf")))
   (check-true (nullable-chunk-list? (list 3 #\A)))
   (check-true (nullable-chunk-list? (list 'asdf "asdf" 33 #\a)))
   (check-true (nullable-chunk-list? (list 'asdf "asdf" 33 #\a (chunk-struct 'name 'body))))
   (check-true (nullable-chunk-list? null))
   (check-false (nullable-chunk-list? (cons 'asdf 4)))
   (check-false (nullable-chunk-list? -1))
   (check-false (nullable-chunk-list? -10))
   (check-false (nullable-chunk-list? chunk-literal?))))

(define/provide-test-suite test-empty-env?
 (test-case
  "Test empty-env?"
  (check-true (empty-env? #false))
  (check-false (empty-env? 'other))))

(define/provide-test-suite test-environment?
  (test-case
   "Test environment?"
   (check-true (environment? #false))
   (check-true (environment? (environment-struct 'comment 0)))
   (check-false (environment? ))))

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

(define/provide-test-suite test-construct-context
  (test-case
   "Test construct-context"
   (check-equal? (construct-context 80)
                 (context 0 80 empty-env))
   (check-equal? (construct-context 80)
                 (context 0 80 empty-env))))

(define/provide-test-suite test-enter-env
  (test-case
   "Test enter-env"
   (define test-context-1 (construct-context 80))
   (check-equal? (enter-env (comment-env 4) test-context-1) (context 0 80 (comment-env 4)))
   (check-equal? (enter-env macro-env test-context-1) (context 0 80 macro-env))))

(define/provide-test-suite test-context-accessors
  (test-case
   "Test non-standard context accessors - empty environment"
   (define test-context (construct-context 80))
   (check-false (context-description test-context))
   (check-false (context-initial-position test-context)))
  (test-case
   "Test non-standard context accessors - comment environment"
   (define comment-context (enter-env (comment-env 4) (construct-context 80)))
   (check-eq? (context-description comment-context) 'comment)
   (check-eq? (context-initial-position comment-context) 4))
  (test-case
   "Test non-standard context accessors - macro environment"
   (define macro-context (enter-env macro-env (construct-context 80)))
   (check-eq? (context-description macro-context) 'macro)
   (check-false (context-initial-position macro-context)))
  (test-case
   "Test non-standard context accessors - comment-macro environment"
   (define comment-macro-context (enter-env macro-env (enter-env (comment-env 4) (construct-context 80))))
   (check-eq? (context-description comment-macro-context) 'comment-macro)
   (check-eq? (context-initial-position comment-macro-context) 4))
  (test-case
   "Test non-standard context accessors - macro-comment environment"
   (define macro-comment-context (enter-env (comment-env 4) (enter-env macro-env (construct-context 80))))
   (check-eq? (context-description macro-comment-context) 'macro-comment)
   (check-eq? (context-initial-position macro-comment-context) 4)))

(define/provide-test-suite test-reindent
  (test-case
   "Test reindent"
   (define test-context (construct-context 80))
   (check-equal? (reindent 4 test-context) (context 4 80 empty-env))
   (check-equal? (reindent 2 (reindent 5 test-context)) (context 7 80 empty-env))))

(define/provide-test-suite test-reset-indent
  (test-case
   "Test reset-indent"
   (define test-context (construct-context 80))
   (check-equal? (reset-indent 4 test-context) (context 4 80 empty-env))
   (check-equal? (reset-indent 2 (reindent 5 test-context)) (context 2 80 empty-env))))

(define/provide-test-suite test-enter-comment
  (test-case
   "Test enter-comment-env"
   (define test-context-1 (construct-context 80))
   (define test-context-2 (context 4 80 (comment-env 4)))
   (define test-context-3 (context 6 80 (comment-env 4)))
   (define test-context-4 (context 0 80 macro-env))
   (define test-context-5 (context 0 80 (macro-comment-env 0)))
   (check-equal? (enter-comment-env (reindent 4 test-context-1)) test-context-2)
   (check-equal? (enter-comment-env (reindent 2 test-context-2)) test-context-3)
   (check-equal? (enter-comment-env test-context-4) test-context-5)))

(define/provide-test-suite test-enter-macro
  (test-case
   "Test enter-macro-env"
   (define test-context-1 (construct-context 80))
   (define test-context-2 (context 0 80 macro-env))
   (define test-context-3 (context 4 80 (comment-env 4)))
   (define test-context-4 (context 4 80 (comment-macro-env 4)))
   (check-equal? (enter-macro-env test-context-1) test-context-2)
   (check-exn exn:fail? (λ () (enter-macro-env test-context-2)))
   (check-equal? (enter-macro-env test-context-3) test-context-4)))
;   (check-equal? (enter-macro-env test-context-3) test-context-3))) ;Causes an intentional error - this line is used to test the testing framework
