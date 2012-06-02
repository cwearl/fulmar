#lang racket

(require rackunit)
(require "../src/basics.rkt")
(require "../src/chunk.rkt")

;unit tests for chunk.rkt

(define/provide-test-suite test-literal-chunk?
  (test-case
   "Test literal-chunk?"
   (check-true (chunk-literal? "asdf"))
   (check-true (chunk-literal? 'asdf))
   (check-true (chunk-literal? #\a))
   (check-true (chunk-literal? 0))
   (check-true (chunk-literal? 3))
   (check-true (chunk-literal? (list "asdf" 'asdf #\a (list 0 3))))
   (check-false (chunk-literal? -1))))

(define/provide-test-suite test-indent-chunk-body?
  (test-case
   "Test indent-chunk-body?"
   (check-true (indent-chunk-body? (list 0 0)))
   (check-true (indent-chunk-body? (list 3 'asdf)))
   (check-false (indent-chunk-body? (list 3 -1)))
   (check-false (indent-chunk-body? (list -1 'asdf)))
   (check-false (indent-chunk-body? (list -1 -1)))
   (check-false (indent-chunk-body? 'other))))

(define/provide-test-suite test-chunk-nl-internal?
  (test-case
   "Test chunk-nl-internal?"
   (check-true (chunk-nl-internal? 'literal 'asdf))
   (check-true (chunk-nl-internal? 'literal "asdf"))
   (check-true (chunk-nl-internal? 'literal #\a))
   (check-true (chunk-nl-internal? 'literal 3))
   (check-false (chunk-nl-internal? 'literal -1))
   (check-true (chunk-nl-internal? 'new-line #false))
   (check-false (chunk-nl-internal? 'new-line 0))
   (check-true (chunk-nl-internal? 'pp-directive #false))
   (check-false (chunk-nl-internal? 'pp-directive 0))
   (check-true (chunk-nl-internal? 'concat 'asdf))
   (check-true (chunk-nl-internal? 'concat (list 'asdf 3 6)))
   (check-false (chunk-nl-internal? 'concat (list 'asdf 3 -1)))
   (check-true (chunk-nl-internal? 'position-indent 'asdf))
   (check-false (chunk-nl-internal? 'position-indent -1))
   (check-true (chunk-nl-internal? 'indent (list 3 'asdf)))
   (check-false (chunk-nl-internal? 'indent -1))
   (check-true (chunk-nl-internal? 'comment 'asdf))
   (check-false (chunk-nl-internal? 'comment -1))
   (check-true (chunk-nl-internal? 'macro 'asdf))
   (check-false (chunk-nl-internal? 'macro -1))
   (check-false (chunk-nl-internal? 'other 0))))

(define/provide-test-suite test-chunk-nl?
  (test-case
   "Test chunk-nl?"
   (check-true (chunk-nl? new-line-chunk))
   (check-true (chunk-nl? pp-directive-chunk))
   (check-true (chunk-nl? (concat-chunk 'as 'df)))
   (check-true (chunk-nl? (position-indent-chunk 'asdf)))
   (check-true (chunk-nl? (indent-chunk 0 'asdf)))
   (check-true (chunk-nl? (comment-chunk 'asdf)))
   (check-true (chunk-nl? (macro-chunk 'asdf)))
   (check-false (chunk-nl? 'other))))

(define/provide-test-suite test-chunk-name?
  (test-case
   "Test chunk-name?"
   (check-true (chunk-name? 'literal))
   (check-true (chunk-name? 'new-line))
   (check-true (chunk-name? 'pp-directive))
   (check-true (chunk-name? 'concat))
   (check-true (chunk-name? 'position-indent))
   (check-true (chunk-name? 'indent))
   (check-true (chunk-name? 'comment))
   (check-true (chunk-name? 'macro))
   (check-false (chunk-name? 'other))))

(define/provide-test-suite test-chunk-body?
  (test-case
   "Test chunk-body?"
   (check-true (chunk-body? 'asdf))
   (check-true (chunk-body? #false))
   (check-true (chunk-body? (list 'asdf 3)))
   (check-true (chunk-body? new-line-chunk))
   (check-true (chunk-body? (list 3 'asdf)))
   (check-false (chunk-body? -1))))

(define/provide-test-suite test-chunk?
  (test-case
   "Test chunk?"
   (check-true (chunk? 'asdf))
   (check-true (chunk? new-line-chunk))
   (check-false (chunk? -1))))

(define/provide-test-suite test-chunk-list?
  (test-case
   "Test chunk-list?"
   (check-true (chunk-list? 'asdf))
   (check-true (chunk-list? new-line-chunk))
   (check-true (chunk-list? (list 'asdf new-line-chunk)))
   (check-false (chunk-list? null))
   (check-false (chunk-list? -1))
   (check-false (chunk-list? (list 'asdf -1)))))

(define/provide-test-suite test-nullable-chunk-list?
  (test-case
   "Test nullable-chunk-list?"
   (check-true (nullable-chunk-list? 'asdf))
   (check-true (nullable-chunk-list? new-line-chunk))
   (check-true (nullable-chunk-list? (list 'asdf new-line-chunk)))
   (check-true (nullable-chunk-list? null))
   (check-false (nullable-chunk-list? -1))
   (check-false (nullable-chunk-list? (list 'asdf -1)))))

(define/provide-test-suite test-literal-chunk
  (test-case
   "Test literal-chunk"
   (check-equal? (literal-chunk "asdf") (list "asdf"))
   (check-equal? (literal-chunk 'asdf) (list 'asdf))
   (check-equal? (literal-chunk 0) (list 0))
   (check-equal? (literal-chunk #\a) (list #\a))
   (check-equal? (literal-chunk (list 0 3 'asdf)) (list 0 3 'asdf))
   (check-equal? (literal-chunk (list (list 0) (list 3 'asdf))) (list 0 3 'asdf))
   (check-equal? (chunk-name 'asdf) 'literal)
   (check-equal? (chunk-body 'asdf) 'asdf)))

(define/provide-test-suite test-spaces-chunk
  (test-case
   "Test spaces-chunk"
   (check-equal? (spaces-chunk "asdf") 4)
   (check-equal? (spaces-chunk 'asdf) 4)
   (check-equal? (spaces-chunk 0) 0)
   (check-equal? (spaces-chunk #\a) 1)
   (check-equal? (spaces-chunk (list 0 3 'asdf)) 7)
   (check-equal? (spaces-chunk (list (list 0) (list 3 'asdf))) 7)))

(define/provide-test-suite test-new-line-chunk
  (test-case
   "Test new-line-chunk"
   (check-true (chunk? new-line-chunk))
   (check-true (chunk-nl? new-line-chunk))
   (check-equal? (chunk-name new-line-chunk) 'new-line)
   (check-equal? (chunk-body new-line-chunk) #false)))

(define/provide-test-suite test-pp-directive-chunk
  (test-case
   "Test pp-directive-chunk"
   (check-true (chunk? pp-directive-chunk))
   (check-true (chunk-nl? pp-directive-chunk))
   (check-equal? (chunk-name pp-directive-chunk) 'pp-directive)
   (check-equal? (chunk-body pp-directive-chunk) #false)))

(define/provide-test-suite test-concat-chunk
  (test-case
   "Test concat-chunk"
   (check-true (chunk? (concat-chunk 'asdf 0)))
   (check-true (chunk-nl? (concat-chunk 'asdf 0)))
   (check-equal? (chunk-name (concat-chunk 'asdf 0)) 'concat)
   (check-equal? (chunk-body (concat-chunk 'asdf 0 (list 3 2))) (list 'asdf 0 3 2))))

(define/provide-test-suite test-no-line-chunk
  (test-case
   "Test no-line-chunk"
   (check-true (chunk? (no-line-chunk 'asdf 0)))
   (check-true (chunk-nl? (no-line-chunk 'asdf 0)))
   (check-equal? (chunk-name (no-line-chunk 'asdf 0)) 'no-line)
   (check-equal? (chunk-body (no-line-chunk 'asdf 0 (list 3 2))) (list 'asdf 0 3 2))))

(define/provide-test-suite test-position-indent-chunk
  (test-case
   "Test position-indent-chunk"
   (check-true (chunk? (position-indent-chunk 'asdf)))
   (check-true (chunk-nl? (position-indent-chunk 'asdf)))
   (check-equal? (chunk-name (position-indent-chunk 'asdf)) 'position-indent)
   (check-equal? (chunk-body (position-indent-chunk 'asdf)) 'asdf)))

(define/provide-test-suite test-indent-chunk
  (test-case
   "Test indent-chunk"
   (check-true (chunk? (indent-chunk 0 'asdf)))
   (check-true (chunk-nl? (indent-chunk 0 'asdf)))
   (check-equal? (chunk-name (indent-chunk 0 'asdf)) 'indent)
   (check-equal? (chunk-body (indent-chunk 0 'asdf)) (list 0 'asdf))))

(define/provide-test-suite test-comment-chunk
  (test-case
   "Test comment-chunk"
   (check-true (chunk? (comment-chunk 'asdf)))
   (check-true (chunk-nl? (comment-chunk 'asdf)))
   (check-equal? (chunk-name (comment-chunk 'asdf)) 'comment)
   (check-equal? (chunk-body (comment-chunk 'asdf)) 'asdf)))

(define/provide-test-suite test-macro-chunk
  (test-case
   "Test macro-chunk"
   (check-true (chunk? (macro-chunk 'asdf)))
   (check-true (chunk-nl? (macro-chunk 'asdf)))
   (check-equal? (chunk-name (macro-chunk 'asdf)) 'macro)
   (check-equal? (chunk-body (macro-chunk 'asdf)) 'asdf)))

(define/provide-test-suite test-chunk-name
  (test-case
   "Test chunk-name"
   (check-equal? (chunk-name 'asdf) 'literal)
   (check-equal? (chunk-name 3) 'literal)
   (check-equal? (chunk-name new-line-chunk) 'new-line)
   (check-equal? (chunk-name pp-directive-chunk) 'pp-directive)
   (check-equal? (chunk-name (concat-chunk 'asdf 0)) 'concat)
   (check-equal? (chunk-name (position-indent-chunk 'asdf)) 'position-indent)
   (check-equal? (chunk-name (indent-chunk 0 'asdf)) 'indent)
   (check-equal? (chunk-name (comment-chunk 'asdf)) 'comment)
   (check-equal? (chunk-name (macro-chunk 'asdf)) 'macro)))
   
(define/provide-test-suite test-chunk-body
  (test-case
   "Test chunk-body"
   (check-equal? (chunk-body 'asdf) 'asdf)
   (check-equal? (chunk-body 3) 3)
   (check-equal? (chunk-body new-line-chunk) #false)
   (check-equal? (chunk-body pp-directive-chunk) #false)
   (check-equal? (chunk-body (concat-chunk 'asdf 0)) (list 'asdf 0))
   (check-equal? (chunk-body (position-indent-chunk 'asdf)) 'asdf)
   (check-equal? (chunk-body (indent-chunk 0 'asdf)) (list 0 'asdf))
   (check-equal? (chunk-body (comment-chunk 'asdf)) 'asdf)
   (check-equal? (chunk-body (macro-chunk 'asdf)) 'asdf)))
