#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")
(require "../private/core-chunk.rkt")

;unit tests for core-chunk.rkt

;helper functions

(define/provide-test-suite test-combine-lengths
  (test-case
   "Test combine-lengths"
   (check-eq? (combine-lengths 4) 4)
   (check-eq? (combine-lengths 4 3) 7)
   (check-eq? (combine-lengths '(4 3)) 7)))

(define/provide-test-suite test-combine-strings
  (test-case
   "Test combine-strings"
   (check-equal? (combine-strings "asdf") "asdf")
   (check-equal? (combine-strings 'asdf) "asdf")
   (check-equal? (combine-strings "asdf" "jkl") "asdfjkl")
   (check-equal? (combine-strings '("asdf" "jkl")) "asdfjkl")
   (check-equal? (combine-strings '(("asdf") "jkl")) "asdfjkl")
   (check-equal? (combine-strings '("asdf" ("jkl"))) "asdfjkl")
   (check-equal? (combine-strings '(("asdf") ("jkl"))) "asdfjkl")
   (check-equal? (combine-strings "asdf" 'jkl) "asdfjkl")
   (check-equal? (combine-strings 'asdf "jkl") "asdfjkl")
   (check-equal? (combine-strings 'asdf 'jkl) "asdfjkl")
   (check-equal? (combine-strings '(asdf jkl)) "asdfjkl")))

(define/provide-test-suite test-length-equals-one
  (test-case
   "Test length-equals-one"
   (check-true (length-equals-one (list "")))
   (check-true (length-equals-one (list "  ")))
   (check-true (length-equals-one (list "abcd")))
   (check-false (length-equals-one (list "" "")))
   (check-false (length-equals-one (list "" "  ")))
   (check-false (length-equals-one (list "  " "")))
   (check-false (length-equals-one (list "  " "  ")))
   (check-false (length-equals-one (list "abcd" "")))
   (check-false (length-equals-one (list "" "abcd")))
   (check-false (length-equals-one (list "abcd" "abcd")))))

(define/provide-test-suite test-chunk-transform
  (test-case
   "Test chunk-transform - tests only implicit chunks (not structured chunks)"
   (check-equal? (write-chunk "asdf")
                 '("asdf"))
   (check-equal? (write-chunk 'asdf)
                 '("asdf"))
   (check-equal? (write-chunk 0)
                 '(" "))
   (check-equal? (write-chunk 4)
                 '(" "))))

;nekot-building chunks

(define/provide-test-suite test-literal
  (test-case
   "Test literal"
   (check-equal? (write-chunk (literal "asdf"))
                 '("asdf"))
   (check-equal? (write-chunk (literal 'asdf))
                 '("asdf"))
   (check-equal? (write-chunk (literal "asdf" "jkl"))
                 '("asdfjkl"))
   (check-equal? (write-chunk (literal "asdf" 'jkl))
                 '("asdfjkl"))
   (check-equal? (write-chunk (literal 'asdf "jkl"))
                 '("asdfjkl"))
   (check-equal? (write-chunk (literal 'asdf 'jkl))
                 '("asdfjkl"))
   (check-equal? (write-chunk (literal '(asdf jkl)))
                 '("asdfjkl"))))

(define/provide-test-suite test-spaces
  (test-case
   "Test spaces"
   (check-equal? (write-chunk (spaces 4))
                 '(" "))
   (check-equal? (write-chunk (spaces 4 3))
                 '(" "))
   (check-equal? (write-chunk (spaces '(4 3)))
                 '(" "))))

(define/provide-test-suite test-new-line
  (test-case
   "Test new-line"
   (check-equal? (write-chunk new-line)
                 '("" ""))
   (check-equal? (write-chunk new-line)
                 '("" ""))))

(define/provide-test-suite test-pp-directive
  (test-case
   "Test pp-directive"
   (check-equal? (write-chunk pp-directive)
                 '("#"))))

;meta-nekot-building chunks

(define/provide-test-suite test-empty
  (test-case
   "Test empty"
   (check-equal? (write-chunk empty)
                 '(""))
   (check-equal? (write-chunk (concat empty
                              "asdf"))
                 '("asdf"))))

(define/provide-test-suite test-concat
  (test-case
   "Test concat"
   (check-equal? (write-chunk (concat 'asdf 'jkl)
                                               )
                 '("asdfjkl"))
   (check-equal? (write-chunk (concat (list 'asdf 'jkl))
                                               )
                 '("asdfjkl"))
   (check-equal? (write-chunk (concat 'asdf 4 'jkl)
                                               )
                 '("asdf jkl"))))

(define/provide-test-suite test-immediate
  (test-case
   "Test immediate"
   (check-equal? (write-chunk (immediate 'asdf)
                                              )
                 '("asdf"))
   (check-equal? (write-chunk (immediate 4)
                                              )
                 '(" "))
   (check-equal? (write-chunk (immediate (concat 4 'asdf))
                                              )
                 '(" asdf"))))

(define/provide-test-suite test-speculative
  (test-case
   "Test speculative"
   (define test-success? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (check-equal? (write-chunk (speculative 'asdf test-success? 'jkl)
                                              )
                 '("jkl"))
   (check-equal? (write-chunk (speculative new-line check-length 4)
                                              )
                 '(" "))))

(define/provide-test-suite test-position-indent
  (test-case
   "Test position-indent"
   (define test 'asdf)
   (define test-2 'jkl)
   (check-equal? (write-chunk (position-indent 'asdf)
                                              )
                 '("asdf"))
   (check-equal? (write-chunk (concat 'asdf (position-indent (concat new-line 'jkl)))
                                              )
                 '("    jkl"
                   "asdf"))))


;context-aware chunks

(define/provide-test-suite test-indent
  (test-case
   "Test indent"
   (check-equal? (write-chunk (indent 0
                                                       (concat 'asdf new-line 'jkl))
                                              )
                 '("jkl"
                   "asdf"))
   (check-equal? (write-chunk (indent 2 (concat 'asdf new-line 'jkl))
                                              )
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-chunk (indent 3 (concat 'asdf new-line 'jkl))
                                              )
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 5 (concat 'asdf new-line 'jkl))
                                              )
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-chunk (indent 2
                                                       (concat 'asdf new-line 'jkl))
                                              )
                 '("  jkl"
                   "  asdf"))
   (check-equal? (write-chunk (indent 3
                                                       (concat 'asdf new-line 'jkl))
                                              )
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 5
                                                       (concat 'asdf new-line 'jkl))
                                              )
                 '("     jkl"
                   "     asdf"))
   (check-equal? (write-chunk (indent 2 (indent 1 (concat 'asdf new-line 'jkl)))
                                              )
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 3 (indent 2 (concat 'asdf new-line 'jkl)))
                                              )
                 '("     jkl"
                   "     asdf")))
  (test-case
   "Test indent - interaction with position-indent tests"
   (check-equal? (write-chunk (indent 3 (position-indent (concat 'asdf new-line 'jkl)))
                                              )
                 '("   jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl))))
                                              )
                 '("       jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl new-line "123"))))
                                              )
                 '("       123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 3 (concat 'asdf (position-indent (concat new-line 'jkl new-line (indent 1 "123")))))
                                              )
                 '("        123"
                   "       jkl"
                   "   asdf"))
   (check-equal? (write-chunk (indent 3 (concat 'asdf (position-indent (concat new-line
                                                                                                (indent 1 'jkl)
                                                                                                new-line
                                                                                                "123"))))
                                              )
                 '("       123"
                   "        jkl"
                   "   asdf"))))

(define/provide-test-suite test-comment-env-chunk
  (test-case
   "Test comment-env-chunk"
   (check-equal? (write-chunk (comment-env-chunk 'asdf #\*))
                 '("/**asdf */"))
   (check-equal? (write-chunk (comment-env-chunk 'asdf #\ ))
                 '("/* asdf */"))
   (check-equal? (write-chunk (comment-env-chunk 'asdf))
                 '("/* asdf */"))
   (check-equal? (write-chunk (comment-env-chunk (indent 2 (comment-env-chunk 'asdf))))
                 '("/* /* asdf ** */"))
   (check-equal? (write-chunk (comment-env-chunk (concat new-line (indent 2 (comment-env-chunk 'asdf)))))
                 '("  /* asdf ** */"
                   "/*"))
   (check-equal? (write-chunk (comment-env-chunk (concat 'asdf new-line 'jkl)))
                 '("jkl */" 
                   "/* asdf"))))
