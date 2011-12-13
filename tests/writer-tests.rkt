#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")
(require "../src/chunk-core.rkt")
(require "../src/writer.rkt")

;unit tests for writer.rkt

;helper tests

(define/provide-test-suite test-is-whitespace?
  (test-case
   "Test is-whitespace? (checks if string is all whitespace)"
   (check-true (is-whitespace? ""))
   (check-true (is-whitespace? " "))
   (check-true (is-whitespace? "  "))
   (check-true (is-whitespace? "                  "))
   (check-false (is-whitespace? "a"))
   (check-false (is-whitespace? " a "))
   (check-false (is-whitespace? "a    "))
   (check-false (is-whitespace? "    a"))
   (check-false (is-whitespace? "asdf"))))

(define/provide-test-suite test-make-whitespace
  (test-case
   "Test make-whitespace (make string of spaces)"
   (check-equal? (make-whitespace 0) "")
   (check-equal? (make-whitespace 1) " ")
   (check-equal? (make-whitespace 2) "  ")
   (check-equal? (make-whitespace 3) "   ")))

(define/provide-test-suite test-remove-whitespace
  (test-case
   "Test remove-whitespace (from end of line)"
   (check-equal? (remove-whitespace "") "")
   (check-equal? (remove-whitespace " ") "")
   (check-equal? (remove-whitespace "  ") "")
   (check-equal? (remove-whitespace "a") "a")
   (check-equal? (remove-whitespace " a") " a")
   (check-equal? (remove-whitespace "  a") "  a")
   (check-equal? (remove-whitespace "a ") "a")
   (check-equal? (remove-whitespace " a ") " a")
   (check-equal? (remove-whitespace " a      ") " a")
   (check-equal? (remove-whitespace " a b ") " a b")
   (check-equal? (remove-whitespace " a  b  c  ") " a  b  c")))

(define/provide-test-suite test-build-indentation
  (test-case
   "Test build-indentation (begins a new line correctly) - empty environment"
   (check-equal? (build-indentation (context 0 80 #false)) "")
   (check-equal? (build-indentation (context 4 80 #false)) "    ")
   (check-equal? (build-indentation (context 4 2 #false)) "    "))
  (test-case
   "Test build-indentation (begins a new line correctly) - comment environment"
   (check-equal? (build-indentation (context 0 80 (comment-env 0))) "/* ")
   (check-equal? (build-indentation (context 4 80 (comment-env 4))) "    /* ")
   (check-equal? (build-indentation (context 6 80 (comment-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (comment-env 4))) "    /* "))
  (test-case
   "Test build-indentation (begins a new line correctly) - macro environment"
   (check-equal? (build-indentation (context 3 80 macro-env)) "   "))
  (test-case
   "Test build-indentation (begins a new line correctly) - comment-macro environment"
   (check-equal? (build-indentation (context 4 80 (comment-macro-env 4))) "    /* ")
   (check-equal? (build-indentation (context 0 80 (comment-macro-env 0))) "/* ")
   (check-equal? (build-indentation (context 6 80 (comment-macro-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (comment-macro-env 4))) "    /* "))
  (test-case
   "Test build-indentation (begins a new line correctly) - macro-comment environment"
   (check-equal? (build-indentation (context 4 80 (macro-comment-env 4))) "    /* ")
   (check-equal? (build-indentation (context 0 80 (macro-comment-env 0))) "/* ")
   (check-equal? (build-indentation (context 6 80 (macro-comment-env 4))) "    /*   ")
   (check-equal? (build-indentation (context 2 80 (macro-comment-env 4))) "    /* ")))

(define/provide-test-suite test-finish-line
  (test-case
   "Test finish-line (ends a line correctly) - empty environment"
   (define test-context (context 0 6 #false))
   (check-equal? (finish-line "" test-context) "")
   (check-equal? (finish-line "  " test-context) "")
   (check-equal? (finish-line "1" test-context) "1")
   (check-equal? (finish-line " 2" test-context) " 2")
   (check-equal? (finish-line " 234" test-context) " 234")
   (check-equal? (finish-line " 234 " test-context) " 234")
   (check-equal? (finish-line " 234567"  test-context) " 234567")
   (check-equal? (finish-line " 234567 " test-context) " 234567")
   (check-equal? (finish-line " 234567  " test-context) " 234567"))
  (test-case
   "Test finish-line (ends a line correctly) - comment environment"
   (define test-context (context 0 6 (comment-env 0)))
   (check-equal? (finish-line "/* " test-context) "")
   (check-equal? (finish-line "/*" test-context) "")
   (check-equal? (finish-line "1" test-context) "1   */")
   (check-equal? (finish-line " 2" test-context) " 2  */")
   (check-equal? (finish-line " 234" test-context) " 234 */")
   (check-equal? (finish-line " 234 " test-context) " 234 */")
   (check-equal? (finish-line " 234567" test-context) " 234567 */")
   (check-equal? (finish-line " 234567 " test-context) " 234567 */")
   (check-equal? (finish-line " 234567  " test-context) " 234567 */"))
  (test-case
   "Test finish-line (ends a line correctly) - macro environment"
   (define test-context (context 0 6 macro-env))
   (check-equal? (finish-line "" test-context) "     \\")
   (check-equal? (finish-line "  " test-context) "     \\")
   (check-equal? (finish-line "1" test-context) "1    \\")
   (check-equal? (finish-line " 2" test-context) " 2   \\")
   (check-equal? (finish-line " 234" test-context) " 234 \\")
   (check-equal? (finish-line " 234 " test-context) " 234 \\")
   (check-equal? (finish-line " 234567" test-context) " 234567 \\")
   (check-equal? (finish-line " 234567 " test-context) " 234567 \\")
   (check-equal? (finish-line " 234567  " test-context) " 234567 \\"))
  (test-case
   "Test finish-line (ends a line correctly) - comment-macro environment"
   (define test-context (context 0 6 (comment-macro-env 0)))
   (check-equal? (finish-line "/* " test-context) "")
   (check-equal? (finish-line "/*" test-context) "")
   (check-equal? (finish-line "1" test-context) "1 \\ */")
   (check-equal? (finish-line " 2" test-context) " 2 \\ */")
   (check-equal? (finish-line " 234" test-context) " 234 \\ */")
   (check-equal? (finish-line " 234 " test-context) " 234 \\ */")
   (check-equal? (finish-line " 234567" test-context) " 234567 \\ */")
   (check-equal? (finish-line " 234567 " test-context) " 234567 \\ */")
   (check-equal? (finish-line " 234567  " test-context) " 234567 \\ */"))
  (test-case
   "Test finish-line (ends a line correctly) - macro-comment environment"
   (define test-context (context 0 6 (macro-comment-env 0)))
   (check-equal? (finish-line "/* " test-context) "     \\")
   (check-equal? (finish-line "/*" test-context) "     \\")
   (check-equal? (finish-line "1" test-context) "1 */ \\")
   (check-equal? (finish-line " 2" test-context) " 2 */ \\")
   (check-equal? (finish-line " 234" test-context) " 234 */ \\")
   (check-equal? (finish-line " 234 " test-context) " 234 */ \\")
   (check-equal? (finish-line " 234567" test-context) " 234567 */ \\")
   (check-equal? (finish-line " 234567 " test-context) " 234567 */ \\")
   (check-equal? (finish-line " 234567  " test-context) " 234567 */ \\")))

(define/provide-test-suite test-check-speculative-line-length
  (test-case
   "Test check-speculative-line-length (check against context's line length - empty string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "" test-context))
   (check-true (check-speculative-line-length 1 "" test-context))
   (check-true (check-speculative-line-length 3 "" test-context))
   (check-true (check-speculative-line-length 5 "" test-context))
   (check-true (check-speculative-line-length 6 "" test-context))
   (check-false (check-speculative-line-length 7 "" test-context))
   (check-false (check-speculative-line-length 10 "" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - single-element string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "1" test-context))
   (check-true (check-speculative-line-length 1 "1" test-context))
   (check-true (check-speculative-line-length 3 "1" test-context))
   (check-true (check-speculative-line-length 5 "1" test-context))
   (check-false (check-speculative-line-length 6 "1" test-context))
   (check-false (check-speculative-line-length 7 "1" test-context))
   (check-false (check-speculative-line-length 10 "1" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - half string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "123" test-context))
   (check-true (check-speculative-line-length 1 "123" test-context))
   (check-true (check-speculative-line-length 3 "123" test-context))
   (check-false (check-speculative-line-length 5 "123" test-context))
   (check-false (check-speculative-line-length 6 "123" test-context))
   (check-false (check-speculative-line-length 7 "123" test-context))
   (check-false (check-speculative-line-length 10 "123" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - long string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "12345" test-context))
   (check-true (check-speculative-line-length 1 "12345" test-context))
   (check-false (check-speculative-line-length 3 "12345" test-context))
   (check-false (check-speculative-line-length 5 "12345" test-context))
   (check-false (check-speculative-line-length 6 "12345" test-context))
   (check-false (check-speculative-line-length 7 "12345" test-context))
   (check-false (check-speculative-line-length 10 "12345" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - max-length string"
   (define test-context (context 0 6 #false))
   (check-true (check-speculative-line-length 0 "123456" test-context))
   (check-false (check-speculative-line-length 1 "123456" test-context))
   (check-false (check-speculative-line-length 3 "123456" test-context))
   (check-false (check-speculative-line-length 5 "123456" test-context))
   (check-false (check-speculative-line-length 6 "123456" test-context))
   (check-false (check-speculative-line-length 7 "123456" test-context))
   (check-false (check-speculative-line-length 10 "123456" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - slightly too long string"
   (define test-context (context 0 6 #false))
   (check-false (check-speculative-line-length 0 "1234567" test-context))
   (check-false (check-speculative-line-length 1 "1234567" test-context))
   (check-false (check-speculative-line-length 3 "1234567" test-context))
   (check-false (check-speculative-line-length 5 "1234567" test-context))
   (check-false (check-speculative-line-length 6 "1234567" test-context))
   (check-false (check-speculative-line-length 7 "1234567" test-context))
   (check-false (check-speculative-line-length 10 "1234567" test-context)))
  (test-case
   "Test check-speculative-line-length (check against context's line length - too long string"
   (define test-context (context 0 6 #false))
   (check-false (check-speculative-line-length 0 "1234567890" test-context))
   (check-false (check-speculative-line-length 1 "1234567890" test-context))
   (check-false (check-speculative-line-length 3 "1234567890" test-context))
   (check-false (check-speculative-line-length 5 "1234567890" test-context))
   (check-false (check-speculative-line-length 6 "1234567890" test-context))
   (check-false (check-speculative-line-length 7 "1234567890" test-context))
   (check-false (check-speculative-line-length 10 "1234567890" test-context))))

(define/provide-test-suite test-add-hash-character
  (test-case
   "Test add-hash-character"
   (check-equal? (add-hash-character "") "#")
   (check-equal? (add-hash-character " ") "#")
   (check-equal? (add-hash-character "  ") "# ")
   (check-equal? (add-hash-character " a ") " a#")
   (check-equal? (add-hash-character " a") " #")
   (check-equal? (add-hash-character "a ") "a#")
   (check-equal? (add-hash-character "  a  ") "  a #")
   (check-equal? (add-hash-character "  a") "  #")
   (check-equal? (add-hash-character "a  ") "a #")))

;nekot handler tests

(define/provide-test-suite test-add-literal
  (test-case
   "Test add-literal (writes literal nekot) - empty line - no indent - normal mode "
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'normal "" test-context "") '(""))
   (check-equal? (add-literal 'normal "abc" test-context "") '("abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - no indent - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'immediate"" test-context "") '(""))
   (check-equal? (add-literal 'immediate"abc" test-context "") '("abc"))
   (check-equal? (add-literal 'immediate"abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'immediate"abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - no indent - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'normal "" test-context "123") '("123"))
   (check-equal? (add-literal 'normal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "123") '("abcd" "123"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "123") '("abcdefg" "123")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - no indent - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "123") '("123"))
   (check-equal? (add-literal 'immediate "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - no indent - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'normal "abc" test-context "1234") '("abc" "1234"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234") '("abcd" "1234"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234") '("abcdefg" "1234")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - no indent - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234") '("1234abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234") '("1234abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234") '("1234abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - no indent - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'normal "abc" test-context "1234567") '("abc" "1234567"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234567") '("abcd" "1234567"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234567") '("abcdefg" "1234567")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - no indent - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234567") '("1234567abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234567") '("1234567abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234567") '("1234567abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - half indent - normal mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'normal "" test-context "") '(""))
   (check-equal? (add-literal 'normal "abc" test-context "") '("abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - half indent - immediate mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "") '(""))
   (check-equal? (add-literal 'immediate "abc" test-context "") '("abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - half indent - normal mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'normal "" test-context "123") '("123"))
   (check-equal? (add-literal 'normal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - half indent - immediate mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "123") '("123"))
   (check-equal? (add-literal 'immediate "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - half indent - normal mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'normal "abc" test-context "1234") '("   abc" "1234"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234") '("   abcd" "1234"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234") '("   abcdefg" "1234")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - half indent - immediate mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234") '("1234abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234") '("1234abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234") '("1234abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - half indent - normal mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'normal "abc" test-context "1234567") '("   abc" "1234567"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234567") '("   abcd" "1234567"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234567") '("   abcdefg" "1234567")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - half indent - immediate mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234567") '("1234567abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234567") '("1234567abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234567") '("1234567abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - long indent - normal mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'normal "" test-context "") '(""))
   (check-equal? (add-literal 'normal "abc" test-context "") '("abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - empty line - long indent - immediate mode"
   (define test-context (context 3 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "") '(""))
   (check-equal? (add-literal 'immediate "abc" test-context "") '("abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "") '("abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "") '("abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - long indent - normal mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'normal "" test-context "123") '("123"))
   (check-equal? (add-literal 'normal "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - half line - long indent - immediate mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "123") '("123"))
   (check-equal? (add-literal 'immediate "abc" test-context "123") '("123abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "123") '("123abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "123") '("123abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - long indent - normal mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'normal "abc" test-context "1234") '("1234abc"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234") '("1234abcd"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234") '("1234abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - long line - long indent - immediate mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234") '("1234"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234") '("1234abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234") '("1234abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234") '("1234abcdefg")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - long indent - normal mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'normal "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'normal "abc" test-context "1234567") '("      abc" "1234567"))
   (check-equal? (add-literal 'normal "abcd" test-context "1234567") '("      abcd" "1234567"))
   (check-equal? (add-literal 'normal "abcdefg" test-context "1234567") '("      abcdefg" "1234567")))
  (test-case
   "Test add-literal (writes literal nekot) - too long line - long indent - immediate mode"
   (define test-context (context 6 6 #false))
   (check-equal? (add-literal 'immediate "" test-context "1234567") '("1234567"))
   (check-equal? (add-literal 'immediate "abc" test-context "1234567") '("1234567abc"))
   (check-equal? (add-literal 'immediate "abcd" test-context "1234567") '("1234567abcd"))
   (check-equal? (add-literal 'immediate "abcdefg" test-context "1234567") '("1234567abcdefg"))))

(define/provide-test-suite test-add-spaces
  (test-case
   "Test add-spaces (writes spaces nekot) - empty line - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'normal 0 test-context "") '(""))
   (check-equal? (add-spaces 'normal 3 test-context "") '("   "))
   (check-equal? (add-spaces 'normal 4 test-context "") '("    "))
   (check-equal? (add-spaces 'normal 7 test-context "") '("" "")))
  (test-case
   "Test add-spaces (writes spaces nekot) - empty line - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'immediate 0 test-context "") '(""))
   (check-equal? (add-spaces 'immediate 3 test-context "") '("   "))
   (check-equal? (add-spaces 'immediate 4 test-context "") '("    "))
   (check-equal? (add-spaces 'immediate 7 test-context "") '("       ")))
  (test-case
   "Test add-spaces (writes spaces nekot) - half line - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'normal 0 test-context "123") '("123"))
   (check-equal? (add-spaces 'normal 3 test-context "123") '("123   "))
   (check-equal? (add-spaces 'normal 4 test-context "123") '("" "123"))
   (check-equal? (add-spaces 'normal 7 test-context "123") '("" "123")))
  (test-case
   "Test add-spaces (writes spaces nekot) - half line - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'immediate 0 test-context "123") '("123"))
   (check-equal? (add-spaces 'immediate 3 test-context "123") '("123   "))
   (check-equal? (add-spaces 'immediate 4 test-context "123") '("123    "))
   (check-equal? (add-spaces 'immediate 7 test-context "123") '("123       ")))
  (test-case
   "Test add-spaces (writes spaces nekot) - long line - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'normal 0 test-context "1234") '("1234"))
   (check-equal? (add-spaces 'normal 3 test-context "1234") '("" "1234"))
   (check-equal? (add-spaces 'normal 4 test-context "1234") '("" "1234"))
   (check-equal? (add-spaces 'normal 7 test-context "1234") '("" "1234")))
  (test-case
   "Test add-spaces (writes spaces nekot) - long line - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'immediate 0 test-context "1234") '("1234"))
   (check-equal? (add-spaces 'immediate 3 test-context "1234") '("1234   "))
   (check-equal? (add-spaces 'immediate 4 test-context "1234") '("1234    "))
   (check-equal? (add-spaces 'immediate 7 test-context "1234") '("1234       ")))
  (test-case
   "Test add-spaces (writes spaces nekot) - too long line - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'normal 0 test-context "1234567") '("1234567"))
   (check-equal? (add-spaces 'normal 3 test-context "1234567") '("" "1234567"))
   (check-equal? (add-spaces 'normal 4 test-context "1234567") '("" "1234567"))
   (check-equal? (add-spaces 'normal 7 test-context "1234567") '("" "1234567")))
  (test-case
   "Test add-spaces (writes spaces nekot) - too long line - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-spaces 'immediate 0 test-context "1234567") '("1234567"))
   (check-equal? (add-spaces 'immediate 3 test-context "1234567") '("1234567   "))
   (check-equal? (add-spaces 'immediate 4 test-context "1234567") '("1234567    "))
   (check-equal? (add-spaces 'immediate 7 test-context "1234567") '("1234567       "))))

(define/provide-test-suite test-add-new-line
  (test-case
   "Test add-new-line (starts a new line) - normal mode"
   (check-equal? (add-new-line 'normal null (construct-context 6) "") '("" ""))
   (check-equal? (add-new-line 'normal null (construct-context 6) "1") '("" "1"))
   (check-equal? (add-new-line 'normal null (construct-context 6) "123") '("" "123"))
   (check-equal? (add-new-line 'normal null (construct-context 6) "1234567") '("" "1234567")))
  (test-case
   "Test add-new-line (starts a new line) - immediate mode"
   (check-equal? (add-new-line 'immediate null (construct-context 6) "") '("" ""))
   (check-equal? (add-new-line 'immediate null (construct-context 6) "1") '("" "1"))
   (check-equal? (add-new-line 'immediate null (construct-context 6) "123") '("" "123"))
   (check-equal? (add-new-line 'immediate null (construct-context 6) "1234567") '("" "1234567"))))

(define/provide-test-suite test-add-pp-directive
  (test-case
   "Test add-pp-directive (modifies line for preprocessor directive) - normal mode"
   (check-equal? (add-pp-directive 'normal null (construct-context 6) "") '("#"))
   (check-equal? (add-pp-directive 'normal null (construct-context 6) " ") '("#"))
   (check-equal? (add-pp-directive 'normal null (construct-context 6) "  ") '("# "))
   (check-equal? (add-pp-directive 'normal null (construct-context 6) "  /* ") '("  /*#")))
  (test-case
   "Test add-pp-directive (modifies line for preprocessor directive) - immediate mode"
   (check-equal? (add-pp-directive 'immediate null (construct-context 6) "") '("#"))
   (check-equal? (add-pp-directive 'immediate null (construct-context 6) " ") '("#"))
   (check-equal? (add-pp-directive 'immediate null (construct-context 6) "  ") '("# "))
   (check-equal? (add-pp-directive 'immediate null (construct-context 6) "  /* ") '("  /*#"))))

;meta-nekot handler tests

(define/provide-test-suite test-add-empty
  (test-case
   "Test add-empty (writes empty nekot) - normal mode"
   (define test-context (context 0 6 #false))
   (define test-context-1 (context 1 6 #false))
   (check-equal? (add-empty 'normal null test-context "") '(""))
   (check-equal? (add-empty 'normal null test-context-1 " ") '(""))
   (check-equal? (add-empty 'normal null test-context "a") '("a"))
   (check-equal? (add-empty 'normal null test-context "asdf") '("asdf"))
   (check-equal? (add-empty 'normal null test-context "asdfjkl") '("asdfjkl")))
  (test-case
   "Test add-empty (writes empty nekot) - immediate mode"
   (define test-context (context 0 6 #false))
   (define test-context-1 (context 1 6 #false))
   (check-equal? (add-empty 'immediate null test-context "") '(""))
   (check-equal? (add-empty 'immediate null test-context-1 " ") '(""))
   (check-equal? (add-empty 'immediate null test-context "a") '("a"))
   (check-equal? (add-empty 'immediate null test-context "asdf") '("asdf"))
   (check-equal? (add-empty 'immediate null test-context "asdfjkl") '("asdfjkl"))))

(define/provide-test-suite test-add-concatenated
  (test-case
   "Test add-concatenated (writes sequence of nekots) - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-concatenated 'normal
                                   (list (nekot 'literal "asdf" test-context)
                                         (nekot 'new-line null test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("  asdf" "asdf"))
   (check-equal? (add-concatenated 'normal
                                   (list (nekot 'literal "asdf" test-context)
                                         (nekot 'new-line null test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'pp-directive null test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("# asdf" "asdf")))
  (test-case
   "Test add-concatenated (writes sequence of nekots) - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-concatenated 'immediate
                                   (list (nekot 'literal "asdf" test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("asdf  asdf"))
   (check-equal? (add-concatenated 'immediate
                                   (list (nekot 'literal "asdf" test-context)
                                         (nekot 'spaces 2 test-context)
                                         (nekot 'pp-directive null test-context)
                                         (nekot 'literal "asdf" test-context))
                                   test-context
                                   "")
                 '("asdf #asdf"))))

(define/provide-test-suite test-add-immediate
  (test-case
   "Test add-immediate (writes nekots without pretty printing) - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-immediate 'normal
                                (nekot 'concat
                                       (list (nekot 'literal "asdf" test-context)
                                             (nekot 'spaces 2 test-context)
                                             (nekot 'literal "asdf" test-context))
                                       test-context)
                                test-context
                                "")
                 '("asdf  asdf")))
  (test-case
   "Test add-immediate (writes nekots without pretty printing) - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (add-immediate 'immediate
                                (nekot 'concat
                                       (list (nekot 'literal "asdf" test-context)
                                             (nekot 'spaces 2 test-context)
                                             (nekot 'literal "asdf" test-context))
                                       test-context)
                                test-context
                                "")
                 '("asdf  asdf"))))

(define/provide-test-suite test-add-speculative
  (test-case
   "Test add-speculative (writes nekots conditionally) - normal mode"
   (define test-context (context 0 20 #false))
   (define test-true? (λ (any) #true))
   (define test-false? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (define check-length-2 (λ (lst) (= 2 (length lst))))
   (check-equal? (add-speculative 'normal
                                  (list (nekot 'literal "asdf" test-context)
                                        test-true?
                                        (nekot 'literal "jkl" test-context))
                                  test-context
                                  "")
                 '("asdf"))
   (check-equal? (add-speculative 'normal
                                  (list (nekot 'literal "asdf" test-context)
                                        test-false?
                                        (nekot 'literal "jkl" test-context))
                                  test-context
                                  "")
                 '("jkl"))
   (check-equal? (add-speculative 'normal
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'new-line null test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("asdf  jkl"))
   (check-equal? (add-speculative 'normal
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 1 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("asdf jkl"))
   (check-equal? (add-speculative 'normal
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'new-line null test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length-2
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("jkl" "asdf")))  (test-case
   "Test add-speculative (writes nekots conditionally) - immediate mode"
   (define test-context (context 0 6 #false))
   (define test-true? (λ (any) #true))
   (define test-false? (λ (any) #false))
   (define check-length (λ (lst) (= 1 (length lst))))
   (define check-length-2 (λ (lst) (= 2 (length lst))))
   (check-equal? (add-speculative 'immediate
                                  (list (nekot 'literal "asdf" test-context)
                                        test-true?
                                        (nekot 'literal "jkl" test-context))
                                  test-context
                                  "")
                 '("asdf"))
   (check-equal? (add-speculative 'immediate
                                  (list (nekot 'literal "asdf" test-context)
                                        test-false?
                                        (nekot 'literal "jkl" test-context))
                                  test-context
                                  "")
                 '("jkl"))
   (check-equal? (add-speculative 'immediate
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'new-line null test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("asdf  jkl"))
   (check-equal? (add-speculative 'immediate
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 1 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("asdf jkl"))
   (check-equal? (add-speculative 'immediate
                                  (list (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'new-line null test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context)
                                        check-length-2
                                        (nekot 'concat
                                               (list (nekot 'literal "asdf" test-context)
                                                     (nekot 'spaces 2 test-context)
                                                     (nekot 'literal "jkl" test-context))
                                               test-context))
                                  test-context
                                  "")
                 '("jkl" "asdf"))))

(define/provide-test-suite test-change-indent-to-current
  (test-case
   "Test change-indent-to-current (sets indent to length of current line - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (change-indent-to-current 'normal
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           test-context
                                           "")
                 '("asdf" ""))
   (check-equal? (change-indent-to-current 'normal
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           test-context
                                           "jkl")
                 '("   asdf" "jkl"))
   (check-equal? (change-indent-to-current 'normal
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           (context 5 6 #false)
                                           "jkl")
                 '("     asdf" "jkl")))
  (test-case
   "Test change-indent-to-current (sets indent to length of current line - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (change-indent-to-current 'immediate
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           test-context
                                           "")
                 '("asdf" ""))
   (check-equal? (change-indent-to-current 'immediate
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           test-context
                                           "jkl")
                 '("   asdf" "jkl"))
   (check-equal? (change-indent-to-current 'immediate
                                           (concat-chunk new-line-chunk
                                                         (literal-chunk "asdf"))
                                           (context 5 6 #false)
                                           "jkl")
                 '("     asdf" "jkl"))))

(define/provide-test-suite test-unknown-nekot-type
  (test-case
   "Test unknown-nekot-type (writes unknown nekot - raises error)"
   (check-exn exn:fail? (λ () (unknown-nekot-type 'normal "FAIL" (construct-context 6) '(""))))
   (check-exn exn:fail? (λ () (unknown-nekot-type 'immediate "FAIL" (construct-context 6) '(""))))))

(define/provide-test-suite test-write-nekot
  (test-case
   "Test write-nekot (writes any nekot) - implied mode"
   (define test-context (context 0 6 #false))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context))
                 '("  asdf" "asdf"))
   (check-equal? (write-nekot (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'pp-directive null test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context))
                 '("# asdf" "asdf")))
  (test-case
   "Test write-nekot (writes any nekot) - normal mode"
   (define test-context (context 0 6 #false))
   (check-equal? (write-nekot 'normal
                              (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "12")
                 '("  asdf" "12asdf"))
   (check-equal? (write-nekot 'normal 
                              (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'pp-directive null test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "123")
                 '("# asdf" "asdf" "123")))
  (test-case
   "Test write-nekot (writes any nekot) - immediate mode"
   (define test-context (context 0 6 #false))
   (check-equal? (write-nekot 'immediate
                              (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "12")
                 '("  asdf" "12asdf"))
   (check-equal? (write-nekot 'immediate
                              (nekot 'concat
                                     (list (nekot 'literal "asdf" test-context)
                                           (nekot 'new-line null test-context)
                                           (nekot 'spaces 2 test-context)
                                           (nekot 'pp-directive null test-context)
                                           (nekot 'literal "asdf" test-context))
                                     test-context)
                              "123")
                 '("# asdf" "123asdf"))))

