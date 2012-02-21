#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")
(require "../src/core-chunk.rkt")
(require "../src/standard-chunk.rkt")
(require "../src/writer.rkt")

;unit tests for standard-chunk.rkt

;character chunks

(define/provide-test-suite test-character-chunks
  (test-case
   "Test space-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform space-chunk
                                               test-context))
                 '(" "))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             space-chunk)
                                               test-context))
                 '("1 "))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             space-chunk)
                                               test-context))
                 '("" "123456")))
  (test-case
   "Test imm-space-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-space-chunk
                                               test-context))
                 '(" "))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-space-chunk)
                                               test-context))
                 '("1 "))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-space-chunk)
                                               test-context))
                 '("123456 ")))
  (test-case
   "Test blank-lines-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             (blank-lines-chunk 1))
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             (blank-lines-chunk 3))
                                               test-context))
                 '("" "" "" "" "1")))
  (test-case
   "Test blank-line-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             blank-line-chunk)
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123"
                                                             blank-line-chunk)
                                               test-context))
                 '("" "" "123")))
  (test-case
   "Test open-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-paren-chunk
                                               test-context))
                 '("("))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             open-paren-chunk)
                                               test-context))
                 '("1("))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             open-paren-chunk)
                                               test-context))
                 '("(" "123456")))
  (test-case
   "Test imm-open-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-paren-chunk
                                               test-context))
                 '("("))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-open-paren-chunk)
                                               test-context))
                 '("1("))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-open-paren-chunk)
                                               test-context))
                 '("123456(")))
  (test-case
   "Test close-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-paren-chunk
                                               test-context))
                 '(")"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             close-paren-chunk)
                                               test-context))
                 '("1)"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             close-paren-chunk)
                                               test-context))
                 '(")" "123456")))
  (test-case
   "Test imm-close-paren-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-paren-chunk
                                               test-context))
                 '(")"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-close-paren-chunk)
                                               test-context))
                 '("1)"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-close-paren-chunk)
                                               test-context))
                 '("123456)")))
  (test-case
   "Test open-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-crbr-chunk
                                               test-context))
                 '("{"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             open-crbr-chunk)
                                               test-context))
                 '("1{"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             open-crbr-chunk)
                                               test-context))
                 '("{" "123456")))
  (test-case
   "Test imm-open-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-crbr-chunk
                                               test-context))
                 '("{"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-open-crbr-chunk)
                                               test-context))
                 '("1{"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-open-crbr-chunk)
                                               test-context))
                 '("123456{")))
  (test-case
   "Test close-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-crbr-chunk
                                               test-context))
                 '("}"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             close-crbr-chunk)
                                               test-context))
                 '("1}"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             close-crbr-chunk)
                                               test-context))
                 '("}" "123456")))
  (test-case
   "Test imm-close-crbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-crbr-chunk
                                               test-context))
                 '("}"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-close-crbr-chunk)
                                               test-context))
                 '("1}"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-close-crbr-chunk)
                                               test-context))
                 '("123456}")))
  (test-case
   "Test open-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-anbr-chunk
                                               test-context))
                 '("<"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             open-anbr-chunk)
                                               test-context))
                 '("1<"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             open-anbr-chunk)
                                               test-context))
                 '("<" "123456")))
  (test-case
   "Test imm-open-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-anbr-chunk
                                               test-context))
                 '("<"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-open-anbr-chunk)
                                               test-context))
                 '("1<"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-open-anbr-chunk)
                                               test-context))
                 '("123456<")))
  (test-case
   "Test close-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-anbr-chunk
                                               test-context))
                 '(">"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             close-anbr-chunk)
                                               test-context))
                 '("1>"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             close-anbr-chunk)
                                               test-context))
                 '(">" "123456")))
  (test-case
   "Test imm-close-anbr-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-anbr-chunk
                                               test-context))
                 '(">"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-close-anbr-chunk)
                                               test-context))
                 '("1>"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-close-anbr-chunk)
                                               test-context))
                 '("123456>")))
  (test-case
   "Test comma-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform comma-chunk
                                               test-context))
                 '(","))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             comma-chunk)
                                               test-context))
                 '("1,"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             comma-chunk)
                                               test-context))
                 '("," "123456")))
  (test-case
   "Test imm-comma-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-comma-chunk
                                               test-context))
                 '(","))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-comma-chunk)
                                               test-context))
                 '("1,"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-comma-chunk)
                                               test-context))
                 '("123456,")))
  (test-case
   "Test period-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform period-chunk
                                               test-context))
                 '("."))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             period-chunk)
                                               test-context))
                 '("1."))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             period-chunk)
                                               test-context))
                 '("." "123456")))
  (test-case
   "Test imm-period-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-period-chunk
                                               test-context))
                 '("."))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-period-chunk)
                                               test-context))
                 '("1."))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-period-chunk)
                                               test-context))
                 '("123456.")))
  (test-case
   "Test colon-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform colon-chunk
                                               test-context))
                 '(":"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             colon-chunk)
                                               test-context))
                 '("1:"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             colon-chunk)
                                               test-context))
                 '(":" "123456")))
  (test-case
   "Test imm-colon-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-colon-chunk
                                               test-context))
                 '(":"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-colon-chunk)
                                               test-context))
                 '("1:"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-colon-chunk)
                                               test-context))
                 '("123456:")))
  (test-case
   "Test semi-colon-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform semi-colon-chunk
                                               test-context))
                 '(";"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             semi-colon-chunk)
                                               test-context))
                 '("1;"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             semi-colon-chunk)
                                               test-context))
                 '(";" "123456")))
  (test-case
   "Test imm-semi-colon-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-semi-colon-chunk
                                               test-context))
                 '(";"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-semi-colon-chunk)
                                               test-context))
                 '("1;"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-semi-colon-chunk)
                                               test-context))
                 '("123456;"))))

;keyword chunks

(define/provide-test-suite test-keyword-chunks
  (test-case
   "Test define-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform define-chunk
                                               test-context))
                 '("define"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             define-chunk)
                                               test-context))
                 '("1define"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             define-chunk)
                                               test-context))
                 '("define" "123456")))
  (test-case
   "Test imm-define-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-define-chunk
                                               test-context))
                 '("define"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-define-chunk)
                                               test-context))
                 '("1define"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-define-chunk)
                                               test-context))
                 '("123456define")))
  (test-case
   "Test include-chunk"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform include-chunk
                                               test-context))
                 '("include"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             include-chunk)
                                               test-context))
                 '("1include"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             include-chunk)
                                               test-context))
                 '("include" "123456")))
  (test-case
   "Test imm-include-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-include-chunk
                                               test-context))
                 '("include"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-include-chunk)
                                               test-context))
                 '("1include"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-include-chunk)
                                               test-context))
                 '("123456include")))
  (test-case
   "Test ifdef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform ifdef-chunk
                                               test-context))
                 '("ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             ifdef-chunk)
                                               test-context))
                 '("1ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             ifdef-chunk)
                                               test-context))
                 '("ifdef" "123456")))
  (test-case
   "Test imm-ifdef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-ifdef-chunk
                                               test-context))
                 '("ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-ifdef-chunk)
                                               test-context))
                 '("1ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-ifdef-chunk)
                                               test-context))
                 '("123456ifdef")))
  (test-case
   "Test ifndef-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform ifndef-chunk
                                               test-context))
                 '("ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             ifndef-chunk)
                                               test-context))
                 '("1ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             ifndef-chunk)
                                               test-context))
                 '("ifndef" "123456")))
  (test-case
   "Test imm-ifndef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-ifndef-chunk
                                               test-context))
                 '("ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-ifndef-chunk)
                                               test-context))
                 '("1ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-ifndef-chunk)
                                               test-context))
                 '("123456ifndef")))
  (test-case
   "Test else-chunk"
   (define test-context (construct-context 5))
   (check-equal? (write-nekot (chunk-transform else-chunk
                                               test-context))
                 '("else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             else-chunk)
                                               test-context))
                 '("1else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             else-chunk)
                                               test-context))
                 '("else" "123456")))
  (test-case
   "Test imm-else-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-else-chunk
                                               test-context))
                 '("else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-else-chunk)
                                               test-context))
                 '("1else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-else-chunk)
                                               test-context))
                 '("123456else")))
  (test-case
   "Test endif-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform endif-chunk
                                               test-context))
                 '("endif"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             endif-chunk)
                                               test-context))
                 '("1endif"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             endif-chunk)
                                               test-context))
                 '("endif" "123456")))
  (test-case
   "Test imm-endif-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-endif-chunk
                                               test-context))
                 '("endif"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-endif-chunk)
                                               test-context))
                 '("1endif"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-endif-chunk)
                                               test-context))
                 '("123456endif")))
  (test-case
   "Test template-chunk"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot (chunk-transform template-chunk
                                               test-context))
                 '("template"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             template-chunk)
                                               test-context))
                 '("1template"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             template-chunk)
                                               test-context))
                 '("template" "123456")))
  (test-case
   "Test imm-template-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-template-chunk
                                               test-context))
                 '("template"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-template-chunk)
                                               test-context))
                 '("1template"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-template-chunk)
                                               test-context))
                 '("123456template")))
  (test-case
   "Test typename-chunk"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot (chunk-transform typename-chunk
                                               test-context))
                 '("typename"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             typename-chunk)
                                               test-context))
                 '("1typename"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             typename-chunk)
                                               test-context))
                 '("typename" "123456")))
  (test-case
   "Test imm-typename-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-typename-chunk
                                               test-context))
                 '("typename"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-typename-chunk)
                                               test-context))
                 '("1typename"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-typename-chunk)
                                               test-context))
                 '("123456typename")))
  (test-case
   "Test typedef-chunk"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform typedef-chunk
                                               test-context))
                 '("typedef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             typedef-chunk)
                                               test-context))
                 '("1typedef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             typedef-chunk)
                                               test-context))
                 '("typedef" "123456")))
  (test-case
   "Test imm-typedef-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-typedef-chunk
                                               test-context))
                 '("typedef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-typedef-chunk)
                                               test-context))
                 '("1typedef"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-typedef-chunk)
                                               test-context))
                 '("123456typedef")))
  (test-case
   "Test void-chunk"
   (define test-context (construct-context 5))
   (check-equal? (write-nekot (chunk-transform void-chunk
                                               test-context))
                 '("void"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             void-chunk)
                                               test-context))
                 '("1void"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             void-chunk)
                                               test-context))
                 '("void" "123456")))
  (test-case
   "Test imm-void-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-void-chunk
                                               test-context))
                 '("void"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-void-chunk)
                                               test-context))
                 '("1void"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-void-chunk)
                                               test-context))
                 '("123456void")))
  (test-case
   "Test inline-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform inline-chunk
                                               test-context))
                 '("inline"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             inline-chunk)
                                               test-context))
                 '("1inline"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             inline-chunk)
                                               test-context))
                 '("inline" "123456")))
  (test-case
   "Test imm-inline-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-inline-chunk
                                               test-context))
                 '("inline"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-inline-chunk)
                                               test-context))
                 '("1inline"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-inline-chunk)
                                               test-context))
                 '("123456inline")))
  (test-case
   "Test static-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform static-chunk
                                               test-context))
                 '("static"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             static-chunk)
                                               test-context))
                 '("1static"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             static-chunk)
                                               test-context))
                 '("static" "123456")))
  (test-case
   "Test imm-static-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-static-chunk
                                               test-context))
                 '("static"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-static-chunk)
                                               test-context))
                 '("1static"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-static-chunk)
                                               test-context))
                 '("123456static")))
  (test-case
   "Test return-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform return-chunk
                                               test-context))
                 '("return"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             return-chunk)
                                               test-context))
                 '("1return"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             return-chunk)
                                               test-context))
                 '("return" "123456")))
  (test-case
   "Test imm-return-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-return-chunk
                                               test-context))
                 '("return"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-return-chunk)
                                               test-context))
                 '("1return"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-return-chunk)
                                               test-context))
                 '("123456return")))
  (test-case
   "Test const-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform const-chunk
                                               test-context))
                 '("const"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             const-chunk)
                                               test-context))
                 '("1const"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             const-chunk)
                                               test-context))
                 '("const" "123456")))
  (test-case
   "Test imm-const-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-const-chunk
                                               test-context))
                 '("const"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-const-chunk)
                                               test-context))
                 '("1const"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-const-chunk)
                                               test-context))
                 '("123456const")))
  (test-case
   "Test struct-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform struct-chunk
                                               test-context))
                 '("struct"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             struct-chunk)
                                               test-context))
                 '("1struct"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             struct-chunk)
                                               test-context))
                 '("struct" "123456")))
  (test-case
   "Test imm-struct-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-struct-chunk
                                               test-context))
                 '("struct"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-struct-chunk)
                                               test-context))
                 '("1struct"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-struct-chunk)
                                               test-context))
                 '("123456struct")))
  (test-case
   "Test class-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform class-chunk
                                               test-context))
                 '("class"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             class-chunk)
                                               test-context))
                 '("1class"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             class-chunk)
                                               test-context))
                 '("class" "123456")))
  (test-case
   "Test imm-class-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-class-chunk
                                               test-context))
                 '("class"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-class-chunk)
                                               test-context))
                 '("1class"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-class-chunk)
                                               test-context))
                 '("123456class")))
  (test-case
   "Test public-chunk"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform public-chunk
                                               test-context))
                 '("public"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             public-chunk)
                                               test-context))
                 '("1public"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             public-chunk)
                                               test-context))
                 '("public" "123456")))
  (test-case
   "Test imm-public-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-public-chunk
                                               test-context))
                 '("public"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-public-chunk)
                                               test-context))
                 '("1public"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-public-chunk)
                                               test-context))
                 '("123456public")))
  (test-case
   "Test protected-chunk"
   (define test-context (construct-context 10))
   (check-equal? (write-nekot (chunk-transform protected-chunk
                                               test-context))
                 '("protected"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             protected-chunk)
                                               test-context))
                 '("1protected"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             protected-chunk)
                                               test-context))
                 '("protected" "123456")))
  (test-case
   "Test imm-protected-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-protected-chunk
                                               test-context))
                 '("protected"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-protected-chunk)
                                               test-context))
                 '("1protected"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-protected-chunk)
                                               test-context))
                 '("123456protected")))
  (test-case
   "Test private-chunk"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform private-chunk
                                               test-context))
                 '("private"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             private-chunk)
                                               test-context))
                 '("1private"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             private-chunk)
                                               test-context))
                 '("private" "123456")))
  (test-case
   "Test imm-private-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-private-chunk
                                               test-context))
                 '("private"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-private-chunk)
                                               test-context))
                 '("1private"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-private-chunk)
                                               test-context))
                 '("123456private")))
  (test-case
   "Test namespace-chunk"
   (define test-context (construct-context 10))
   (check-equal? (write-nekot (chunk-transform namespace-chunk
                                               test-context))
                 '("namespace"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             namespace-chunk)
                                               test-context))
                 '("1namespace"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             namespace-chunk)
                                               test-context))
                 '("namespace" "123456")))
  (test-case
   "Test imm-namespace-chunk"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-namespace-chunk
                                               test-context))
                 '("namespace"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "1"
                                                             imm-namespace-chunk)
                                               test-context))
                 '("1namespace"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "123456"
                                                             imm-namespace-chunk)
                                               test-context))
                 '("123456namespace"))))

;list chunks

(define/provide-test-suite test-attach-list-separator
  (test-case
   "Test attach-list-separator"
   (define test-context (construct-context 80))
   (check-equal? (attach-list-separator comma-chunk)
                 null)
   (check-equal? (write-nekot (chunk-transform (concat-chunk (attach-list-separator comma-chunk
                                                                    'asdf
                                                                    'jkl))
                               test-context))
                 '("asdf,jkl"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (attach-list-separator comma-chunk
                                                                   'asdf))
                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (attach-list-separator comma-chunk
                                                                    'asdf
                                                                    'jkl
                                                                    "12345"))
                               test-context))
                 '("asdf,jkl,12345"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (attach-list-separator comma-chunk
                                                                    'asdf
                                                                    "123"
                                                                    "12345"
                                                                    "1"))
                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-between-chunk
  (test-case
   "Test between-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between-chunk space-chunk)
                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between-chunk space-chunk
                                              'asdf
                                              'jkl)
                               test-context))
                 '("asdf jkl"))
   (check-equal? (write-nekot (chunk-transform (between-chunk space-chunk
                                              'asdf)
                              test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between-chunk space-chunk
                                              'asdf
                                              'jkl
                                              "12345")
                              test-context))
                 '("asdf jkl 12345"))
   (check-equal? (write-nekot (chunk-transform (between-chunk new-line-chunk
                                              'asdf
                                              "123"
                                              "12345"
                                              "1")
                               test-context))
                 '("1" "12345" "123" "asdf"))))

(define/provide-test-suite test-between/attach-chunk
  (test-case
   "Test between/attach-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk)
                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk
                                                     'asdf
                                                     'jkl)
                               test-context))
                 '("asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk
                                                     'asdf)
                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk
                                                     'asdf
                                                     'jkl
                                                     "12345")
                               test-context))
                 '("asdf, jkl, 12345"))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk
                                                     'asdf
                                                     "123"
                                                     "12345"
                                                     "1")
                               test-context))
                 '("asdf, 123, 12345, 1"))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     new-line-chunk
                                                     'asdf
                                                     "123"
                                                     "12345"
                                                     "1")
                               test-context))
                 '("1" "12345," "123," "asdf,"))
   (check-equal? (write-nekot (chunk-transform (between/attach-chunk comma-chunk
                                                     space-chunk
                                                     'asdf
                                                     "123"
                                                     "12345"
                                                     "1")
                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-arg-list-chunk
  (test-case
   "Test arg-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (arg-list-chunk open-paren-chunk
                                               comma-chunk
                                               close-paren-chunk
                                               'asdf)
                               test-context))
                 '("(asdf)"))
   (check-equal? (write-nekot (chunk-transform (arg-list-chunk open-paren-chunk
                                               comma-chunk
                                               close-paren-chunk
                                               (concat-chunk 'asdf
                                                             'jkl))
                               test-context))
                 '(" jkl)"
                   "(asdf"))
   (check-equal? (write-nekot (chunk-transform (arg-list-chunk open-paren-chunk
                                               comma-chunk
                                               close-paren-chunk
                                               'asdf
                                               empty-chunk
                                               'jkl)
                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (arg-list-chunk open-paren-chunk
                                               comma-chunk
                                               close-paren-chunk
                                               'asdf
                                               empty-chunk
                                               'jkl)
                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-paren-list-chunk
  (test-case
   "Test paren-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (paren-list-chunk 'asdf
                                                 empty-chunk
                                                 'jkl)
                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (paren-list-chunk 'asdf
                                                 empty-chunk
                                                 'jkl)
                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-template-list-chunk
  (test-case
   "Test template-list-chunk"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-list-chunk 'asdf
                                                    empty-chunk
                                                    'jkl)
                               test-context))
                 '(" jkl>" " ," "<asdf,"))
   (check-equal? (write-nekot (chunk-transform (template-list-chunk 'asdf
                                                    empty-chunk
                                                    'jkl)
                               test-context-2))
                 '("<asdf, , jkl>"))))

(define/provide-test-suite test-smt-list-chunk
  (test-case
   "Test smt-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (smt-list-chunk blank-line-chunk
                                               'asdf
                                               space-chunk
                                               'jkl)
                               test-context))
                 '("jkl" "" " ;" "" "asdf;"))
   (check-equal? (write-nekot (chunk-transform (smt-list-chunk new-line-chunk
                                               (spaces-chunk 4)
                                               'asdf
                                               empty-chunk)
                               test-context))
                 '("" "asdf;" "    ;"))))

(define/provide-test-suite test-top-smt-list-chunk
  (test-case
   "Test top-smt-list-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (top-smt-list-chunk blank-line-chunk
                                               'asdf
                                               space-chunk
                                               'jkl)
                               test-context))
                 '("jkl;" "" " ;" "" "asdf;"))
   (check-equal? (write-nekot (chunk-transform (top-smt-list-chunk new-line-chunk
                                               (spaces-chunk 4)
                                               'asdf
                                               empty-chunk)
                               test-context))
                 '(";" "asdf;" "    ;"))))

(define/provide-test-suite test-constructor-assignment-list-chunk
  (test-case
   "Test constructor-assignment-list-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list-chunk)
                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list-chunk 'asdf)
                               test-context))
                 '("  : asdf"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list-chunk 'asdf
                                                                  'jkl)
                               test-context))
                 '("  : asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list-chunk 'asdf
                                                                  'jkl)
                               test-context-2))
                 '("    jkl"
                   "  : asdf,"))))

(define/provide-test-suite test-body-chunk
  (test-case
   "Test body-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (body-chunk 'asdf
                                           space-chunk
                                           'jkl)
                               test-context))
                 '("{ asdf;  ; jkl; }"))
   (check-equal? (write-nekot (chunk-transform (body-chunk "123456")
                               test-context-2))
                 '("}"
                   "   123456;"
                   "{"))
   (check-equal? (write-nekot (chunk-transform (body-chunk 'asdf
                                           space-chunk
                                           'jkl)
                               test-context-2))
                 '("}"
                   "   jkl;"
                   ""
                   "    ;"
                   ""
                   "   asdf;"
                   ""
                   "{"))))

;preprocessor chunks

(define/provide-test-suite test-pp-define-chunk
  (test-case
   "Test pp-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-define-chunk 'name) test-context)) '("#define name"))
   (check-equal? (write-nekot (chunk-transform (pp-define-chunk (concat-chunk 'name "2")) test-context)) '("#define name2"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) (pp-define-chunk 'name)) test-context)) '("#  define name"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " (pp-define-chunk 'name)) test-context)) '("/*#define name"))))

(define/provide-test-suite test-pp-include-chunk
  (test-case
   "Test pp-include-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-include-chunk 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-include-chunk (concat-chunk 'name "2")) test-context)) '("#include <name2>"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) (pp-include-chunk 'name)) test-context)) '("#  include <name>"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " (pp-include-chunk 'name)) test-context)) '("/*#include <name>"))))

(define/provide-test-suite test-pp-includes-chunk
  (test-case
   "Test pp-includes-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-includes-chunk 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-includes-chunk 'name
                                               'name2)
                               test-context))
                 '("#include <name2>"
                   "#include <name>"))))

(define/provide-test-suite test-pp-ifdef-chunk
  (test-case
   "Test pp-ifdef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef-chunk 'condition) test-context)) '("#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef-chunk (concat-chunk 'condition "2")) test-context)) '("#ifdef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) (pp-ifdef-chunk 'condition)) test-context)) '("#  ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " (pp-ifdef-chunk 'condition)) test-context)) '("/*#ifdef condition"))))

(define/provide-test-suite test-pp-ifndef-chunk
  (test-case
   "Test pp-ifndef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef-chunk 'condition) test-context)) '("#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef-chunk (concat-chunk 'condition "2")) test-context)) '("#ifndef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) (pp-ifndef-chunk 'condition)) test-context)) '("#  ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " (pp-ifndef-chunk 'condition)) test-context)) '("/*#ifndef condition"))))

(define/provide-test-suite test-pp-else-chunk
  (test-case
   "Test pp-else-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform pp-else-chunk test-context)) '("#else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) pp-else-chunk) test-context)) '("#  else"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " pp-else-chunk) test-context)) '("/*#else"))))

(define/provide-test-suite test-pp-endif-chunk
  (test-case
   "Test pp-endif-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-endif-chunk 'condition) test-context)) '("#endif /* condition */"))
   (check-equal? (write-nekot (chunk-transform (pp-endif-chunk (concat-chunk 'condition "2")) test-context)) '("#endif /* condition2 */"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk (spaces-chunk 3) (pp-endif-chunk 'condition)) test-context)) '("#  endif /* condition */"))
   (check-equal? (write-nekot (chunk-transform (concat-chunk "/* " (pp-endif-chunk 'condition)) test-context)) '("/*#endif /* condition */"))))

(define/provide-test-suite test-pp-conditional-chunk
  (test-case
   "Test pp-conditional-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-chunk ifdef-chunk
                                                     'condition
                                                     'then)
                               test-context))
                 '("#endif /* condition */"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-chunk ifndef-chunk
                                                     'condition
                                                     'then
                                                     'else2)
                               test-context))
                 '("#endif /* condition */"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-conditional-ifdef-chunk
  (test-case
   "Test pp-conditional-ifdef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef-chunk 'condition
                                                           'then)
                               test-context))
                 '("#endif /* condition */"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef-chunk 'condition
                                                           'then
                                                           'else2)
                               test-context))
                 '("#endif /* condition */"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifdef condition"))))

(define/provide-test-suite test-pp-conditional-ifndef-chunk
  (test-case
   "Test pp-conditional-ifndef-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef-chunk 'condition
                                                           'then)
                               test-context))
                 '("#endif /* condition */"
                   "   then"
                   "#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef-chunk 'condition
                                                           'then
                                                           'else2)
                               test-context))
                 '("#endif /* condition */"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-header-file-chunk
  (test-case
   "Test pp-header-file-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-header-file-chunk 'header_file
                                                     empty-chunk
                                                     'asdf
                                                     'jkl)
                               test-context))
                 '("#endif /* header_file */"
                   ""
                   "   jkl;"
                   ""
                   "   asdf;"
                   ""
                   ""
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-nekot (chunk-transform (pp-header-file-chunk 'header_file
                                                     (concat-chunk (pp-include-chunk 'iostream)
                                                                   new-line-chunk
                                                                   (pp-include-chunk 'algorithm))
                                                     'asdf
                                                     'jkl)
                               test-context))
                 '("#endif /* header_file */"
                   ""
                   "   jkl;"
                   ""
                   "   asdf;"
                   ""
                   "#  include <algorithm>"
                   "#  include <iostream>"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))))

(define/provide-test-suite test-macro-define-chunk
  (test-case
   "Test macro-defintion-chunk"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 20))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   null
                                                   'asdf)
                               test-context))
                 '("#define name asdf"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list 'first)
                                                   (concat-chunk 'asdf
                                                                 space-chunk
                                                                 'first))
                               test-context))
                 '("#define name(first) asdf first"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list 'first)
                                                   (concat-chunk 'asdf
                                                                 space-chunk
                                                                 'first))
                               test-context-2))
                 '("   asdf first" "#define name(first) \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list 'first
                                                         'second)
                                                   (concat-chunk 'asdf
                                                                 space-chunk
                                                                 'first
                                                                 space-chunk
                                                                 'second))
                               test-context))
                 '("#define name(first, second) asdf first second"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list 'first
                                                         'second)
                                                   (concat-chunk 'asdf
                                                                 new-line-chunk
                                                                 'first
                                                                 new-line-chunk
                                                                 'second))
                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list (concat-chunk (comment-env-chunk 't)
                                                                       new-line-chunk
                                                                       'first)
                                                         'second)
                                                   (concat-chunk 'asdf
                                                                 new-line-chunk
                                                                 'first
                                                                 new-line-chunk
                                                                 'second))
                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             first, \\"
                   "#define name(/* t */ \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list 'first
                                                         (concat-chunk (comment-env-chunk 't)
                                                                       new-line-chunk
                                                                       'second))
                                                   (concat-chunk 'asdf
                                                                 new-line-chunk
                                                                 'first
                                                                 new-line-chunk
                                                                 'second))
                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             /* t */ \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define-chunk 'name
                                                   (list (concat-chunk (comment-env-chunk 't)
                                                                       new-line-chunk
                                                                       'first)
                                                         (concat-chunk (comment-env-chunk 'T)
                                                                       new-line-chunk
                                                                       'second))
                                                   (concat-chunk 'asdf
                                                                 new-line-chunk
                                                                 'first
                                                                 new-line-chunk
                                                                 'second))
                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             /* T */ \\"
                   "             first, \\"
                   "#define name(/* t */ \\"))))

;general chunks

(define/provide-test-suite test-namespace-define-chunk
  (test-case
   "Test namespace-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (namespace-define-chunk 'name
                                                       'asdf)
                               test-context))
                 '("namespace name { asdf; }"))
   (check-equal? (write-nekot (chunk-transform (namespace-define-chunk 'name
                                                       'asdf)
                               (construct-context 12)))
                 '("} /* name */"
                   "   asdf;"
                   "namespace name {"))
   (check-equal? (write-nekot (chunk-transform (namespace-define-chunk 'name
                                                       'asdf
                                                       'jkl)
                               (construct-context 12)))
                 '("} /* name */"
                   "   jkl;"
                   ""
                   "   asdf;"
                   ""
                   "namespace name {"))))

(define/provide-test-suite test-described-smts-chunk
  (test-case
   "Test described-smts-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (described-smts-chunk 'name
                                                     'asdf)
                               test-context))
                 '("asdf"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts-chunk 'name
                                                     'asdf
                                                     'jkl)
                               test-context))
                 '("jkl"
                   "asdf;"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts-chunk 'name
                                                     'asdf
                                                     'jkl
                                                     "1234")
                               test-context))
                 '("1234"
                   "jkl;"
                   "asdf;"
                   "/* name */"))))

(define/provide-test-suite test-constize-chunk
  (test-case
   "Test constize-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constize-chunk 'asdf)
                               test-context))
                 '("asdf const"))
   (check-equal? (write-nekot (chunk-transform (constize-chunk 'asdf)
                               (construct-context 4)))
                 '("asdf const"))))

;template chunks

(define/provide-test-suite test-template-define-chunk
  (test-case
   "Test template-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-define-chunk null
                                                      'asdf)
                               test-context))
                 '(" asdf"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-define-chunk (list 'first)
                                                      'asdf)
                               test-context))
                 '(" asdf"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-define-chunk (list 'first
                                                            'second)
                                                      'asdf)
                               test-context))
                 '(" asdf"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-define-chunk (list (concat-chunk (comment-env-chunk 'test)
                                                                          new-line-chunk
                                                                          'first)
                                                            'second)
                                                      'asdf)
                               test-context))
                 '(" asdf"
                   "         second>"
                   "         first,"
                   "template</* test */"))))

(define/provide-test-suite test-template-use-chunk
  (test-case
   "Test template-defintion-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-use-chunk 'name
                                                   null)
                               test-context))
                 '("name"))
   (check-equal? (write-nekot (chunk-transform (template-use-chunk 'name
                                                   (list 'first))
                               test-context))
                 '("name<first>"))
   (check-equal? (write-nekot (chunk-transform (template-use-chunk 'name
                                                   (list 'first
                                                         'second))
                               test-context))
                 '("name<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-use-chunk 'name
                                                   (list 'first
                                                         'second))
                               (construct-context 6)))
                 '("     second>"
                   "name<first,"))))

;function chunks

(define/provide-test-suite test-function-declare-chunk
  (test-case
   "Test function-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-declare-chunk 'name
                                                       'return-type
                                                       null)
                               test-context))
                 '("inline return-type name (void)"))
   (check-equal? (write-nekot (chunk-transform (function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first))
                               test-context))
                 '("inline return-type name (first)"))
   (check-equal? (write-nekot (chunk-transform (function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first
                                                             'second))
                               test-context))
                 '("inline return-type name (first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first
                                                             'second))
                               (construct-context 8)))
                 '("      second)"
                   "name (first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-static-function-declare-chunk
  (test-case
   "Test static-function-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (static-function-declare-chunk 'name
                                                       'return-type
                                                       null)
                               test-context))
                 '("static inline return-type name (void)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first))
                               test-context))
                 '("static inline return-type name (first)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first
                                                             'second))
                               test-context))
                 '("static inline return-type name (first, second)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare-chunk 'name
                                                       'return-type
                                                       (list 'first
                                                             'second))
                               (construct-context 20)))
                 '("                  second)"
                   "return-type name (first,"
                   "static inline"))))

(define/provide-test-suite test-void-function-declare-chunk
  (test-case
   "Test void-function-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-declare-chunk 'name
                                                            null)
                               test-context))
                 '("inline void name (void)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare-chunk 'name
                                                            (list 'first))
                               test-context))
                 '("inline void name (first)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare-chunk 'name
                                                            (list 'first
                                                                  'second))
                               test-context))
                 '("inline void name (first, second)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare-chunk 'name
                                                            (list 'first
                                                                  'second))
                               (construct-context 20)))
                 '("                  second)"
                   "inline void name (first,"))))

(define/provide-test-suite test-function-define-chunk
  (test-case
   "Test function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-define-chunk 'signature)
                               test-context))
                 '("signature {}"))
   (check-equal? (write-nekot (chunk-transform (function-define-chunk 'signature
                                                      'first)
                               test-context))
                 '("signature { first; }"))
   (check-equal? (write-nekot (chunk-transform (function-define-chunk 'signature
                                                      'first
                                                      'second)
                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (function-define-chunk 'signature
                                                      'first
                                                      'second)
                               (construct-context 20)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   ""
                   "signature {"))))

(define/provide-test-suite test-void-function-define-chunk
  (test-case
   "Test void-function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-define-chunk 'name
                                                           null
                                                           null)
                               test-context))
                 '("inline void name (void) {}"))
   (check-equal? (write-nekot (chunk-transform (void-function-define-chunk 'name
                                                      (list 'first)
                                                      (list 'first))
                               test-context))
                 '("inline void name (first) { first; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define-chunk 'name
                                                      (list 'first
                                                            'second)
                                                      (list 'first
                                                            'second))
                               test-context))
                 '("inline void name (first, second) { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define-chunk 'name
                                                      (list 'first
                                                            'second)
                                                      (list 'first
                                                            'second))
                               (construct-context 10)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   ""
                   "           second) {"
                   "void name (first,"
                   "inline"))))

(define/provide-test-suite test-returning-function-define-chunk
  (test-case
   "Test returning-function-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (returning-function-define-chunk (function-declare-chunk 'name
                                                                                        'return-type
                                                                                        null)
                                                                null
                                                                'expr)
                               test-context))
                 '("inline return-type name (void) { return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define-chunk (function-declare-chunk 'name
                                                                                        'return-type
                                                                                        (list 'first))
                                                                (list 'first)
                                                                'expr)

                               test-context))
                 '("inline return-type name (first) { first; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define-chunk (function-declare-chunk 'name
                                                                                        'return-type
                                                                                        (list 'first
                                                                                              'second))
                                                                (list 'first
                                                                      'second)
                                                                'expr)
                               test-context))
                 '("inline return-type name (first, second) { first; second; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define-chunk (function-declare-chunk 'name
                                                                                        'return-type
                                                                                        (list 'first
                                                                                              'second))
                                                                (list 'first
                                                                      'second)
                                                                'expr)
                               (construct-context 20)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   ""
                   "name (first, second) {"
                   "inline return-type"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define-chunk (function-declare-chunk 'name
                                                                                        'return-type
                                                                                        (list 'first
                                                                                              'second))
                                                                (list 'first
                                                                      'second)
                                                                'expr)
                               (construct-context 10)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   ""
                   "      second) {"
                   "name (first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-constructor-assignment-chunk
  (test-case
   "Test constructor-assignment-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-chunk 'first
                                                             'second)
                               test-context))
                 '("first(second)"))))

(define/provide-test-suite test-constructor-chunk
  (test-case
   "Test constructor-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor-chunk 'name
                                                  null
                                                  null)
                               test-context))
                 '("name() {}"))
   (check-equal? (write-nekot (chunk-transform (constructor-chunk 'name
                                                  (list 'first)
                                                  (list 'assign)
                                                  'asdf)
                               test-context))
                 '("{ asdf; }"
                   "  : assign"
                   "name(first)"))
   (check-equal? (write-nekot (chunk-transform (constructor-chunk 'name
                                                  (list 'first
                                                        'second)
                                                  (list 'assign1
                                                        'assign2)
                                                  'asdf
                                                  'jkl)
                               test-context))
                 '("{ asdf; jkl; }"
                   "  : assign1, assign2"
                   "name(first, second)"))))

;class/struct chunks

(define/provide-test-suite test-struct-declare-chunk
  (test-case
   "Test struct-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-declare-chunk 'name)
                               test-context))
                 '("struct name"))))

(define/provide-test-suite test-template-struct-declare-chunk
  (test-case
   "Test template-struct-declare-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare-chunk 'name
                                                              null
                                                              null)
                               test-context))
                 '(" struct name"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare-chunk 'name
                                                              (list 'first)
                                                              (list 'first))
                               test-context))
                 '(" struct name<first>"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare-chunk 'name
                                                              (list 'first
                                                                    'second)
                                                              (list 'first
                                                                    'second))
                               test-context))
                 '(" struct name<first, second>"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare-chunk 'name
                                                              (list 'first)
                                                              (list 'first
                                                                    'second))
                               test-context))
                 '(" struct name<first, second>"
                   "template<first>"))))

(define/provide-test-suite test-section-define-chunk
  (test-case
   "Test section-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (section-define-chunk 'name
                                                     'first
                                                     'second)
                               test-context))
                 '(" second"
                   " first;"
                   "name:"))))

(define/provide-test-suite test-struct-define-chunk
  (test-case
   "Test struct-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-define-chunk 'signature
                                                    'first
                                                    'second)
                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (struct-define-chunk 'signature
                                                    'first
                                                    'second)
                               (construct-context 8)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   ""
                   "signature {"))))

(define/provide-test-suite test-template-struct-define-chunk
  (test-case
   "Test template-struct-define-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-define-chunk 'name
                                                             (list 'first
                                                                   'second)
                                                             null
                                                             'first
                                                             'second)
                               test-context))
                 '(" struct name { first; second; }"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-define-chunk 'name
                                                             (list 'first
                                                                   'second)
                                                             null
                                                             'first
                                                             'second)
                               (construct-context 12)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   ""
                   " struct name {"
                   "         second>"
                   "template<first,"))))

;statement chunks

(define/provide-test-suite test-typedef-smt-chunk
  (test-case
   "Test typedef-smt-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (typedef-smt-chunk 'lhs
                                                  'rhs)
                               test-context))
                 '("lhs typedef rhs"))))

(define/provide-test-suite test-function-call-chunk
  (test-case
   "Test function-call-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-call-chunk 'name)
                               test-context))
                 '("name()"))
   (check-equal? (write-nekot (chunk-transform (function-call-chunk 'name
                                                    'first)
                               test-context))
                 '("name(first)"))
   (check-equal? (write-nekot (chunk-transform (function-call-chunk 'name
                                                    'first
                                                    'second)
                               test-context))
                 '("name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-call-chunk 'name
                                                    'first
                                                    'second)
                               (construct-context 4)))
                 '("     second)"
                   "name(first,"))))

(define/provide-test-suite test-member-function-call-chunk
  (test-case
   "Test member-function-call-chunk"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (member-function-call-chunk 'obj
                                                           'name)
                               test-context))
                 '("obj.name()"))
   (check-equal? (write-nekot (chunk-transform (member-function-call-chunk 'obj
                                                           'name
                                                           'first)
                               test-context))
                 '("obj.name(first)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call-chunk 'obj
                                                           'name
                                                           'first
                                                           'second)
                               test-context))
                 '("obj.name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call-chunk 'object
                                                           'name
                                                           'first
                                                           'second)
                               (construct-context 4)))
                 '("            second)"
                   "object.name(first,"))))
