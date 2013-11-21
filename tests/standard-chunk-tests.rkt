#lang racket

(require rackunit)
(require "../src/fulmar-core.rkt")
(require "../src/core-chunk.rkt")
(require "../src/standard-chunk.rkt")
(require "../src/writer.rkt")

;unit tests for standard-chunk.rkt

;character chunks

(define/provide-test-suite test-characters
  (test-case
   "Test space"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform space test-context))
                 '(" "))
   (check-equal? (write-nekot (chunk-transform (concat "1" space)
                                               test-context))
                 '("1 "))
   (check-equal? (write-nekot (chunk-transform (concat "123456" space)
                                               test-context))
                 '("" "123456")))
  (test-case
   "Test imm-space"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-space test-context))
                 '(" "))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-space)
                                               test-context))
                 '("1 "))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-space)
                                               test-context))
                 '("123456 ")))
  (test-case
   "Test blank-lines"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat "1" (blank-lines 1))
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat "1" (blank-lines 3))
                                               test-context))
                 '("" "" "" "" "1")))
  (test-case
   "Test blank-line"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (concat "1" blank-line)
                                               test-context))
                 '("" "" "1"))
   (check-equal? (write-nekot (chunk-transform (concat "123" blank-line)
                                               test-context))
                 '("" "" "123")))
  (test-case
   "Test open-paren"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-paren test-context))
                 '("("))
   (check-equal? (write-nekot (chunk-transform (concat "1" open-paren)
                                               test-context))
                 '("1("))
   (check-equal? (write-nekot (chunk-transform (concat "123456" open-paren)
                                               test-context))
                 '("(" "123456")))
  (test-case
   "Test imm-open-paren"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-paren test-context))
                 '("("))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-open-paren)
                                               test-context))
                 '("1("))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-open-paren)
                                               test-context))
                 '("123456(")))
  (test-case
   "Test close-paren"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-paren test-context))
                 '(")"))
   (check-equal? (write-nekot (chunk-transform (concat "1" close-paren)
                                               test-context))
                 '("1)"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" close-paren)
                                               test-context))
                 '(")" "123456")))
  (test-case
   "Test imm-close-paren"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-paren test-context))
                 '(")"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-close-paren)
                                               test-context))
                 '("1)"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-close-paren)
                                               test-context))
                 '("123456)")))
  (test-case
   "Test open-crbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-crbr test-context))
                 '("{"))
   (check-equal? (write-nekot (chunk-transform (concat "1" open-crbr)
                                               test-context))
                 '("1{"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" open-crbr)
                                               test-context))
                 '("{" "123456")))
  (test-case
   "Test imm-open-crbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-crbr test-context))
                 '("{"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-open-crbr)
                                               test-context))
                 '("1{"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-open-crbr)
                                               test-context))
                 '("123456{")))
  (test-case
   "Test close-crbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-crbr test-context))
                 '("}"))
   (check-equal? (write-nekot (chunk-transform (concat "1" close-crbr)
                                               test-context))
                 '("1}"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" close-crbr)
                                               test-context))
                 '("}" "123456")))
  (test-case
   "Test imm-close-crbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-crbr test-context))
                 '("}"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-close-crbr)
                                               test-context))
                 '("1}"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-close-crbr)
                                               test-context))
                 '("123456}")))
  (test-case
   "Test open-anbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform open-anbr test-context))
                 '("<"))
   (check-equal? (write-nekot (chunk-transform (concat "1" open-anbr)
                                               test-context))
                 '("1<"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" open-anbr)
                                               test-context))
                 '("<" "123456")))
  (test-case
   "Test imm-open-anbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-open-anbr test-context))
                 '("<"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-open-anbr)
                                               test-context))
                 '("1<"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-open-anbr)
                                               test-context))
                 '("123456<")))
  (test-case
   "Test close-anbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform close-anbr test-context))
                 '(">"))
   (check-equal? (write-nekot (chunk-transform (concat "1" close-anbr)
                                               test-context))
                 '("1>"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" close-anbr)
                                               test-context))
                 '(">" "123456")))
  (test-case
   "Test imm-close-anbr"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-close-anbr test-context))
                 '(">"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-close-anbr)
                                               test-context))
                 '("1>"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-close-anbr)
                                               test-context))
                 '("123456>")))
  (test-case
   "Test comma"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform comma test-context))
                 '(","))
   (check-equal? (write-nekot (chunk-transform (concat "1" comma)
                                               test-context))
                 '("1,"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" comma)
                                               test-context))
                 '("," "123456")))
  (test-case
   "Test imm-comma"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-comma test-context))
                 '(","))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-comma)
                                               test-context))
                 '("1,"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-comma)
                                               test-context))
                 '("123456,")))
  (test-case
   "Test period"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform period test-context))
                 '("."))
   (check-equal? (write-nekot (chunk-transform (concat "1" period)
                                               test-context))
                 '("1."))
   (check-equal? (write-nekot (chunk-transform (concat "123456" period)
                                               test-context))
                 '("." "123456")))
  (test-case
   "Test imm-period"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-period test-context))
                 '("."))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-period)
                                               test-context))
                 '("1."))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-period)
                                               test-context))
                 '("123456.")))
  (test-case
   "Test colon"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform colon test-context))
                 '(":"))
   (check-equal? (write-nekot (chunk-transform (concat "1" colon)
                                               test-context))
                 '("1:"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" colon)
                                               test-context))
                 '(":" "123456")))
  (test-case
   "Test imm-colon"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-colon test-context))
                 '(":"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-colon)
                                               test-context))
                 '("1:"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-colon)
                                               test-context))
                 '("123456:")))
  (test-case
   "Test semi-colon"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform semi-colon test-context))
                 '(";"))
   (check-equal? (write-nekot (chunk-transform (concat "1" semi-colon)
                                               test-context))
                 '("1;"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" semi-colon)
                                               test-context))
                 '(";" "123456")))
  (test-case
   "Test imm-semi-colon"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-semi-colon test-context))
                 '(";"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-semi-colon)
                                               test-context))
                 '("1;"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-semi-colon)
                                               test-context))
                 '("123456;"))))

;keyword chunks

(define/provide-test-suite test-keywords
  (test-case
   "Test define"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform define-chunk test-context))
                 '("define"))
   (check-equal? (write-nekot (chunk-transform (concat "1" define-chunk)
                                               test-context))
                 '("1define"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" define-chunk)
                                               test-context))
                 '("define" "123456")))
  (test-case
   "Test imm-define"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-define test-context))
                 '("define"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-define)
                                               test-context))
                 '("1define"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-define)
                                               test-context))
                 '("123456define")))
  (test-case
   "Test include"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform include test-context))
                 '("include"))
   (check-equal? (write-nekot (chunk-transform (concat "1" include)
                                               test-context))
                 '("1include"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" include)
                                               test-context))
                 '("include" "123456")))
  (test-case
   "Test imm-include"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-include test-context))
                 '("include"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-include)
                                               test-context))
                 '("1include"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-include)
                                               test-context))
                 '("123456include")))
  (test-case
   "Test ifdef"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform ifdef test-context))
                 '("ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" ifdef)
                                               test-context))
                 '("1ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" ifdef)
                                               test-context))
                 '("ifdef" "123456")))
  (test-case
   "Test imm-ifdef"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-ifdef test-context))
                 '("ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-ifdef)
                                               test-context))
                 '("1ifdef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-ifdef)
                                               test-context))
                 '("123456ifdef")))
  (test-case
   "Test ifndef"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform ifndef test-context))
                 '("ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" ifndef)
                                               test-context))
                 '("1ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" ifndef)
                                               test-context))
                 '("ifndef" "123456")))
  (test-case
   "Test imm-ifndef"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-ifndef test-context))
                 '("ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-ifndef)
                                               test-context))
                 '("1ifndef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-ifndef)
                                               test-context))
                 '("123456ifndef")))
  (test-case
   "Test else"
   (define test-context (construct-context 5))
   (check-equal? (write-nekot (chunk-transform else test-context))
                 '("else"))
   (check-equal? (write-nekot (chunk-transform (concat "1" else)
                                               test-context))
                 '("1else"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" else)
                                               test-context))
                 '("else" "123456")))
  (test-case
   "Test imm-else"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-else test-context))
                 '("else"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-else)
                                               test-context))
                 '("1else"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-else)
                                               test-context))
                 '("123456else")))
  (test-case
   "Test endif"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform endif test-context))
                 '("endif"))
   (check-equal? (write-nekot (chunk-transform (concat "1" endif)
                                               test-context))
                 '("1endif"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" endif)
                                               test-context))
                 '("endif" "123456")))
  (test-case
   "Test imm-endif"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-endif test-context))
                 '("endif"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-endif)
                                               test-context))
                 '("1endif"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-endif)
                                               test-context))
                 '("123456endif")))
  (test-case
   "Test template"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot (chunk-transform template test-context))
                 '("template"))
   (check-equal? (write-nekot (chunk-transform (concat "1" template)
                                               test-context))
                 '("1template"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" template)
                                               test-context))
                 '("template" "123456")))
  (test-case
   "Test imm-template"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-template test-context))
                 '("template"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-template)
                                               test-context))
                 '("1template"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-template)
                                               test-context))
                 '("123456template")))
  (test-case
   "Test typename"
   (define test-context (construct-context 9))
   (check-equal? (write-nekot (chunk-transform typename test-context))
                 '("typename"))
   (check-equal? (write-nekot (chunk-transform (concat "1" typename)
                                               test-context))
                 '("1typename"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" typename)
                                               test-context))
                 '("typename" "123456")))
  (test-case
   "Test imm-typename"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-typename test-context))
                 '("typename"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-typename)
                                               test-context))
                 '("1typename"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-typename)
                                               test-context))
                 '("123456typename")))
  (test-case
   "Test typedef"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform typedef test-context))
                 '("typedef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" typedef)
                                               test-context))
                 '("1typedef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" typedef)
                                               test-context))
                 '("typedef" "123456")))
  (test-case
   "Test imm-typedef"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-typedef test-context))
                 '("typedef"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-typedef)
                                               test-context))
                 '("1typedef"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-typedef)
                                               test-context))
                 '("123456typedef")))
  (test-case
   "Test void"
   (define test-context (construct-context 5))
   (check-equal? (write-nekot (chunk-transform void test-context))
                 '("void"))
   (check-equal? (write-nekot (chunk-transform (concat "1" void)
                                               test-context))
                 '("1void"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" void)
                                               test-context))
                 '("void" "123456")))
  (test-case
   "Test imm-void"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-void test-context))
                 '("void"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-void)
                                               test-context))
                 '("1void"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-void)
                                               test-context))
                 '("123456void")))
  (test-case
   "Test inline"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform inline test-context))
                 '("inline"))
   (check-equal? (write-nekot (chunk-transform (concat "1" inline)
                                               test-context))
                 '("1inline"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" inline)
                                               test-context))
                 '("inline" "123456")))
  (test-case
   "Test imm-inline"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-inline test-context))
                 '("inline"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-inline)
                                               test-context))
                 '("1inline"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-inline)
                                               test-context))
                 '("123456inline")))
  (test-case
   "Test static"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform static test-context))
                 '("static"))
   (check-equal? (write-nekot (chunk-transform (concat "1" static)
                                               test-context))
                 '("1static"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" static)
                                               test-context))
                 '("static" "123456")))
  (test-case
   "Test imm-static"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-static test-context))
                 '("static"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-static)
                                               test-context))
                 '("1static"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-static)
                                               test-context))
                 '("123456static")))
  (test-case
   "Test return"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform return
                                               test-context))
                 '("return"))
   (check-equal? (write-nekot (chunk-transform (concat "1" return)
                                               test-context))
                 '("1return"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" return)
                                               test-context))
                 '("return" "123456")))
  (test-case
   "Test imm-return"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-return test-context))
                 '("return"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-return)
                                               test-context))
                 '("1return"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-return)
                                               test-context))
                 '("123456return")))
  (test-case
   "Test const"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform const test-context))
                 '("const"))
   (check-equal? (write-nekot (chunk-transform (concat "1" const)
                                               test-context))
                 '("1const"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" const)
                                               test-context))
                 '("const" "123456")))
  (test-case
   "Test imm-const"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-const test-context))
                 '("const"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-const)
                                               test-context))
                 '("1const"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-const)
                                               test-context))
                 '("123456const")))
  (test-case
   "Test struct"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform struct test-context))
                 '("struct"))
   (check-equal? (write-nekot (chunk-transform (concat "1" struct)
                                               test-context))
                 '("1struct"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" struct)
                                               test-context))
                 '("struct" "123456")))
  (test-case
   "Test imm-struct"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-struct test-context))
                 '("struct"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-struct)
                                               test-context))
                 '("1struct"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-struct)
                                               test-context))
                 '("123456struct")))
  (test-case
   "Test class"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform class test-context))
                 '("class"))
   (check-equal? (write-nekot (chunk-transform (concat "1" class)
                                               test-context))
                 '("1class"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" class)
                                               test-context))
                 '("class" "123456")))
  (test-case
   "Test imm-class"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-class test-context))
                 '("class"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-class)
                                               test-context))
                 '("1class"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-class)
                                               test-context))
                 '("123456class")))
  (test-case
   "Test public"
   (define test-context (construct-context 7))
   (check-equal? (write-nekot (chunk-transform public test-context))
                 '("public"))
   (check-equal? (write-nekot (chunk-transform (concat "1" public)
                                               test-context))
                 '("1public"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" public)
                                               test-context))
                 '("public" "123456")))
  (test-case
   "Test imm-public"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-public test-context))
                 '("public"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-public)
                                               test-context))
                 '("1public"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-public)
                                               test-context))
                 '("123456public")))
  (test-case
   "Test protected"
   (define test-context (construct-context 10))
   (check-equal? (write-nekot (chunk-transform protected test-context))
                 '("protected"))
   (check-equal? (write-nekot (chunk-transform (concat "1" protected)
                                               test-context))
                 '("1protected"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" protected)
                                               test-context))
                 '("protected" "123456")))
  (test-case
   "Test imm-protected"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-protected test-context))
                 '("protected"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-protected)
                                               test-context))
                 '("1protected"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-protected)
                                               test-context))
                 '("123456protected")))
  (test-case
   "Test private"
   (define test-context (construct-context 8))
   (check-equal? (write-nekot (chunk-transform private test-context))
                 '("private"))
   (check-equal? (write-nekot (chunk-transform (concat "1" private)
                                               test-context))
                 '("1private"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" private)
                                               test-context))
                 '("private" "123456")))
  (test-case
   "Test imm-private"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-private test-context))
                 '("private"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-private)
                                               test-context))
                 '("1private"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-private)
                                               test-context))
                 '("123456private")))
  (test-case
   "Test namespace"
   (define test-context (construct-context 10))
   (check-equal? (write-nekot (chunk-transform namespace test-context))
                 '("namespace"))
   (check-equal? (write-nekot (chunk-transform (concat "1" namespace)
                                               test-context))
                 '("1namespace"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" namespace)
                                               test-context))
                 '("namespace" "123456")))
  (test-case
   "Test imm-namespace"
   (define test-context (construct-context 6))
   (check-equal? (write-nekot (chunk-transform imm-namespace test-context))
                 '("namespace"))
   (check-equal? (write-nekot (chunk-transform (concat "1" imm-namespace)
                                               test-context))
                 '("1namespace"))
   (check-equal? (write-nekot (chunk-transform (concat "123456" imm-namespace)
                                               test-context))
                 '("123456namespace"))))

;list chunks

(define/provide-test-suite test-attach-list-separator
  (test-case
   "Test attach-list-separator"
   (define test-context (construct-context 80))
   (check-equal? (attach-list-separator comma)
                 null)
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator comma 'asdf 'jkl))
                                               test-context))
                 '("asdf,jkl"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator comma 'asdf))
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator comma 'asdf 'jkl "12345"))
                                               test-context))
                 '("asdf,jkl,12345"))
   (check-equal? (write-nekot (chunk-transform (concat (attach-list-separator comma 'asdf "123" "12345" "1"))
                                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-between
  (test-case
   "Test between"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between space)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between space 'asdf 'jkl)
                                               test-context))
                 '("asdf jkl"))
   (check-equal? (write-nekot (chunk-transform (between space 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between space 'asdf 'jkl "12345")
                                               test-context))
                 '("asdf jkl 12345"))
   (check-equal? (write-nekot (chunk-transform (between new-line 'asdf "123" "12345" "1")
                                               test-context))
                 '("1" "12345" "123" "asdf"))))

(define/provide-test-suite test-between/attach
  (test-case
   "Test between/attach"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space 'asdf 'jkl)
                                               test-context))
                 '("asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space 'asdf)
                                               test-context))
                 '("asdf"))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space 'asdf 'jkl "12345")
                                               test-context))
                 '("asdf, jkl, 12345"))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space 'asdf "123" "12345" "1")
                                               test-context))
                 '("asdf, 123, 12345, 1"))
   (check-equal? (write-nekot (chunk-transform (between/attach comma new-line 'asdf "123" "12345" "1")
                                               test-context))
                 '("1" "12345," "123," "asdf,"))
   (check-equal? (write-nekot (chunk-transform (between/attach comma space 'asdf "123" "12345" "1")
                                               (construct-context 4)))
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-arg-list
  (test-case
   "Test arg-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren comma 'asdf)
                                               test-context))
                 '("(asdf)"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren comma (concat 'asdf 'jkl))
                                               test-context))
                 '(" jkl)"
                   "(asdf"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren comma 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (arg-list sur-paren comma 'asdf empty 'jkl)
                                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-paren-list
  (test-case
   "Test paren-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (paren-list 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-nekot (chunk-transform (paren-list 'asdf empty 'jkl)
                                               test-context-2))
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-template-list
  (test-case
   "Test template-list"
   (define test-context (construct-context 6))
   (define test-context-2 (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-list 'asdf empty 'jkl)
                                               test-context))
                 '(" jkl>" " ," "<asdf,"))
   (check-equal? (write-nekot (chunk-transform (template-list 'asdf empty 'jkl)
                                               test-context-2))
                 '("<asdf, , jkl>"))))

(define/provide-test-suite test-smt-list
  (test-case
   "Test smt-list"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (smt-list blank-line 'asdf space 'jkl)
                                               test-context))
                 '("jkl;" "" " ;" "" "asdf;"))
   (check-equal? (write-nekot (chunk-transform (smt-list new-line
                                                         (spaces 4)
                                                         'asdf
                                                         empty)
                                               test-context))
                 '(";" "asdf;" "    ;"))))


(define/provide-test-suite test-constructor-assignment-list
  (test-case
   "Test constructor-assignment-list"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list)
                                               test-context))
                 '(""))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf)
                                               test-context))
                 '("  : asdf"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf 'jkl)
                                               test-context))
                 '("  : asdf, jkl"))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment-list 'asdf 'jkl)
                                               test-context-2))
                 '("    jkl"
                   "  : asdf,"))))

(define/provide-test-suite test-body
  (test-case
   "Test body"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 6))
   (check-equal? (write-nekot (chunk-transform (body 'asdf space 'jkl)
                                               test-context))
                 '("{ asdf;  ; jkl; }"))
   (check-equal? (write-nekot (chunk-transform (body "123456")
                                               test-context-2))
                 '("}"
                   "   123456;"
                   "{"))
   (check-equal? (write-nekot (chunk-transform (body 'asdf space 'jkl)
                                               test-context-2))
                 '("}"
                   "   jkl;"
                   ""
                   "    ;"
                   ""
                   "   asdf;"
                   "{"))))

;preprocessor chunks

(define/provide-test-suite test-pp-define
  (test-case
   "Test pp-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-define 'name) test-context)) '("#define name"))
   (check-equal? (write-nekot (chunk-transform (pp-define (concat 'name "2")) test-context)) '("#define name2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-define 'name)) test-context)) '("#  define name"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-define 'name)) test-context)) '("/*#define name"))))

(define/provide-test-suite test-pp-include
  (test-case
   "Test pp-include"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-include 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-include (concat 'name "2")) test-context)) '("#include <name2>"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-include 'name)) test-context)) '("#  include <name>"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-include 'name)) test-context)) '("/*#include <name>"))))

(define/provide-test-suite test-pp-includes
  (test-case
   "Test pp-includes"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-includes 'name) test-context)) '("#include <name>"))
   (check-equal? (write-nekot (chunk-transform (pp-includes 'name 'name2)
                                               test-context))
                 '("#include <name2>"
                   "#include <name>"))))

(define/provide-test-suite test-pp-ifdef
  (test-case
   "Test pp-ifdef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef 'condition) test-context)) '("#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifdef (concat 'condition "2")) test-context)) '("#ifdef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-ifdef 'condition)) test-context)) '("#  ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-ifdef 'condition)) test-context)) '("/*#ifdef condition"))))

(define/provide-test-suite test-pp-ifndef
  (test-case
   "Test pp-ifndef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef 'condition) test-context)) '("#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-ifndef (concat 'condition "2")) test-context)) '("#ifndef condition2"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-ifndef 'condition)) test-context)) '("#  ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-ifndef 'condition)) test-context)) '("/*#ifndef condition"))))

(define/provide-test-suite test-pp-else
  (test-case
   "Test pp-else"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform pp-else test-context)) '("#else"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) pp-else) test-context)) '("#  else"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " pp-else) test-context)) '("/*#else"))))

(define/provide-test-suite test-pp-endif
  (test-case
   "Test pp-endif"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-endif 'condition) test-context)) '("/* condition */" "#endif"))
   (check-equal? (write-nekot (chunk-transform (pp-endif (concat 'condition "2")) test-context)) '("/* condition2 */" "#endif"))
   (check-equal? (write-nekot (chunk-transform (concat (spaces 3) (pp-endif 'condition)) test-context)) '("/* condition */" "#  endif"))
   (check-equal? (write-nekot (chunk-transform (concat "/* " (pp-endif 'condition)) test-context)) '("/* condition */" "/*#endif"))))

(define/provide-test-suite test-pp-conditional
  (test-case
   "Test pp-conditional"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional ifdef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional ifndef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-conditional-ifdef
  (test-case
   "Test pp-conditional-ifdef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifdef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifdef condition"))))

(define/provide-test-suite test-pp-conditional-ifndef
  (test-case
   "Test pp-conditional-ifndef"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef 'condition 'then)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifndef condition"))
   (check-equal? (write-nekot (chunk-transform (pp-conditional-ifndef 'condition 'then 'else2)
                                               test-context))
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-header-file
  (test-case
   "Test pp-header-file"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (pp-header-file 'header_file empty 'asdf 'jkl)
                                               test-context))
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   ""
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-nekot (chunk-transform (pp-header-file 'header_file
                                                               (concat (pp-include 'iostream)
                                                                       new-line
                                                                       (pp-include 'algorithm))
                                                               'asdf
                                                               'jkl)
                                               test-context))
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   "#  include <algorithm>"
                   "#  include <iostream>"
                   ""
                   "#  define header_file"
                   "#ifndef header_file"))))

(define/provide-test-suite test-macro-define
  (test-case
   "Test macro-defintion"
   (define test-context (construct-context 80))
   (define test-context-2 (construct-context 20))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name null 'asdf)
                                               test-context))
                 '("#define name asdf"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list 'first)
                                                             (concat 'asdf space 'first))
                                               test-context))
                 '("#define name(first) asdf first"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list 'first)
                                                             (concat 'asdf space 'first))
                                               test-context-2))
                 '("   asdf first" "#define name(first) \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list 'first 'second)
                                                             (concat 'asdf space 'first space 'second))
                                               test-context))
                 '("#define name(first, second) asdf first second"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list 'first 'second)
                                                             (concat 'asdf new-line 'first new-line 'second))
                                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list (concat (comment-env-chunk 't)
                                                                           new-line
                                                                           'first)
                                                                   'second)
                                                             (concat 'asdf new-line 'first new-line 'second))
                                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             first, \\"
                   "#define name(/* t */ \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list 'first
                                                                   (concat (comment-env-chunk 't)
                                                                           new-line
                                                                           'second))
                                                             (concat 'asdf new-line 'first new-line 'second))
                                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             /* t */ \\"
                   "#define name(first, \\"))
   (check-equal? (write-nekot (chunk-transform (macro-define 'name
                                                             (list (concat (comment-env-chunk 't)
                                                                           new-line
                                                                           'first)
                                                                   (concat (comment-env-chunk 'T)
                                                                           new-line
                                                                           'second))
                                                             (concat 'asdf new-line 'first new-line 'second))
                                               test-context-2))
                 '("   second"
                   "   first           \\"
                   "   asdf            \\"
                   "             second) \\"
                   "             /* T */ \\"
                   "             first, \\"
                   "#define name(/* t */ \\"))))

;general chunks

(define/provide-test-suite test-namespace-define
  (test-case
   "Test namespace-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf)
                                               test-context))
                 '("namespace name { asdf; }"))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf)
                                               (construct-context 12)))
                 '("} /* name */"
                   "   asdf;"
                   "namespace name {"))
   (check-equal? (write-nekot (chunk-transform (namespace-define 'name 'asdf 'jkl)
                                               (construct-context 12)))
                 '("} /* name */"
                   "   jkl;"
                   ""
                   "   asdf;"
                   "namespace name {"))))

(define/provide-test-suite test-described-smts
  (test-case
   "Test described-smts"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf)
                                               test-context))
                 '("asdf"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf 'jkl)
                                               test-context))
                 '("jkl"
                   "asdf;"
                   "/* name */"))
   (check-equal? (write-nekot (chunk-transform (described-smts 'name 'asdf 'jkl "1234")
                                               test-context))
                 '("1234"
                   "jkl;"
                   "asdf;"
                   "/* name */"))))

(define/provide-test-suite test-constize
  (test-case
   "Test constize"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constize 'asdf)
                                               test-context))
                 '("asdf const"))
   (check-equal? (write-nekot (chunk-transform (constize 'asdf)
                                               (construct-context 4)))
                 '("asdf const"))))

;template chunks

(define/provide-test-suite test-template-define
  (test-case
   "Test template-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-define null 'asdf)
                                               test-context))
                 '(" asdf"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list 'first) 
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list 'first 'second)
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-define (list (concat (comment-env-chunk 'test)
                                                                              new-line
                                                                              'first)
                                                                      'second)
                                                                'asdf)
                                               test-context))
                 '(" asdf"
                   "         second>"
                   "         first,"
                   "template</* test */"))))

(define/provide-test-suite test-template-use
  (test-case
   "Test template-defintion"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-use 'name null)
                                               test-context))
                 '("name"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first))
                                               test-context))
                 '("name<first>"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first 'second))
                                               test-context))
                 '("name<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-use 'name (list 'first 'second))
                                               (construct-context 6)))
                 '("     second>"
                   "name<first,"))))

;function chunks

(define/provide-test-suite test-function-declare
  (test-case
   "Test function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type null)
                                               test-context))
                 '("inline return-type name(void)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first))
                                               test-context))
                 '("inline return-type name(first)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first 'second))
                                               test-context))
                 '("inline return-type name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-declare 'name 'return-type (list 'first 'second))
                                               (construct-context 8)))
                 '("     second)"
                   "name(first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-static-function-declare
  (test-case
   "Test static-function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type null)
                                               test-context))
                 '("static inline return-type name(void)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first))
                                               test-context))
                 '("static inline return-type name(first)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first 'second))
                                               test-context))
                 '("static inline return-type name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (static-function-declare 'name 'return-type (list 'first 'second))
                                               (construct-context 20)))
                 '("                 second)"
                   "return-type name(first,"
                   "static inline"))))

(define/provide-test-suite test-void-function-declare
  (test-case
   "Test void-function-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name null)
                                               test-context))
                 '("inline void name(void)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first))
                                               test-context))
                 '("inline void name(first)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first 'second))
                                               test-context))
                 '("inline void name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (void-function-declare 'name (list 'first 'second))
                                               (construct-context 20)))
                 '("                 second)"
                   "inline void name(first,"))))

(define/provide-test-suite test-function-define
  (test-case
   "Test function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature)
                                               test-context))
                 '("signature {}"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first)
                                               test-context))
                 '("signature { first; }"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first 'second)
                                               test-context))
                 '("signature { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (function-define 'signature 'first 'second)
                                               (construct-context 20)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "signature {"))))

(define/provide-test-suite test-void-function-define
  (test-case
   "Test void-function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name null null)
                                               test-context))
                 '("inline void name(void) {}"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first)
                                                                     (list 'first))
                                               test-context))
                 '("inline void name(first) { first; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first 'second)
                                                                     (list 'first 'second))
                                               test-context))
                 '("inline void name(first, second) { first; second; }"))
   (check-equal? (write-nekot (chunk-transform (void-function-define 'name
                                                                     (list 'first 'second)
                                                                     (list 'first 'second))
                                               (construct-context 10)))
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "          second) {"
                   "void name(first,"
                   "inline"))))

(define/provide-test-suite test-returning-function-define
  (test-case
   "Test returning-function-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type null)
                                                                          null
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(void) { return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first))
                                                                          (list 'first)
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(first) { first; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               test-context))
                 '("inline return-type name(first, second) { first; second; return expr; }"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               (construct-context 20)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "name(first, second) {"
                   "inline return-type"))
   (check-equal? (write-nekot (chunk-transform (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                          (list 'first 'second)
                                                                          'expr)
                                               (construct-context 10)))
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "     second) {"
                   "name(first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-constructor-assignment
  (test-case
   "Test constructor-assignment"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor-assignment 'first 'second)
                                               test-context))
                 '("first(second)"))))

(define/provide-test-suite test-constructor
  (test-case
   "Test constructor"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (constructor 'name null null)
                                               test-context))
                 '("name() {}"))
   (check-equal? (write-nekot (chunk-transform (constructor 'name
                                                            (list 'first)
                                                            (list 'assign)
                                                            'asdf)
                                               test-context))
                 '("{ asdf; }"
                   "  : assign"
                   "name(first)"))
   (check-equal? (write-nekot (chunk-transform (constructor 'name
                                                            (list 'first 'second)
                                                            (list 'assign1 'assign2)
                                                            'asdf
                                                            'jkl)
                                               test-context))
                 '("{ asdf; jkl; }"
                   "  : assign1, assign2"
                   "name(first, second)"))))

;class/struct chunks

(define/provide-test-suite test-struct-declare
  (test-case
   "Test struct-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-declare 'name)
                                               test-context))
                 '("struct name"))))

(define/provide-test-suite test-template-struct-declare
  (test-case
   "Test template-struct-declare"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name null null)
                                               test-context))
                 '(" struct name"
                   "template<>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first)
                                                                        (list 'first))
                                               test-context))
                 '(" struct name<first>"
                   "template<first>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first 'second)
                                                                        (list 'first 'second))
                                               test-context))
                 '(" struct name<first, second>"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-declare 'name
                                                                        (list 'first)
                                                                        (list 'first 'second))
                                               test-context))
                 '(" struct name<first, second>"
                   "template<first>"))))

(define/provide-test-suite test-section-define
  (test-case
   "Test section-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (section-define 'name 'first 'second)
                                               test-context))
                 '(" second"
                   ""
                   " first"
                   "name:"))))

(define/provide-test-suite test-struct-define
  (test-case
   "Test struct-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (struct-define 'signature 'first 'second)
                                               test-context))
                 '("signature { first second }"))
   (check-equal? (write-nekot (chunk-transform (struct-define 'signature 'first 'second)
                                               (construct-context 8)))
                 '("}"
                   "   second"
                   ""
                   "   first"
                   "signature {"))))

(define/provide-test-suite test-template-struct-define
  (test-case
   "Test template-struct-define"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (template-struct-define 'name
                                                                       (list 'first 'second)
                                                                       null
                                                                       'first
                                                                       'second)
                                               test-context))
                 '(" struct name { first second }"
                   "template<first, second>"))
   (check-equal? (write-nekot (chunk-transform (template-struct-define 'name
                                                                       (list 'first 'second)
                                                                       null
                                                                       'first
                                                                       'second)
                                               (construct-context 12)))
                 '("}"
                   "   second"
                   ""
                   "   first"
                   " struct name {"
                   "         second>"
                   "template<first,"))))

(define/provide-test-suite test-scope-resolution-operator
  (test-case
   "Test scope-resolution-operator"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (scope-resolution-operator 'first 'second)
                                               test-context))
                 '("first::second"))))

;statement chunks

(define/provide-test-suite test-typedef-smt
  (test-case
   "Test typedef-smt"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (typedef-smt 'lhs 'rhs)
                                               test-context))
                 '("lhs typedef rhs"))))

(define/provide-test-suite test-function-call
  (test-case
   "Test function-call"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (function-call 'name)
                                               test-context))
                 '("name()"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first)
                                               test-context))
                 '("name(first)"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first 'second)
                                               test-context))
                 '("name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (function-call 'name 'first 'second)
                                               (construct-context 4)))
                 '("     second)"
                   "name(first,"))))

(define/provide-test-suite test-member-function-call
  (test-case
   "Test member-function-call"
   (define test-context (construct-context 80))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name)
                                               test-context))
                 '("obj.name()"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name 'first)
                                               test-context))
                 '("obj.name(first)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'obj 'name 'first 'second)
                                               test-context))
                 '("obj.name(first, second)"))
   (check-equal? (write-nekot (chunk-transform (member-function-call 'object 'name 'first 'second)
                                               (construct-context 4)))
                 '("            second)"
                   "object.name(first,"))))
