#lang racket

(require rackunit)
(require "../private/fulmar-core.rkt")
(require "../private/core-chunk.rkt")
(require "../standard-chunk.rkt")

;unit tests for standard-chunk.rkt

(define (write-chunk-with-length chunk length)
  (parameterize ([line-length length])
    (write-chunk chunk)))

;basic chunks
(define/provide-test-suite test-basic-chunks
  (test-case
   "Test blank-lines"
   (define test-context 6)
   (check-equal? (write-chunk-with-length (concat "1" (blank-lines 1))
                                          test-context)
                 '("" "" "1"))
   (check-equal? (write-chunk-with-length (concat "1" (blank-lines 3))
                                          test-context)
                 '("" "" "" "" "1")))
  (test-case
   "Test blank-line"
   (define test-context 6)
   (check-equal? (write-chunk-with-length (concat "1" blank-line)
                                          test-context)
                 '("" "" "1"))
   (check-equal? (write-chunk-with-length (concat "123" blank-line)
                                          test-context)
                 '("" "" "123"))))

;list chunks

(define/provide-test-suite test-attach-list-separator
  (test-case
   "Test attach-list-separator"
   (define test-context 80)
   (check-equal? (attach-list-separator ",")
                 null)
   (check-equal? (write-chunk-with-length (concat (attach-list-separator "," 'asdf 'jkl))
                                          test-context)
                 '("asdf,jkl"))
   (check-equal? (write-chunk-with-length (concat (attach-list-separator "," 'asdf))
                                          test-context)
                 '("asdf"))
   (check-equal? (write-chunk-with-length (concat (attach-list-separator "," 'asdf 'jkl "12345"))
                                          test-context)
                 '("asdf,jkl,12345"))
   (check-equal? (write-chunk-with-length (concat (attach-list-separator "," 'asdf "123" "12345" "1"))
                                          4)
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-between
  (test-case
   "Test between"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (between 1)
                                          test-context)
                 '(""))
   (check-equal? (write-chunk-with-length (between space 'asdf 'jkl)
                                          test-context)
                 '("asdf jkl"))
   (check-equal? (write-chunk-with-length (between 1 'asdf)
                                          test-context)
                 '("asdf"))
   (check-equal? (write-chunk-with-length (between space 'asdf 'jkl "12345")
                                          test-context)
                 '("asdf jkl 12345"))
   (check-equal? (write-chunk-with-length (between new-line 'asdf "123" "12345" "1")
                                          test-context)
                 '("1" "12345" "123" "asdf"))))

(define/provide-test-suite test-between/attach
  (test-case
   "Test between/attach"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (between/attach "," 1)
                                          test-context)
                 '(""))
   (check-equal? (write-chunk-with-length (between/attach "," space 'asdf 'jkl)
                                          test-context)
                 '("asdf, jkl"))
   (check-equal? (write-chunk-with-length (between/attach "," space 'asdf)
                                          test-context)
                 '("asdf"))
   (check-equal? (write-chunk-with-length (between/attach "," space 'asdf 'jkl "12345")
                                          test-context)
                 '("asdf, jkl, 12345"))
   (check-equal? (write-chunk-with-length (between/attach "," space 'asdf "123" "12345" "1")
                                          test-context)
                 '("asdf, 123, 12345, 1"))
   (check-equal? (write-chunk-with-length (between/attach "," new-line 'asdf "123" "12345" "1")
                                          test-context)
                 '("1" "12345," "123," "asdf,"))
   (check-equal? (write-chunk-with-length (between/attach "," space 'asdf "123" "12345" "1")
                                          4)
                 '("1" "12345," "123," "asdf,"))))

(define/provide-test-suite test-arg-list
  (test-case
   "Test arg-list"
   (define test-context 6)
   (define test-context-2 80)
   (check-equal? (write-chunk-with-length (arg-list sur-paren "," 'asdf)
                                          test-context)
                 '("(asdf)"))
   (check-equal? (write-chunk-with-length (arg-list sur-paren "," (concat 'asdf 'jkl))
                                          test-context)
                 '(" jkl)"
                   "(asdf"))
   (check-equal? (write-chunk-with-length (arg-list sur-paren "," 'asdf empty 'jkl)
                                          test-context)
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-chunk-with-length (arg-list sur-paren "," 'asdf empty 'jkl)
                                          test-context-2)
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-paren-list
  (test-case
   "Test paren-list"
   (define test-context 6)
   (define test-context-2 80)
   (check-equal? (write-chunk-with-length (paren-list 'asdf empty 'jkl)
                                          test-context)
                 '(" jkl)" " ," "(asdf,"))
   (check-equal? (write-chunk-with-length (paren-list 'asdf empty 'jkl)
                                          test-context-2)
                 '("(asdf, , jkl)"))))

(define/provide-test-suite test-template-list
  (test-case
   "Test template-list"
   (define test-context 6)
   (define test-context-2 80)
   (check-equal? (write-chunk-with-length (template-list 'asdf empty 'jkl)
                                          test-context)
                 '(" jkl >" " ," "<asdf,"))
   (check-equal? (write-chunk-with-length (template-list 'asdf empty 'jkl)
                                          test-context-2)
                 '("<asdf, , jkl >"))))

(define/provide-test-suite test-smt-list
  (test-case
   "Test smt-list"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (smt-list blank-line 'asdf space 'jkl)
                                          test-context)
                 '("jkl;" "" " ;" "" "asdf;"))
   (check-equal? (write-chunk-with-length (smt-list new-line
                                                    space
                                                    'asdf
                                                    empty)
                                          test-context)
                 '(";" "asdf;" " ;"))))


(define/provide-test-suite test-constructor-assignment-list
  (test-case
   "Test constructor-assignment-list"
   (define test-context 80)
   (define test-context-2 6)
   (check-equal? (write-chunk-with-length (constructor-assignment-list)
                                          test-context)
                 '(""))
   (check-equal? (write-chunk-with-length (constructor-assignment-list 'asdf)
                                          test-context)
                 '("  : asdf"))
   (check-equal? (write-chunk-with-length (constructor-assignment-list 'asdf 'jkl)
                                          test-context)
                 '("  : asdf, jkl"))
   (check-equal? (write-chunk-with-length (constructor-assignment-list 'asdf 'jkl)
                                          test-context-2)
                 '("    jkl"
                   "  : asdf,"))))

(define/provide-test-suite test-body
  (test-case
   "Test body"
   (define test-context 80)
   (define test-context-2 6)
   (check-equal? (write-chunk-with-length (body "123456")
                                          test-context-2)
                 '("}"
                   "   123456;"
                   "{"))
   (check-equal? (write-chunk-with-length (body 'asdf space 'jkl)
                                          test-context-2)
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
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-define 'name) test-context) '("#define name"))
   (check-equal? (write-chunk-with-length (pp-define (concat 'name "2")) test-context) '("#define name2"))
   (check-equal? (write-chunk-with-length (concat space (pp-define 'name)) test-context) '(" #define name"))
   (check-equal? (write-chunk-with-length (concat "/* " (pp-define 'name)) test-context) '("/* #define name"))))

(define/provide-test-suite test-pp-include
  (test-case
   "Test pp-include"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-include 'name) test-context) '("#include <name>"))
   (check-equal? (write-chunk-with-length (pp-include (concat 'name "2")) test-context) '("#include <name2>"))
   (check-equal? (write-chunk-with-length (concat space (pp-include 'name)) test-context) '(" #include <name>"))
   (check-equal? (write-chunk-with-length (concat "/* " (pp-include 'name)) test-context) '("/* #include <name>"))))

(define/provide-test-suite test-pp-includes
  (test-case
   "Test pp-includes"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-includes 'name) test-context) '("#include <name>"))
   (check-equal? (write-chunk-with-length (pp-includes 'name 'name2)
                                          test-context)
                 '("#include <name2>"
                   "#include <name>"))))

(define/provide-test-suite test-pp-ifdef
  (test-case
   "Test pp-ifdef"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-ifdef 'condition) test-context) '("#ifdef condition"))
   (check-equal? (write-chunk-with-length (pp-ifdef (concat 'condition "2")) test-context) '("#ifdef condition2"))
   (check-equal? (write-chunk-with-length (concat space (pp-ifdef 'condition)) test-context) '(" #ifdef condition"))
   (check-equal? (write-chunk-with-length (concat "/* " (pp-ifdef 'condition)) test-context) '("/* #ifdef condition"))))

(define/provide-test-suite test-pp-ifndef
  (test-case
   "Test pp-ifndef"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-ifndef 'condition) test-context) '("#ifndef condition"))
   (check-equal? (write-chunk-with-length (pp-ifndef (concat 'condition "2")) test-context) '("#ifndef condition2"))
   (check-equal? (write-chunk-with-length (concat space (pp-ifndef 'condition)) test-context) '(" #ifndef condition"))
   (check-equal? (write-chunk-with-length (concat "/* " (pp-ifndef 'condition)) test-context) '("/* #ifndef condition"))))

(define/provide-test-suite test-pp-else
  (test-case
   "Test pp-else"
   (define test-context 80)
   (check-equal? (write-chunk-with-length pp-else test-context) '("#else"))
   (check-equal? (write-chunk-with-length (concat space pp-else) test-context) '(" #else"))
   (check-equal? (write-chunk-with-length (concat "/* " pp-else) test-context) '("/* #else"))))

(define/provide-test-suite test-pp-endif
  (test-case
   "Test pp-endif"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-endif 'condition) test-context) '("/* condition */" "#endif"))
   (check-equal? (write-chunk-with-length (pp-endif (concat 'condition "2")) test-context) '("/* condition2 */" "#endif"))
   (check-equal? (write-chunk-with-length (concat space (pp-endif 'condition)) test-context) '("/* condition */" " #endif"))
   (check-equal? (write-chunk-with-length (concat "/* " (pp-endif 'condition)) test-context) '("/* condition */" "/* #endif"))))

(define/provide-test-suite test-pp-conditional
  (test-case
   "Test pp-conditional"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-conditional 'ifdef 'condition 'then)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-chunk-with-length (pp-conditional 'ifndef 'condition 'then 'else2)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-conditional-ifdef
  (test-case
   "Test pp-conditional-ifdef"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-conditional-ifdef 'condition 'then)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifdef condition"))
   (check-equal? (write-chunk-with-length (pp-conditional-ifdef 'condition 'then 'else2)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifdef condition"))))

(define/provide-test-suite test-pp-conditional-ifndef
  (test-case
   "Test pp-conditional-ifndef"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-conditional-ifndef 'condition 'then)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   then"
                   "#ifndef condition"))
   (check-equal? (write-chunk-with-length (pp-conditional-ifndef 'condition 'then 'else2)
                                          test-context)
                 '("/* condition */"
                   "#endif"
                   "   else2"
                   "#else"
                   "   then"
                   "#ifndef condition"))))

(define/provide-test-suite test-pp-header-file
  (test-case
   "Test pp-header-file"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (pp-header-file 'header_file empty 'asdf 'jkl)
                                          test-context)
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   ""
                   ""
                   "   #define header_file"
                   "#ifndef header_file"))
   (check-equal? (write-chunk-with-length (pp-header-file 'header_file
                                                          (concat (pp-include 'iostream)
                                                                  new-line
                                                                  (pp-include 'algorithm))
                                                          'asdf
                                                          'jkl)
                                          test-context)
                 '("/* header_file */"
                   "#endif"
                   ""
                   "   jkl"
                   ""
                   "   asdf"
                   ""
                   "   #include <algorithm>"
                   "   #include <iostream>"
                   ""
                   "   #define header_file"
                   "#ifndef header_file"))))

(define/provide-test-suite test-macro-define
  (test-case
   "Test macro-defintion"
   (define test-context 80)
   (define test-context-2 20)
   (check-equal? (write-chunk-with-length (macro-define 'name null 'asdf)
                                          test-context)
                 '("#define name asdf"))))

;general chunks

(define/provide-test-suite test-namespace-define
  (test-case
   "Test namespace-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (namespace-define 'name 'asdf)
                                          test-context)
                 '("namespace name { asdf; } /* name */"))
   (check-equal? (write-chunk-with-length (namespace-define 'name 'asdf)
                                          12)
                 '("} /* name */"
                   "   asdf;"
                   "namespace name {"))
   (check-equal? (write-chunk-with-length (namespace-define 'name 'asdf 'jkl)
                                          12)
                 '("} /* name */"
                   "   jkl;"
                   ""
                   "   asdf;"
                   "namespace name {"))))

(define/provide-test-suite test-described-smts
  (test-case
   "Test described-smts"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (described-smts 'name 'asdf)
                                          test-context)
                 '("asdf"
                   "/* name */"))
   (check-equal? (write-chunk-with-length (described-smts 'name 'asdf 'jkl)
                                          test-context)
                 '("jkl"
                   "asdf;"
                   "/* name */"))
   (check-equal? (write-chunk-with-length (described-smts 'name 'asdf 'jkl "1234")
                                          test-context)
                 '("1234"
                   "jkl;"
                   "asdf;"
                   "/* name */"))))

(define/provide-test-suite test-constize
  (test-case
   "Test constize"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (constize 'asdf)
                                          test-context)
                 '("asdf const"))
   (check-equal? (write-chunk-with-length (constize 'asdf)
                                          4)
                 '("asdf const"))))

;template chunks

(define/provide-test-suite test-template-define
  (test-case
   "Test template-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (template-define null 'asdf)
                                          test-context)
                 '(" asdf"
                   "template< >"))
   (check-equal? (write-chunk-with-length (template-define (list 'first) 
                                                           'asdf)
                                          test-context)
                 '(" asdf"
                   "template<first >"))
   (check-equal? (write-chunk-with-length (template-define (list 'first 'second)
                                                           'asdf)
                                          test-context)
                 '(" asdf"
                   "template<first, second >"))
   (check-equal? (write-chunk-with-length (template-define (list (concat (comment-env-chunk 'test)
                                                                         new-line
                                                                         'first)
                                                                 'second)
                                                           'asdf)
                                          test-context)
                 '(" asdf"
                   "         second >"
                   "         first,"
                   "template</* test */"))))

(define/provide-test-suite test-template-use
  (test-case
   "Test template-defintion"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (template-use 'name null)
                                          test-context)
                 '("name"))
   (check-equal? (write-chunk-with-length (template-use 'name (list 'first))
                                          test-context)
                 '("name<first >"))
   (check-equal? (write-chunk-with-length (template-use 'name (list 'first 'second))
                                          test-context)
                 '("name<first, second >"))
   (check-equal? (write-chunk-with-length (template-use 'name (list 'first 'second))
                                          6)
                 '("     second >"
                   "name<first,"))))

;function chunks

(define/provide-test-suite test-function-declare
  (test-case
   "Test function-declare"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (function-declare 'name 'return-type null)
                                          test-context)
                 '("inline return-type name(void)"))
   (check-equal? (write-chunk-with-length (function-declare 'name 'return-type (list 'first))
                                          test-context)
                 '("inline return-type name(first)"))
   (check-equal? (write-chunk-with-length (function-declare 'name 'return-type (list 'first 'second))
                                          test-context)
                 '("inline return-type name(first, second)"))
   (check-equal? (write-chunk-with-length (function-declare 'name 'return-type (list 'first 'second))
                                          8)
                 '("     second)"
                   "name(first,"
                   "return-type"
                   "inline"))))

(define/provide-test-suite test-static-function-declare
  (test-case
   "Test static-function-declare"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (static-function-declare 'name 'return-type null)
                                          test-context)
                 '("static inline return-type name(void)"))
   (check-equal? (write-chunk-with-length (static-function-declare 'name 'return-type (list 'first))
                                          test-context)
                 '("static inline return-type name(first)"))
   (check-equal? (write-chunk-with-length (static-function-declare 'name 'return-type (list 'first 'second))
                                          test-context)
                 '("static inline return-type name(first, second)"))
   (check-equal? (write-chunk-with-length (static-function-declare 'name 'return-type (list 'first 'second))
                                          20)
                 '("                 second)"
                   "return-type name(first,"
                   "static inline"))))

(define/provide-test-suite test-void-function-declare
  (test-case
   "Test void-function-declare"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (void-function-declare 'name null)
                                          test-context)
                 '("inline void name(void)"))
   (check-equal? (write-chunk-with-length (void-function-declare 'name (list 'first))
                                          test-context)
                 '("inline void name(first)"))
   (check-equal? (write-chunk-with-length (void-function-declare 'name (list 'first 'second))
                                          test-context)
                 '("inline void name(first, second)"))
   (check-equal? (write-chunk-with-length (void-function-declare 'name (list 'first 'second))
                                          20)
                 '("                 second)"
                   "inline void name(first,"))))

(define/provide-test-suite test-function-define
  (test-case
   "Test function-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (function-define 'signature)
                                          test-context)
                 '("signature {}"))
   (check-equal? (write-chunk-with-length (function-define 'signature 'first)
                                          test-context)
                 '("signature { first; }"))
   (check-equal? (write-chunk-with-length (function-define 'signature 'first 'second)
                                          20)
                 '("}"
                   "   second;"
                   ""
                   "   first;"
                   "signature {"))))

(define/provide-test-suite test-void-function-define
  (test-case
   "Test void-function-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (void-function-define 'name null null)
                                          test-context)
                 '("inline void name(void) {}"))
   (check-equal? (write-chunk-with-length (void-function-define 'name
                                                                (list 'first)
                                                                (list 'first))
                                          test-context)
                 '("inline void name(first) { first; }"))
   (check-equal? (write-chunk-with-length (void-function-define 'name
                                                                (list 'first 'second)
                                                                (list 'first 'second))
                                          test-context)
                 '("inline void name(first, second) { first; second; }"))
   (check-equal? (write-chunk-with-length (void-function-define 'name
                                                                (list 'first 'second)
                                                                (list 'first 'second))
                                          10)
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
   (define test-context 80)
   (check-equal? (write-chunk-with-length (returning-function-define (function-declare 'name 'return-type null)
                                                                     null
                                                                     'expr)
                                          test-context)
                 '("inline return-type name(void) { return expr; }"))
   (check-equal? (write-chunk-with-length (returning-function-define (function-declare 'name 'return-type (list 'first))
                                                                       (list 'first)
                                                                       'expr)
                                            test-context)
                   '("}"
                     "   return expr;"
                     ""
                     "   first;"
                     "inline return-type name(first) {"))
   (check-equal? (write-chunk-with-length (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                     (list 'first 'second)
                                                                     'expr)
                                          test-context)
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "inline return-type name(first, second) {"))
   (check-equal? (write-chunk-with-length (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                     (list 'first 'second)
                                                                     'expr)
                                          20)
                 '("}"
                   "   return expr;"
                   ""
                   "   second;"
                   ""
                   "   first;"
                   "name(first, second) {"
                   "inline return-type"))
   (check-equal? (write-chunk-with-length (returning-function-define (function-declare 'name 'return-type (list 'first 'second))
                                                                     (list 'first 'second)
                                                                     'expr)
                                          10)
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
   (define test-context 80)
   (check-equal? (write-chunk-with-length (constructor-assignment 'first 'second)
                                          test-context)
                 '("first(second)"))))

(define/provide-test-suite test-constructor
  (test-case
   "Test constructor"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (constructor 'name null null)
                                          test-context)
                 '("name() {}"))
   (check-equal? (write-chunk-with-length (constructor 'name
                                                       (list 'first)
                                                       (list 'assign)
                                                       'asdf)
                                          test-context)
                 '("{ asdf; }"
                   "  : assign"
                   "name(first)"))
   (check-equal? (write-chunk-with-length (constructor 'name
                                                       (list 'first 'second)
                                                       (list 'assign1 'assign2)
                                                       'asdf
                                                       'jkl)
                                          test-context)
                 '("}"
                   "   jkl;"
                   ""
                   "   asdf;"
                   "{"
                   "  : assign1, assign2"
                   "name(first, second)"))))

;class/struct chunks

(define/provide-test-suite test-struct-declare
  (test-case
   "Test struct-declare"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (struct-declare 'name)
                                          test-context)
                 '("struct name"))))

(define/provide-test-suite test-template-struct-declare
  (test-case
   "Test template-struct-declare"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (template-struct-declare 'name null null)
                                          test-context)
                 '(" struct name"
                   "template< >"))
   (check-equal? (write-chunk-with-length (template-struct-declare 'name
                                                                   (list 'first)
                                                                   (list 'first))
                                          test-context)
                 '(" struct name<first >"
                   "template<first >"))
   (check-equal? (write-chunk-with-length (template-struct-declare 'name
                                                                   (list 'first 'second)
                                                                   (list 'first 'second))
                                          test-context)
                 '(" struct name<first, second >"
                   "template<first, second >"))
   (check-equal? (write-chunk-with-length (template-struct-declare 'name
                                                                   (list 'first)
                                                                   (list 'first 'second))
                                          test-context)
                 '(" struct name<first, second >"
                   "template<first >"))))

(define/provide-test-suite test-section-define
  (test-case
   "Test section-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (section-define 'name 'first 'second)
                                          test-context)
                 '(" second"
                   ""
                   " first"
                   "name:"))))

(define/provide-test-suite test-struct-define
  (test-case
   "Test struct-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (struct-define 'signature 'first 'second)
                                          test-context)
                 '("}"
                   "   second"
                   ""
                   "   first"
                   "signature {"))
   (check-equal? (write-chunk-with-length (struct-define 'signature 'first 'second)
                                          8)
                 '("}"
                   "   second"
                   ""
                   "   first"
                   "signature {"))))

(define/provide-test-suite test-template-struct-define
  (test-case
   "Test template-struct-define"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (template-struct-define 'name
                                                                  (list 'first 'second)
                                                                  null
                                                                  'first
                                                                  'second)
                                          test-context)
                 '("}"
                   "   second"
                   ""
                   "   first"
                   " struct name {"
                   "template<first, second >"))
   (check-equal? (write-chunk-with-length (template-struct-define 'name
                                                                  (list 'first 'second)
                                                                  null
                                                                  'first
                                                                  'second)
                                          12)
                 '("}"
                   "   second"
                   ""
                   "   first"
                   " struct name {"
                   "         second >"
                   "template<first,"))))

(define/provide-test-suite test-scope-resolution-operator
  (test-case
   "Test scope-resolution-operator"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (scope-resolution-operator 'first 'second)
                                          test-context)
                 '("first::second"))))

;statement chunks

(define/provide-test-suite test-typedef-smt
  (test-case
   "Test typedef-smt"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (typedef-smt 'lhs 'rhs)
                                          test-context)
                 '("lhs typedef rhs"))))

(define/provide-test-suite test-function-call
  (test-case
   "Test function-call"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (function-call 'name)
                                          test-context)
                 '("name()"))
   (check-equal? (write-chunk-with-length (function-call 'name 'first)
                                          test-context)
                 '("name(first)"))
   (check-equal? (write-chunk-with-length (function-call 'name 'first 'second)
                                          test-context)
                 '("name(first, second)"))
   (check-equal? (write-chunk-with-length (function-call 'name 'first 'second)
                                          4)
                 '("     second)"
                   "name(first,"))))

(define/provide-test-suite test-member-function-call
  (test-case
   "Test member-function-call"
   (define test-context 80)
   (check-equal? (write-chunk-with-length (member-function-call 'obj 'name)
                                          test-context)
                 '("obj.name()"))
   (check-equal? (write-chunk-with-length (member-function-call 'obj 'name 'first)
                                          test-context)
                 '("obj.name(first)"))
   (check-equal? (write-chunk-with-length (member-function-call 'obj 'name 'first 'second)
                                          test-context)
                 '("obj.name(first, second)"))
   (check-equal? (write-chunk-with-length (member-function-call 'object 'name 'first 'second)
                                          4)
                 '("            second)"
                   "object.name(first,"))))
