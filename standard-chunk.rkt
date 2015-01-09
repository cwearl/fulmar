#lang typed/racket

(require "private/core-chunk.rkt"
         "private/doc/document.rkt")

; from core-chunk
(provide
 flatten*
 literal
 space
 new-line
 empty
 concat
 immediate
 speculative
 position-indent
 indent
 comment-env-chunk)

(provide 
 (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;
;basic chunks;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(document empty*?
"Mostly a helper for other standard chunks. Takes anything as its input. If the
input is an empty list or a list of nested lists that are all empty, returns #t.
For ANY OTHER INPUT, returns #f.")
(: empty*? (Any -> Boolean))
(define (empty*? lst)
  (match lst
    ['() #t]
    [(list xs ...) (for/and: : Boolean ([x xs]) (empty*? x))]
    [_ #f]))

(document if-empty
"Macro of the form: (if-empty given then else)"
"Returns then, if given is null (or a list that flattens to null).
 Returns else, otherwise.")
(define-syntax if-empty
  (syntax-rules ()
    [(if-empty given then else)
     (if (empty*? given)
         then
         else)]))

(document surround
"Surrounds the second given chunk with copies of the first.
 Useful if you need to make sure the President is always guarded by the Secret
 Service:"
"(surround \"SS\" \"president\") => \"SSpresidentSS\"")
(: surround (Chunk Chunk -> Chunk))
(define (surround surround chunk)
  (concat surround chunk surround))

(document blank-lines
"Adds n blank lines."
"Actually, you'll notice that this function accepts any number of integers.
 If you provide more than one, the numbers will be added together to produce
 the number of blank lines that will be emitted. This might be useful if, for
 example, you know you want 2 blank lines, then n more.")
(: blank-lines (Integer * -> Chunk))
(define (blank-lines . lengths)
  (concat (ann (make-list (apply +  (cons 1 lengths))
                          new-line) (Listof Chunk))))

(document blank-line
"Adds one blank line")
(define blank-line (blank-lines 1))

(document sur-paren
"Surround a chunk in parenthesis. No, really! See:"
"(sur-paren \"chunk\") => \"(chunk)\""
"It even works with multiple chunks:"
"(sur-paren \"A\" \"chunk\") => \"(Achunk)\"")
(: sur-paren (Chunk * -> Chunk))
(define (sur-paren . chunks)
  (concat (immediate "(")
          chunks
          (immediate ")")))

(document sur-crbr
"Surround a chunk in curly brackets. Same as sur-paren, but with these: {}")
(: sur-crbr (Chunk * -> Chunk))
(define (sur-crbr . chunks)
  (concat (immediate "{")
          chunks
          (immediate "}")))

(document sur-anbr
"Surround a chunk in angle brackets. Same as sur-paren, but with these: <>")
(: sur-anbr (Chunk * -> Chunk))
(define (sur-anbr . chunks)
  (concat (immediate "<")
          chunks
          (immediate ">")))

(document sur-sqbr
"Surround a chunk in square brackets. Same as sur-paren, but with these: []")
(: sur-sqbr (Chunk * -> Chunk))
(define (sur-sqbr . chunks)
  (concat (immediate "[")
          chunks
          (immediate "]")))

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(document attach-list-separator
"Takes a list separator chunk, and a list of chunks. Takes each chunk in the
 list except the last and appends a copy of the separator chunk to it. The
 result is a list of chunks with list separators. Before you consider using this
 function directly, be sure you wouldn't rather use between/attach."
"(attach-list-separator \", \" `(a b c d e)) => \"a, b, c, d, e\""
"You can also give this function its list as a rest argument. It will take any
 arguments after the first to be chunks that need separators:"
"(attach-list-separator \", \" `a 'b 'c 'd 'e) => \"a, b, c, d, e\""
"Note: This function flattens. So (attach-list-separator 'x '(a b) 'c 'd) will
 give \"axbxcxd\", NOT \"abxcxd\".")
(: attach-list-separator (Chunk NestofChunks * -> (Listof Chunk)))
(define (attach-list-separator to-attach . chunk-lists)
  (define chunks (flatten* chunk-lists))
  (define lmap (inst map Chunk Chunk))
  (if-empty chunks
            null
            (flatten* (lmap (λ: ([chunk : Chunk]) (concat chunk (immediate to-attach)))
                            (take chunks (- (length chunks) 1))) (last chunks))))

(document between
"Takes an add-between chunk, and a list of chunks. Concatenates given chunks
 with add-between between each of the given chunks. Operates similarly to
 attach-list-separator, but does not assume you want to keep the add-between
 chunk paired with the preceding list chunk.")
(: between (Chunk NestofChunks * -> Chunk))
(define (between add-between-chunk . chunks)
  (define ladd-between (inst add-between Chunk Chunk))
  (concat (ladd-between (flatten* chunks) add-between-chunk)))

(document between/attach
"Combines between and attach functionality to give a convenient way of making
 multi-line lists or function bodies in an intelligent way. Takes a to-attach
 chunk, an add-between chunk, and then a list of chunks to build into the list.
 It then attaches the to-attach chunk to each chunk in the list using
 attach-list-separator. Finally, it uses the between function to insert the
 add-between chunk after each to-attach."
"This function will work hard to do the Right Thing™ with lists or function
 bodies. It tries, for example, to keep commas on the same line as the preceding
 list element."
"Assuming line-length was parameterized to a value of 10:"
"(between/attach \",\" space (range 8))"
"=>"
"\"0, 1, 2, 3,\""
"\"4, 5, 6, 7\"")
(: between/attach (Chunk Chunk NestofChunks * -> Chunk))
(define (between/attach to-attach add-between . chunks)
  (apply between add-between (apply attach-list-separator to-attach chunks)))

(document arg-list
"Argument list chunk"
"Attempts to put chunks on a single line with a space between each chunk.
 If that fails, puts chunks on their own lines. Commonly useful for lists and
 arguments to functions. Handier than rolling something for this purpose by
 hand.")
(: arg-list ((Chunk -> Chunk) Chunk NestofChunks * -> Chunk))
(define (arg-list sur attach . chunks)
  (: build (Chunk -> Chunk))
  (define (build spacing)
    (apply between/attach attach spacing chunks))
  (sur (if-empty chunks
                 empty
                 (speculative (build space)
                              length-equals-one
                              (position-indent (build new-line))))))

(document paren-list
"Parenthesis argument list chunk"
"Takes any list of chunks, separates them with commas, and surrounds the whole
 list with parenthesis.")
(: paren-list (NestofChunks * -> Chunk))
(define (paren-list . chunks)
  (apply arg-list sur-paren "," chunks))

(document template-list
"Template argument list chunk"
"Takes any list of chunks, separates them with commas, and surrounds the whole
 list with angle brackets.")
(: template-list (NestofChunks * -> Chunk))
(define (template-list . chunks)
  (apply arg-list sur-anbr "," chunks))

(document body-list
"Takes a separator chunk and a list of chunks, separates the chunks with the
 separator, and surrounds the whole thing with curly braces.")
(: body-list (Chunk NestofChunks * -> Chunk))
(define (body-list attach . chunks)
  (apply arg-list sur-crbr attach chunks))

(document top-list
"Takes any number of chunks as arguments and inserts a blank line between each.")
(: top-list (NestofChunks * -> Chunk))
(define (top-list . chunks)
  (apply between blank-line chunks))

(document internal-smt-list
"List of statement chunks without final semi-colon."
"Takes a spacing chunk and any number of chunks, adds spacing between each chunk
 and attaches a semi-colon to the end of each chunk (except the last)."
"(internal-smt-list space 'a 'b 'c) => \"a; b; c\"")
(: internal-smt-list (Chunk NestofChunks * -> Chunk))
(define (internal-smt-list spacing . chunks)
  (apply between/attach ";" spacing chunks))

(document smt-list
"Takes a spacing chunk and any number of chunks, adds spacing added between each
 chunk and attaches a semi-colon to the end of each chunk."
"(smt-list space 'a 'b 'c) => \"a; b; c;\"")
(: smt-list (Chunk NestofChunks * -> Chunk))
(define (smt-list spacing . chunks)
  (if-empty chunks
            empty
            (concat (apply internal-smt-list spacing chunks)
                    (immediate ";"))))

(document constructor-assignment-list
"Constructor assignment list chunk"
"Each assignment is separated by a comma - first line is indented 2 spaces and
 begun with a colon."
"(constructor-assignment-list \"a(other.a)\" \"b(other.b)\" \"c(other.c)\" => \"  : a(other.a), b(other.b), c(other.c)\"")
(: constructor-assignment-list (NestofChunks * -> Chunk))
(define (constructor-assignment-list . chunks)
  (: build (Chunk -> Chunk))
  (define (build spacing)
    (indent 2 (concat ":"
                      (immediate space)
                      (position-indent (apply between/attach "," spacing chunks)))))
  (if-empty chunks
            empty
            (speculative (build space)
                         length-equals-one
                         (build new-line))))

(document general-body
"General body chunk"
"Takes a boolean (use-semi-colons) and any number of chunks."
"Surrounds chunks with curly brackets"
"- adds a semi-colon after each chunk, if use-semi-colons is true"
"- attempts to put chunks on a single line with a space between each chunk"
"- if that fails, puts chunks on their own lines with indented"
"This version places the open curly bracket immediately on current line and the
 close curly bracket on its own line. This roughly corresponds to Java or
 Google style, with attached brackets. The indent level is 3 spaces. This is not
 currently user-configurable.")
(: general-body (Boolean NestofChunks * -> Chunk))
(define (general-body use-semi-colons . chunks)
  (: build (Chunk Chunk -> Chunk))
  (define (build start/end-spacing spacing)
    (surround start/end-spacing
              (if use-semi-colons
                  (apply smt-list spacing chunks)
                  (apply between spacing chunks))))
  (sur-crbr (if-empty chunks
                      empty
                      (if (= 1 (length chunks))
                          (speculative 
                           (build space space)
                           length-equals-one
                           (indent 3 (build new-line blank-line)))
                          (indent 3 (build new-line blank-line))))))

(document body
"Body with semicolons"
"Basically a shortcut for (general-body #true ...)"
"Surrounds chunks with curly brackets"
"- adds a semi-colon after each chunk"
"- attempts to put chunks on a single line with a space between each chunk"
"- if that fails, puts chunks on their own lines with indented"
"Style is the same as with general-body above.")
(: body (NestofChunks * -> Chunk))
(define (body . chunks)
  (apply general-body #true chunks))

(document class-body
"Body without semicolons"
"Basically a shortcut for (general-body #false ...)"
"Surrounds chunks with curly brackets"
"- does NOT add semi-colons after each chunk"
"- attempts to put chunks on a single line with a space between each chunk"
"- if that fails, puts chunks on their own lines with indented"
"Style is the same as with general-body above.")
(: class-body (NestofChunks * -> Chunk))
(define (class-body . chunks)
  (apply general-body #false chunks))

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

(document pp-define
"Preprocessor define chunk"
"Accepts one argument, a name. Produces:"
"#define <name>")
(: pp-define (Chunk -> Chunk))
(define (pp-define name)
  (concat pp-directive 'define space name))

(document pp-include
"Preprocessor include chunk"
"Accepts one argument, a header filename. Produces:"
"#include <<filename>>"
"Where the outer pointy brackets are literal.")
(: pp-include (Chunk -> Chunk))
(define (pp-include included)
  (concat pp-directive 'include space (template-list included)))

(document pp-alt-include
"Alternate preprocessor include chunk"
"Accepts one argument, a header filename. Produces:"
"#include \"<filename>\"")
(: pp-alt-include (Chunk -> Chunk))
(define (pp-alt-include included)
  (concat pp-directive 'include space "\"" included "\""))

(document pp-includes
"Multiple includes"
"Accepts any number of arguments and produces an include directive for each.")
(: pp-includes (Chunk * -> Chunk))
(define (pp-includes . chunks)
  (apply between new-line (map pp-include (flatten* chunks))))

(document pp-ifdef
"Preprocessor if-defined chunk"
"Accepts one argument, a preprocessor ifdef condition, and makes an ifdef line.")
(: pp-ifdef (Chunk -> Chunk))
(define (pp-ifdef condition)
  (concat pp-directive 'ifdef space condition))

(document pp-ifndef
"Preprocessor if-not-defined chunk"
"Makes an ifndef preprocessor directive.")
(: pp-ifndef (Chunk -> Chunk))
(define (pp-ifndef condition)
  (concat pp-directive 'ifndef space condition))

(document pp-else
"Preprocessor else chunk"
"Makes an else preprocessor directive. Constant value, should not be called.")
(define pp-else (concat pp-directive 'else))

(document pp-endif
"Preprocessor endif chunk"
"Makes an endif preprocessor directive. The argument will end up in a comment
 on the same line as the endif, and can be used to specify what condition is
 being endifed.")
(: pp-endif (Chunk -> Chunk))
(define (pp-endif condition)
  (concat pp-directive 'endif new-line (comment-env-chunk condition)))

(document pp-conditional
"Preprocessor conditional chunk"
"General preprocessor conditional block in a single handy package. Accepts 3
 required arguments and one optional argument: directive, condition, then, else."
"directive - the preprocessor command that comes right after the #."
"condition - argument to the directive, following on the same line."
"then - Chunk of code to be placed before the endif."
"else - if present, chunk of code to go after an else, between then and endif."
"Example: (pp-conditional 'ifdef ARM64 \"do_arm64();\" \"reticulate_splines();\")")
(: pp-conditional (case->
                   [Chunk Chunk Chunk -> Chunk]
                   [Chunk Chunk Chunk (U Chunk False) -> Chunk]))
(define (pp-conditional directive condition then [else #false])
  (concat pp-directive
          directive
          space
          condition
          new-line
          (indent 3 then)
          new-line
          (if else
              (concat pp-else
                      new-line
                      (indent 3 else)
                      new-line)
              empty)
          (pp-endif condition)))

(document pp-conditional-ifdef
"Preprocessor conditional ifdef chunk"
"Basically a shortcut for (pp-conditional 'ifdef ...)"
"Makes a preprocessor ifdef line with then and optional else bodies. Its 2
 required arguments are the name of the define to check, and the 'then' body.
 The optional argument is an 'else' body.")
(: pp-conditional-ifdef (case->
                         [Chunk Chunk -> Chunk]
                         [Chunk Chunk (U False Chunk) -> Chunk]))
(define (pp-conditional-ifdef condition then [else #false])
  (pp-conditional 'ifdef condition then else))

(document pp-conditional-ifndef
"Preprocessor conditional ifndef chunk"
"Basically a shortcut for (pp-conditional 'ifndef ...)"
"Makes a preprocessor ifndef line with then and optional else bodies. Its 2
 required arguments are the name of the define to check, and the 'then' body.
 The optional argument is an 'else' body.")
(: pp-conditional-ifndef (case->
                          [Chunk Chunk -> Chunk]
                          [Chunk Chunk (U False Chunk) -> Chunk]))
(define (pp-conditional-ifndef condition then [else #false])
  (pp-conditional 'ifndef condition then else))

(document pp-header-file
"Preprocessor include guard / header file wrapper chunk"
"This is a pretty standard #include guard. Takes at least 1 argument: a name to
 define (usually something like 'MY_HEADER_H). Further arguments will become the
 body of the header file."
"Example:"
"(pp-header-file 'FOO_STRUCT_H \"struct foo { int member; };\")"
"Produces:"
"#ifndef FOO_STRUCT_H"
"#define FOO_STRUCT_H"
"   struct foo { int member; };"
"#endif"
"/* FOO_STRUCT_H */")
(: pp-header-file (Chunk NestofChunks * -> Chunk))
(define (pp-header-file file-name . chunks)
  (pp-conditional-ifndef file-name
                         (concat (pp-define file-name)
                                 blank-line
                                 (apply top-list chunks)
                                 new-line)))

(document macro-define
"Macro defintion chunk"
"General macro definition. Accepts at least one argument. The first will be the
 name of the macro. Following arguments will become the definition of the macro.
 Be careful, as it's quite easy to create invalid code if your definition
 contains newlines.")
(: macro-define (Chunk NestofChunks Chunk -> Chunk))
(define (macro-define name params chunk)
  (immediate (concat (pp-define name) space chunk)))

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(document namespace-define
"Declare a namespace for a block of code."
"Accepts a name and any number of further arguments. The name goes after the
 namespace keyword, and the remaining arguments get put into a body which goes
 in the namespace block. After the code block, a comment containing the name is
 added to clarify what is being ended. Example:"
"(namespace-define 'foo \"bar()\") =>"
"namespace foo { bar(); } /* foo */")
(: namespace-define (Chunk NestofChunks * -> Chunk))
(define (namespace-define name . chunks)
  (define chunk (concat 'namespace
                        (immediate space)
                        (immediate name)
                        (immediate space)
                        (apply body chunks)))
  
  (concat chunk space (comment-env-chunk name)))

(document described-smts
"Described statements chunk"
"Accepts a comment and any number of other chunks. Puts the comment into a
 comment environment (surrounds it with /* */) and then places the remaining
 arguments normally, after a newline. Example:"
"(described-smts \"Automatically synchronize cardinal grammaters\" \"encabulate();\") =>"
"/* Automatically synchronize cardinal grammaters */"
"encabulate();")
(: described-smts (Chunk NestofChunks * -> Chunk))
(define (described-smts comment . chunks)
  (concat (comment-env-chunk comment)
          new-line
          (apply between/attach ";" new-line chunks)))

(document constize
"Make constant"
"To be used with types or identifiers. Takes a type, identifier, or any chunk,
 and appends const to the end of it."
"(constize \"int a\") =>"
"int a const")
(: constize (Chunk -> Chunk))
(define (constize chunk)
  (concat chunk
          (immediate space)
          (immediate 'const)))

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(document template-define
"Make a template with given template parameters"
"Takes two arguments: a list of parameters, and a name. Produces a template
 declaration (sans the final semicolon)."
"Example:"
"(template-define '(p1 p2 p3) 'MyTemplate) =>"
"template<p1, p2, p3>"
"MyTemplate")
(: template-define (NestofChunks Chunk -> Chunk))
(define (template-define params chunk)
  (concat 'template
          (template-list params)
          new-line
          (indent 1 chunk)))

(document template-use
"Make use of a template"
"Takes a name and any number of template arguments. Produces a reference to the
 given template with the given arguments. If only a name is given, no pointy
 brackets are produced."
"Example:"
"(template-use 'foo 'bar 'baaz) =>"
"foo<bar, baaz>")
(: template-use (NestofChunks NestofChunks * -> Chunk))
(define (template-use name . args)
  (concat name (if-empty args empty (apply template-list args))))

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(document general-function-declare
"General function declaration"
"Takes a name, a return type, and any number of additional arguments."
"Example:"
"(general-function-declare 'foo 'bar "int baaz" "float quux") =>"
"bar foo(int baaz, float quux)")
(: general-function-declare (Chunk Chunk NestofChunks * -> Chunk))
(define (general-function-declare name return-type . params)
  (concat return-type space name (apply paren-list (if-empty params
                                                         '(void)
                                                         params))))

;function declaration
(: function-declare (Chunk Chunk NestofChunks * -> Chunk))
(define (function-declare name return-type . params)
  (concat 'inline space (apply general-function-declare name return-type params)))

;static function declaration
(: static-function-declare (Chunk Chunk NestofChunks * -> Chunk))
(define (static-function-declare name return-type . params)
  (concat 'static space (apply function-declare name return-type params)))

;void function declaration
(: void-function-declare (Chunk NestofChunks -> Chunk))
(define (void-function-declare name params)
  (function-declare name 'void params))

;function defintion
(: function-define (Chunk NestofChunks * -> Chunk))
(define (function-define signature . chunks)
  (concat signature
          (immediate space)
          (apply body chunks)))

;void function defintion
(: void-function-define (Chunk NestofChunks NestofChunks * -> Chunk))
(define (void-function-define name params . body)
  (apply function-define (void-function-declare name params)
         body))

;returning function defintion
(: returning-function-define (Chunk NestofChunks Chunk -> Chunk))
(define (returning-function-define signature body return-expr)
  (apply function-define signature (flatten* body (concat 'return
                                                          (immediate space)
                                                          (position-indent return-expr)))))

;constructor assignment
(: constructor-assignment (Chunk NestofChunks * -> Chunk))
(define (constructor-assignment var . val)
  (concat var (apply paren-list val)))

;constructor defintion
(: constructor (Chunk NestofChunks NestofChunks NestofChunks * -> Chunk))
(define (constructor name params assigns . chunks)
  (concat name 
          (paren-list params)
          (if-empty assigns
                    (immediate space)
                    (surround new-line (constructor-assignment-list assigns)))
          (apply body chunks)))

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(: struct-declare (Chunk -> Chunk))
(define (struct-declare name)
  (concat 'struct space name))

;template struct declaration
(: template-struct-declare (Chunk NestofChunks NestofChunks * -> Chunk))
(define (template-struct-declare name params . args)
  (template-define params (apply template-use (struct-declare name)
                                 args)))

;struct section
(: section-define (Chunk NestofChunks * -> Chunk))
(define (section-define type . chunks)
  (if-empty chunks
            empty
            (concat type ":" new-line (indent 1 (apply between blank-line chunks)))))

;public section
(: public-section (NestofChunks * -> Chunk))
(define (public-section . chunks)
  (apply section-define 'public chunks))

;private section
(: private-section (NestofChunks * -> Chunk))
(define (private-section . chunks)
  (apply section-define 'private chunks))

;protected section
(: protected-section (NestofChunks * -> Chunk))
(define (protected-section . chunks)
  (apply section-define 'protected chunks))

;struct definition
(: struct-define (Chunk NestofChunks * -> Chunk))
(define (struct-define signature . body)
  (concat signature
          (immediate space)
          (apply class-body body)))

;template struct definition
(: template-struct-define (Chunk NestofChunks NestofChunks NestofChunks * -> Chunk))
(define (template-struct-define name params args . body)
  (apply struct-define (template-struct-declare name params args)
         body))

;scope resolution operator
(: scope-resolution-operator (Chunk Chunk -> Chunk))
(define (scope-resolution-operator scope variable)
  (concat scope
          (immediate ":")
          (immediate ":")
          variable))

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(: typedef-smt (Chunk Chunk -> Chunk))
(define (typedef-smt lhs rhs)
  (concat lhs space 'typedef space rhs))

;function call
(: function-call (Chunk NestofChunks * -> Chunk))
(define (function-call fcn . args)
  (concat fcn (apply paren-list args)))

;member function call
(: member-function-call (Chunk Chunk NestofChunks * -> Chunk))
(define (member-function-call obj fcn . args)
  (concat obj
          (immediate ".")
          (position-indent (apply function-call fcn args))))

;array access
(: array-access (Chunk Chunk * -> Chunk))
(define (array-access array . arg)
  (concat array (apply sur-sqbr arg)))
