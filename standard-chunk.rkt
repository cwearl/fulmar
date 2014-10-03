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
 list except the last and appends a copy of the separator chunk to it. Then
 concatenates the whole list together, in order."
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

;insert a chunk between other chunks
; concatenates given chunks with add-between between given chunks
(: between (Chunk NestofChunks * -> Chunk))
(define (between add-between-chunk . chunks)
  (define ladd-between (inst add-between Chunk Chunk))
  (concat (ladd-between (flatten* chunks) add-between-chunk)))

; combine between and attach functionality
;  adds to-add after each of the given chunks
;    and then adds add-between between new chunks
(: between/attach (Chunk Chunk NestofChunks * -> Chunk))
(define (between/attach to-attach add-between . chunks)
  (apply between add-between (apply attach-list-separator to-attach chunks)))

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
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

;parenthesis argument list chunk
(: paren-list (NestofChunks * -> Chunk))
(define (paren-list . chunks)
  (apply arg-list sur-paren "," chunks))

;template argument list chunk
(: template-list (NestofChunks * -> Chunk))
(define (template-list . chunks)
  (apply arg-list sur-anbr "," chunks))

;body list chunk
(: body-list (Chunk NestofChunks * -> Chunk))
(define (body-list attach . chunks)
  (apply arg-list sur-crbr attach chunks))

;list of chunks
; blank line added between each chunk
(: top-list (NestofChunks * -> Chunk))
(define (top-list . chunks)
  (apply between blank-line chunks))

;list of statement chunks without final semi-colon
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk (except last)
(: internal-smt-list (Chunk NestofChunks * -> Chunk))
(define (internal-smt-list spacing . chunks)
  (apply between/attach ";" spacing chunks))

;list of statement chunks
; adds spacing added between each chunk
;  and attaches a semi-colon to the end of each chunk
(: smt-list (Chunk NestofChunks * -> Chunk))
(define (smt-list spacing . chunks)
  (if-empty chunks
            empty
            (concat (apply internal-smt-list spacing chunks)
                    (immediate ";"))))

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
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

;general body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk, if use-semi-colons is true
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
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

;body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(: body (NestofChunks * -> Chunk))
(define (body . chunks)
  (apply general-body #true chunks))

;class body chunk
; surrounds chunks with curly brackets
; - does NOT add semi-colons after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(: class-body (NestofChunks * -> Chunk))
(define (class-body . chunks)
  (apply general-body #false chunks))

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(: pp-define (Chunk -> Chunk))
(define (pp-define name)
  (concat pp-directive 'define space name))

;preprocessor include chunk
; #include <...> chunk
(: pp-include (Chunk -> Chunk))
(define (pp-include included)
  (concat pp-directive 'include space (template-list included)))

;alternate preprocessor include chunk
; #include "..." chunk
(: pp-alt-include (Chunk -> Chunk))
(define (pp-alt-include included)
  (concat pp-directive 'include space "\"" included "\""))

;multiple includes
(: pp-includes (Chunk * -> Chunk))
(define (pp-includes . chunks)
  (apply between new-line (map pp-include (flatten* chunks))))

;preprocessor if-not-defined chunk
(: pp-ifdef (Chunk -> Chunk))
(define (pp-ifdef condition)
  (concat pp-directive 'ifdef space condition))

;preprocessor if-not-defined chunk
(: pp-ifndef (Chunk -> Chunk))
(define (pp-ifndef condition)
  (concat pp-directive 'ifndef space condition))

;preprocessor if-not-defined chunk
(define pp-else (concat pp-directive 'else))

;preprocessor endif chunk
(: pp-endif (Chunk -> Chunk))
(define (pp-endif condition)
  (concat pp-directive 'endif new-line (comment-env-chunk condition)))

;preprocessor conditional chunk
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

;preprocessor conditional ifdef chunk
(: pp-conditional-ifdef (case->
                         [Chunk Chunk -> Chunk]
                         [Chunk Chunk (U False Chunk) -> Chunk]))
(define (pp-conditional-ifdef condition then [else #false])
  (pp-conditional 'ifdef condition then else))

;preprocessor conditional ifndef chunk
(: pp-conditional-ifndef (case->
                          [Chunk Chunk -> Chunk]
                          [Chunk Chunk (U False Chunk) -> Chunk]))
(define (pp-conditional-ifndef condition then [else #false])
  (pp-conditional 'ifndef condition then else))

;preprocessor h file wrapper chunk
(: pp-header-file (Chunk NestofChunks * -> Chunk))
(define (pp-header-file file-name . chunks)
  (pp-conditional-ifndef file-name
                         (concat (pp-define file-name)
                                 blank-line
                                 (apply top-list chunks)
                                 new-line)))

;macro defintion chunk
; a macro definition
(: macro-define (Chunk NestofChunks Chunk -> Chunk))
(define (macro-define name params chunk)
  (immediate (concat (pp-define name) space chunk)))

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(: namespace-define (Chunk NestofChunks * -> Chunk))
(define (namespace-define name . chunks)
  (define chunk (concat 'namespace
                        (immediate space)
                        (immediate name)
                        (immediate space)
                        (apply body chunks)))
  
  (concat chunk space (comment-env-chunk name)))

;described statements chunk
(: described-smts (Chunk NestofChunks * -> Chunk))
(define (described-smts comment . chunks)
  (concat (comment-env-chunk comment)
          new-line
          (apply between/attach ";" new-line chunks)))

;make constant
(: constize (Chunk -> Chunk))
(define (constize chunk)
  (concat chunk
          (immediate space)
          (immediate 'const)))

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(: template-define (NestofChunks Chunk -> Chunk))
(define (template-define params chunk)
  (concat 'template
          (template-list params)
          new-line
          (indent 1 chunk)))

;make a use of a template
(: template-use (NestofChunks NestofChunks * -> Chunk))
(define (template-use name . args)
  (concat name (if-empty args empty (apply template-list args))))

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;general function declaration
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
