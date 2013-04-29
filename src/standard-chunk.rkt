#lang racket

(require "core-chunk.rkt")

;fulmar standard chunks

;;;;;;;;;;;;;;;;;;;;;;
;core chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(provide flatten*)
(provide literal-chunk)
(provide spaces-chunk)
(provide new-line-chunk)
(provide empty-chunk)
(provide concat-chunk)
(provide immediate-chunk)
(provide speculative-chunk)
(provide position-indent-chunk)
(provide indent-chunk)
(provide comment-env-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;character chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;if empty
; returns then, if given is null (or a list that flattens to null)
; returns else, otherwise
(define (if-empty given then else)
  (if (empty? (flatten given))
      then
      else))
(provide if-empty)

;surround/before and after chunk
; adds surround before and after chunk
(define (surround-chunk surround chunk)
  (concat-chunk surround chunk surround))
(provide surround-chunk)

;space chunk
; adds a space
(define space-chunk
  (spaces-chunk 1))
(provide space-chunk)

;immediate space chunk
; adds a space immediately
(define imm-space-chunk
  (immediate-chunk space-chunk))
(provide imm-space-chunk)

;blank lines chunk
; adds n blank lines
(define (blank-lines-chunk . lengths)
  (concat-chunk (make-list (combine-lengths 1
                                            lengths)
                           new-line-chunk)))
(provide blank-lines-chunk)

;blank line chunk
; adds a blank line
(define blank-line-chunk
  (blank-lines-chunk 1))
(provide blank-line-chunk)

;open parenthesis chunk
; adds "("
(define open-paren-chunk
  "(")
(provide open-paren-chunk)

;immediate open parenthesis chunk
; adds "(" immediately
(define imm-open-paren-chunk
  (immediate-chunk open-paren-chunk))
(provide imm-open-paren-chunk)

;close parenthesis chunk
; adds ")"
(define close-paren-chunk
  ")")
(provide close-paren-chunk)

;immediate close parenthesis chunk
; adds ")" immediately
(define imm-close-paren-chunk
  (immediate-chunk close-paren-chunk))
(provide imm-close-paren-chunk)

;surround parenthesis chunk
(define (sur-paren-chunk . chunks)
  (concat-chunk imm-open-paren-chunk
                chunks
                imm-close-paren-chunk))
(provide sur-paren-chunk)

;open curly bracket chunk
; adds "{"
(define open-crbr-chunk
  "{")
(provide open-crbr-chunk)

;immediate open curly bracket chunk
; adds "{" immediately
(define imm-open-crbr-chunk
  (immediate-chunk open-crbr-chunk))
(provide imm-open-crbr-chunk)

;close curly bracket chunk
; adds "}"
(define close-crbr-chunk
  "}")
(provide close-crbr-chunk)

;immediate close curly bracket chunk
; adds "}" immediately
(define imm-close-crbr-chunk
  (immediate-chunk close-crbr-chunk))
(provide imm-close-crbr-chunk)

;surround curly bracket chunk
(define (sur-crbr-chunk . chunks)
  (concat-chunk imm-open-crbr-chunk
                chunks
                imm-close-crbr-chunk))
(provide sur-crbr-chunk)

;open angle-bracket chunk
; adds "<"
(define open-anbr-chunk
  "<")
(provide open-anbr-chunk)

;immediate open angle-bracket chunk
; adds "<" immediately
(define imm-open-anbr-chunk
  (immediate-chunk open-anbr-chunk))
(provide imm-open-anbr-chunk)

;close angle-bracket chunk
; adds ">"
(define close-anbr-chunk
  ">")
(provide close-anbr-chunk)

;immediate close angle-bracket chunk
; adds ">" immediately
(define imm-close-anbr-chunk
  (immediate-chunk close-anbr-chunk))
(provide imm-close-anbr-chunk)

;surround angle bracket chunk
(define (sur-anbr-chunk . chunks)
  (concat-chunk imm-open-anbr-chunk
                chunks
                imm-close-anbr-chunk))
(provide sur-anbr-chunk)

;comma chunk
; adds ","
(define comma-chunk
  ",")
(provide comma-chunk)

;immediate comma chunk
; adds "," immediately
(define imm-comma-chunk
  (immediate-chunk comma-chunk))
(provide imm-comma-chunk)

;period chunk
; adds "."
(define period-chunk
  ".")
(provide period-chunk)

;immediate period chunk
; adds "," immediately
(define imm-period-chunk
  (immediate-chunk period-chunk))
(provide imm-period-chunk)

;colon chunk
; adds ":"
(define colon-chunk
  ":")
(provide colon-chunk)

;immediate colon chunk
; adds "," immediately
(define imm-colon-chunk
  (immediate-chunk colon-chunk))
(provide imm-colon-chunk)

;semi-colon chunk
; adds ";"
(define semi-colon-chunk
  ";")
(provide semi-colon-chunk)

;immediate semi-colon chunk
; adds ";" immediately
(define imm-semi-colon-chunk
  (immediate-chunk semi-colon-chunk))
(provide imm-semi-colon-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;keyword chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;define chunk
(define define-chunk
  'define)
(provide define-chunk)

;immediate define chunk
; adds "define" immediately
(define imm-define-chunk
  (immediate-chunk define-chunk))
(provide imm-define-chunk)

;include chunk
(define include-chunk
  'include)
(provide include-chunk)

;immediate include chunk
; adds "include" immediately
(define imm-include-chunk
  (immediate-chunk include-chunk))
(provide imm-include-chunk)

;ifdef chunk
(define ifdef-chunk
  'ifdef)
(provide ifdef-chunk)

;immediate ifdef chunk
; adds "ifdef" immediately
(define imm-ifdef-chunk
  (immediate-chunk ifdef-chunk))
(provide imm-ifdef-chunk)

;ifndef chunk
(define ifndef-chunk
  'ifndef)
(provide ifndef-chunk)

;immediate ifndef chunk
; adds "ifndef" immediately
(define imm-ifndef-chunk
  (immediate-chunk ifndef-chunk))
(provide imm-ifndef-chunk)

;else chunk
(define else-chunk
  'else)
(provide else-chunk)

;immediate else chunk
; adds "else" immediately
(define imm-else-chunk
  (immediate-chunk else-chunk))
(provide imm-else-chunk)

;endif chunk
(define endif-chunk
  'endif)
(provide endif-chunk)

;immediate endif chunk
; adds "endif" immediately
(define imm-endif-chunk
  (immediate-chunk endif-chunk))
(provide imm-endif-chunk)

;template chunk
(define template-chunk
  'template)
(provide template-chunk)

;immediate template chunk
; adds "template" immediately
(define imm-template-chunk
  (immediate-chunk template-chunk))
(provide imm-template-chunk)

;typename chunk
(define typename-chunk
  'typename)
(provide typename-chunk)

;immediate typename chunk
; adds "typename" immediately
(define imm-typename-chunk
  (immediate-chunk typename-chunk))
(provide imm-typename-chunk)

;typedef chunk
(define typedef-chunk
  'typedef)
(provide typedef-chunk)

;immediate typedef chunk
; adds "typedef" immediately
(define imm-typedef-chunk
  (immediate-chunk typedef-chunk))
(provide imm-typedef-chunk)

;void chunk
(define void-chunk
  'void)
(provide void-chunk)

;immediate void chunk
; adds "void" immediately
(define imm-void-chunk
  (immediate-chunk void-chunk))
(provide imm-void-chunk)

;inline chunk
(define inline-chunk
  'inline)
(provide inline-chunk)

;immediate inline chunk
; adds "inline" immediately
(define imm-inline-chunk
  (immediate-chunk inline-chunk))
(provide imm-inline-chunk)

(define static-chunk
  'static)
(provide static-chunk)

;immediate static chunk
; adds "static" immediately
(define imm-static-chunk
  (immediate-chunk static-chunk))
(provide imm-static-chunk)

;return chunk
(define return-chunk
  'return)
(provide return-chunk)

;immediate return chunk
; adds "return" immediately
(define imm-return-chunk
  (immediate-chunk return-chunk))
(provide imm-return-chunk)

;const chunk
(define const-chunk
  'const)
(provide const-chunk)

;immediate const chunk
; adds "const" immediately
(define imm-const-chunk
  (immediate-chunk const-chunk))
(provide imm-const-chunk)

;struct chunk
(define struct-chunk
  'struct)
(provide struct-chunk)

;immediate struct chunk
; adds "struct" immediately
(define imm-struct-chunk
  (immediate-chunk struct-chunk))
(provide imm-struct-chunk)

;class chunk
(define class-chunk
  'class)
(provide class-chunk)

;immediate class chunk
; adds "class" immediately
(define imm-class-chunk
  (immediate-chunk class-chunk))
(provide imm-class-chunk)

;public chunk
(define public-chunk
  'public)
(provide public-chunk)

;immediate public chunk
; adds "public" immediately
(define imm-public-chunk
  (immediate-chunk public-chunk))
(provide imm-public-chunk)

;protected chunk
(define protected-chunk
  'protected)
(provide protected-chunk)

;immediate protected chunk
; adds "protected" immediately
(define imm-protected-chunk
  (immediate-chunk protected-chunk))
(provide imm-protected-chunk)

;private chunk
(define private-chunk
  'private)
(provide private-chunk)

;immediate private chunk
; adds "private" immediately
(define imm-private-chunk
  (immediate-chunk private-chunk))
(provide imm-private-chunk)

;namespace chunk
(define namespace-chunk
  'namespace)
(provide namespace-chunk)

;immediate namespace chunk
; adds "namespace" immediately
(define imm-namespace-chunk
  (immediate-chunk namespace-chunk))
(provide imm-namespace-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;attach a chunk to other chunks
; adds to-add-chunk immediately after each of the given chunks
; except: to-add-chunk is NOT added to the final chunk
(define (attach-list-separator to-attach-chunk . chunk-lists)
  (define chunks (flatten chunk-lists))
  (if (empty? chunks) ; can't use if-empty here - take will complain
      null
      (flatten* (map (λ (chunk) (concat-chunk chunk (immediate-chunk to-attach-chunk)))
                     (take chunks (- (length chunks) 1)))
                (last chunks))))
(provide attach-list-separator)

;insert a chunk between other chunks
; concatenates given chunks with add-between-chunk between given chunks
(define (between-chunk add-between-chunk . chunks)
  (concat-chunk (add-between (flatten chunks)
                             add-between-chunk)))
(provide between-chunk)

; combine between and attach functionality
;  adds to-add-chunk after each of the given chunks
;    and then adds add-between-chunk between new chunks
(define (between/attach-chunk to-attach-chunk add-between-chunk . chunks)
  (between-chunk add-between-chunk
                 (attach-list-separator to-attach-chunk
                                        chunks)))
(provide between/attach-chunk)

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define (arg-list-chunk sur-chunk attach-chunk . chunks)
  (define (build spacing-chunk)
    (between/attach-chunk attach-chunk
                          spacing-chunk
                          chunks))
  (sur-chunk (if-empty chunks
                       empty-chunk
                       (speculative-chunk (build space-chunk)
                                          length-equals-one
                                          (position-indent-chunk (build new-line-chunk))))))
(provide arg-list-chunk)

;parenthesis argument list chunk
(define (paren-list-chunk . chunks)
  (arg-list-chunk sur-paren-chunk
                  comma-chunk
                  chunks))
(provide paren-list-chunk)

;template argument list chunk
(define (template-list-chunk . chunks)
  (arg-list-chunk sur-anbr-chunk
                  comma-chunk
                  chunks))
(provide template-list-chunk)

;body list chunk
(define (body-list-chunk attach-chunk . chunks)
  (arg-list-chunk sur-crbr-chunk
                  attach-chunk
                  chunks))
(provide body-list-chunk)

;list of chunks
; blank line added between each chunk
(define (top-list-chunk . chunks)
  (between-chunk blank-line-chunk chunks))
(provide top-list-chunk)

;list of statement chunks without final semi-colon
; adds spacing-chunk added between each chunk
;  and attaches a semi-colon to the end of each chunk (except last)
(define (internal-smt-list-chunk spacing-chunk . chunks)
  (between/attach-chunk semi-colon-chunk
                        spacing-chunk
                        chunks))
(provide internal-smt-list-chunk)

;list of statement chunks
; adds spacing-chunk added between each chunk
;  and attaches a semi-colon to the end of each chunk
(define (smt-list-chunk spacing-chunk . chunks)
  (if-empty chunks
            empty-chunk
            (concat-chunk (internal-smt-list-chunk spacing-chunk
                                                   chunks)
                          imm-semi-colon-chunk)))
(provide smt-list-chunk)

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
(define (constructor-assignment-list-chunk . chunks)
  (define (build spacing-chunk)
    (indent-chunk 2
                  (concat-chunk colon-chunk
                                imm-space-chunk
                                (position-indent-chunk (between/attach-chunk comma-chunk
                                                                             spacing-chunk
                                                                             chunks)))))
  (if-empty chunks
            empty-chunk
            (speculative-chunk (build space-chunk)
                               length-equals-one
                               (build new-line-chunk))))
(provide constructor-assignment-list-chunk)

;general body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk, if use-semi-colons is true
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (general-body-chunk use-semi-colons . chunks)
  (define (build start/end-spacing-chunk spacing-chunk)
    (surround-chunk start/end-spacing-chunk
                    (if use-semi-colons
                        (smt-list-chunk spacing-chunk chunks)
                        (between-chunk spacing-chunk chunks))))
  (sur-crbr-chunk (if-empty chunks
                            empty-chunk
                            (speculative-chunk (build space-chunk space-chunk)
                                               length-equals-one
                                               (indent-chunk 3 (build new-line-chunk blank-line-chunk))))))
(provide general-body-chunk)

;body chunk
; surrounds chunks with curly brackets
; - adds a semi-colon after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (body-chunk . chunks)
  (general-body-chunk #true chunks))
(provide body-chunk)

;class body chunk
; surrounds chunks with curly brackets
; - does NOT add semi-colons after each chunk
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (class-body-chunk . chunks)
  (general-body-chunk #false chunks))
(provide class-body-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;preprocessor chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;preprocessor define chunk
; #define chunk
(define (pp-define-chunk name)
  (concat-chunk pp-directive-chunk
                define-chunk
                space-chunk
                name))
(provide pp-define-chunk)

;preprocessor include chunk
; #include<...> chunk
(define (pp-include-chunk included)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                (template-list-chunk included)))
(provide pp-include-chunk)

;alternate preprocessor include chunk
; #include<...> chunk
(define (pp-alt-include-chunk included)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                "\""
                included
                "\""))
(provide pp-alt-include-chunk)

;multiple includes
(define (pp-includes-chunk . chunks)
  (between-chunk new-line-chunk
                 (map pp-include-chunk
                      (flatten chunks))))
(provide pp-includes-chunk)

;preprocessor if-not-defined chunk
(define (pp-ifdef-chunk condition)
  (concat-chunk pp-directive-chunk
                ifdef-chunk
                space-chunk
                condition))
(provide pp-ifdef-chunk)

;preprocessor if-not-defined chunk
(define (pp-ifndef-chunk condition)
  (concat-chunk pp-directive-chunk
                ifndef-chunk
                space-chunk
                condition))
(provide pp-ifndef-chunk)

;preprocessor if-not-defined chunk
(define pp-else-chunk
  (concat-chunk pp-directive-chunk
                else-chunk))
(provide pp-else-chunk)

;preprocessor endif chunk
(define (pp-endif-chunk condition)
  (concat-chunk pp-directive-chunk
                endif-chunk
                new-line-chunk
                (comment-env-chunk condition)))
(provide pp-endif-chunk)

;preprocessor conditional chunk
(define (pp-conditional-chunk directive condition then [else #false])
  (concat-chunk pp-directive-chunk
                directive
                space-chunk
                condition
                new-line-chunk
                (indent-chunk 3
                              then)
                new-line-chunk
                (if else
                    (concat-chunk pp-else-chunk
                                  new-line-chunk
                                  (indent-chunk 3
                                                else)
                                  new-line-chunk)
                    empty-chunk)
                (pp-endif-chunk condition)))
(provide pp-conditional-chunk)

;preprocessor conditional ifdef chunk
(define (pp-conditional-ifdef-chunk condition then [else #false])
  (pp-conditional-chunk ifdef-chunk condition then else))
(provide pp-conditional-ifdef-chunk)

;preprocessor conditional ifndef chunk
(define (pp-conditional-ifndef-chunk condition then [else #false])
  (pp-conditional-chunk ifndef-chunk condition then else))
(provide pp-conditional-ifndef-chunk)

;preprocessor h file wrapper chunk
(define (pp-header-file-chunk file-name . chunks)
  (pp-conditional-ifndef-chunk file-name
                               (concat-chunk (pp-define-chunk file-name)
                                             blank-line-chunk
                                             (top-list-chunk chunks)
                                             new-line-chunk)))
(provide pp-header-file-chunk)

;macro defintion chunk
; a macro definition
(define (macro-define-chunk name params chunk)
  (define macro-signature (concat-chunk (pp-define-chunk name)
                                        (if-empty params
                                                  empty-chunk
                                                  (paren-list-chunk params))))
  (speculative-chunk (concat-chunk macro-signature
                                   space-chunk
                                   chunk)
                     length-equals-one
                     (macro-env-chunk (concat-chunk macro-signature
                                                    new-line-chunk
                                                    (indent-chunk 3 chunk)))))
(provide macro-define-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(define (namespace-define-chunk name . chunks)
  (define chunk (concat-chunk imm-namespace-chunk
                              imm-space-chunk
                              (immediate-chunk name)
                              imm-space-chunk
                              (body-chunk chunks)))
  (speculative-chunk chunk
                     length-equals-one
                     (concat-chunk chunk
                                   space-chunk
                                   (comment-env-chunk name))))
(provide namespace-define-chunk)

;described statements chunk
(define (described-smts-chunk comment . chunks)
  (concat-chunk (comment-env-chunk comment)
                new-line-chunk
                (between/attach-chunk semi-colon-chunk
                                      new-line-chunk
                                      chunks)))
(provide described-smts-chunk)

;make constant
(define (constize-chunk chunk)
  (concat-chunk chunk
                imm-space-chunk
                imm-const-chunk))
(provide constize-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define (template-define-chunk params chunk)
  (concat-chunk template-chunk
                (template-list-chunk params)
                new-line-chunk
                (indent-chunk 1 chunk)))
(provide template-define-chunk)

;make a use of a template
(define (template-use-chunk name . args)
  (concat-chunk name (if-empty args empty-chunk (template-list-chunk args))))
(provide template-use-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;general function declaration
(define (general-function-declare-chunk name return-type . params)
  (concat-chunk return-type
                space-chunk
                name
                (paren-list-chunk (if-empty params
                                            imm-void-chunk
                                            params))))
(provide general-function-declare-chunk)

;function declaration
(define (function-declare-chunk name return-type . params)
  (concat-chunk imm-inline-chunk
                space-chunk
                (general-function-declare-chunk name
                                                return-type
                                                params)))
(provide function-declare-chunk)

;static function declaration
(define (static-function-declare-chunk name return-type . params)
  (concat-chunk imm-static-chunk
                imm-space-chunk
                (function-declare-chunk name
                                        return-type
                                        params)))
(provide static-function-declare-chunk)

;void function declaration
(define (void-function-declare-chunk name params)
  (function-declare-chunk name void-chunk params))
(provide void-function-declare-chunk)

;function defintion
(define (function-define-chunk signature . body)
  (concat-chunk signature
                imm-space-chunk
                (body-chunk body)))
(provide function-define-chunk)

;void function defintion
(define (void-function-define-chunk name params . body)
  (function-define-chunk (void-function-declare-chunk name
                                                      params)
                         body))
(provide void-function-define-chunk)

;returning function defintion
(define (returning-function-define-chunk signature body return-expr)
  (function-define-chunk signature
                         (flatten* body
                                   (concat-chunk return-chunk
                                                 imm-space-chunk
                                                 (position-indent-chunk return-expr)))))
(provide returning-function-define-chunk)

;constructor assignment
(define (constructor-assignment-chunk var val)
  (concat-chunk var
                (sur-paren-chunk (concat-chunk val))))
(provide constructor-assignment-chunk)

;constructor defintion
(define (constructor-chunk name params assigns . body)
  (concat-chunk name
                (paren-list-chunk params)
                (if-empty assigns
                          imm-space-chunk
                          (surround-chunk new-line-chunk
                                          (constructor-assignment-list-chunk assigns)))
                (body-chunk body)))
(provide constructor-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define (struct-declare-chunk name)
  (concat-chunk struct-chunk
                space-chunk
                name))
(provide struct-declare-chunk)

;template struct declaration
(define (template-struct-declare-chunk name params . args)
  (template-define-chunk params
                         (template-use-chunk (struct-declare-chunk name)
                                             args)))
(provide template-struct-declare-chunk)

;struct section
(define (section-define-chunk type . chunks)
  (if-empty chunks
            empty-chunk
            (concat-chunk type
                          colon-chunk
                          new-line-chunk
                          (indent-chunk 1 (between-chunk blank-line-chunk chunks)))))
(provide section-define-chunk)

;public section
(define (public-section-chunk . chunks)
  (section-define-chunk public-chunk chunks))
(provide public-section-chunk)

;private section
(define (private-section-chunk . chunks)
  (section-define-chunk private-chunk chunks))
(provide private-section-chunk)

;protected section
(define (protected-section-chunk . chunks)
  (section-define-chunk protected-chunk chunks))
(provide protected-section-chunk)

;struct definition
(define (struct-define-chunk signature . body)
  (concat-chunk signature
                imm-space-chunk
                (class-body-chunk body)))
(provide struct-define-chunk)

;template struct definition
(define (template-struct-define-chunk name params args . body)
  (struct-define-chunk (template-struct-declare-chunk name params args)
                       body))
(provide template-struct-define-chunk)

;scope resolution operator
(define (scope-resolution-operator-chunk scope variable)
  (concat-chunk scope
                imm-colon-chunk
                imm-colon-chunk
                variable))
(provide scope-resolution-operator-chunk)

;;;;;;;;;;;;;;;;;;;;;;
;statement chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;typedef statement chunk
(define (typedef-smt-chunk lhs rhs)
  (concat-chunk lhs
                space-chunk
                typedef-chunk
                space-chunk
                rhs))
(provide typedef-smt-chunk)

;function call
(define (function-call-chunk fcn . args)
  (concat-chunk fcn
                (paren-list-chunk args)))
(provide function-call-chunk)

;member function call
(define (member-function-call-chunk obj fcn . args)
  (concat-chunk obj
                imm-period-chunk
                (position-indent-chunk (function-call-chunk fcn args))))
(provide member-function-call-chunk)
