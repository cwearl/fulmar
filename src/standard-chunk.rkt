#lang racket

(require "fulmar-core.rkt")
(require "core-chunk.rkt")

;fulmar standard chunks

;;;;;;;;;;;;;;;;;;;;;;
;core chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

(provide (contract-out (literal-chunk literal-chunk/c)
                       (spaces-chunk spaces-chunk/c)
                       (new-line-chunk chunk/c)
                       (empty-chunk chunk/c)
                       (concat-chunk concat-chunk/c)
                       (immediate-chunk immediate-chunk/c)
                       (speculative-chunk speculative-chunk/c)
                       (position-indent-chunk position-indent-chunk/c)
                       (indent-chunk indent-chunk/c)
                       (comment-env-chunk comment-env-chunk/c)))

;;;;;;;;;;;;;;;;;;;;;;
;character chunks;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;space chunk
; adds a space
(define space-chunk
  (spaces-chunk 1))
(provide (contract-out (space-chunk chunk/c)))

;immediate space chunk
; adds a space immediately
(define imm-space-chunk
  (immediate-chunk space-chunk))
(provide (contract-out (imm-space-chunk chunk/c)))

;blank lines chunk
; adds n blank lines
(define (blank-lines-chunk . lengths)
  (concat-chunk (make-list (combine-lengths 1
                                            lengths)
                           new-line-chunk)))
(provide (contract-out (blank-lines-chunk (->* () #:rest (non-empty-listof exact-positive-integer?) chunk/c))))

;blank line chunk
; adds a blank line
(define blank-line-chunk
  (blank-lines-chunk 1))
(provide (contract-out (blank-line-chunk chunk/c)))

;open parenthesis chunk
; adds "("
(define open-paren-chunk
  "(")
(provide (contract-out (open-paren-chunk chunk/c)))

;immediate open parenthesis chunk
; adds "(" immediately
(define imm-open-paren-chunk
  (immediate-chunk open-paren-chunk))
(provide (contract-out (imm-open-paren-chunk chunk/c)))

;close parenthesis chunk
; adds ")"
(define close-paren-chunk
  ")")
(provide (contract-out (close-paren-chunk chunk/c)))

;immediate close parenthesis chunk
; adds ")" immediately
(define imm-close-paren-chunk
  (immediate-chunk close-paren-chunk))
(provide (contract-out (imm-close-paren-chunk chunk/c)))

;open curly bracket chunk
; adds "{"
(define open-crbr-chunk
  "{")
(provide (contract-out (open-crbr-chunk chunk/c)))

;immediate open curly bracket chunk
; adds "{" immediately
(define imm-open-crbr-chunk
  (immediate-chunk open-crbr-chunk))
(provide (contract-out (imm-open-crbr-chunk chunk/c)))

;close curly bracket chunk
; adds "}"
(define close-crbr-chunk
  "}")
(provide (contract-out (close-crbr-chunk chunk/c)))

;immediate close curly bracket chunk
; adds "}" immediately
(define imm-close-crbr-chunk
  (immediate-chunk close-crbr-chunk))
(provide (contract-out (imm-close-crbr-chunk chunk/c)))

;open angle-bracket chunk
; adds "<"
(define open-anbr-chunk
  "<")
(provide (contract-out (open-anbr-chunk chunk/c)))

;immediate open angle-bracket chunk
; adds "<" immediately
(define imm-open-anbr-chunk
  (immediate-chunk open-anbr-chunk))
(provide (contract-out (imm-open-anbr-chunk chunk/c)))

;close angle-bracket chunk
; adds ">"
(define close-anbr-chunk
  ">")
(provide (contract-out (close-anbr-chunk chunk/c)))

;immediate close angle-bracket chunk
; adds ">" immediately
(define imm-close-anbr-chunk
  (immediate-chunk close-anbr-chunk))
(provide (contract-out (imm-close-anbr-chunk chunk/c)))

;comma chunk
; adds ","
(define comma-chunk
  ",")
(provide (contract-out (comma-chunk chunk/c)))

;immediate comma chunk
; adds "," immediately
(define imm-comma-chunk
  (immediate-chunk comma-chunk))
(provide (contract-out (imm-comma-chunk chunk/c)))

;period chunk
; adds "."
(define period-chunk
  ".")
(provide (contract-out (period-chunk chunk/c)))

;immediate period chunk
; adds "," immediately
(define imm-period-chunk
  (immediate-chunk period-chunk))
(provide (contract-out (imm-period-chunk chunk/c)))

;colon chunk
; adds ":"
(define colon-chunk
  ":")
(provide (contract-out (colon-chunk chunk/c)))

;immediate colon chunk
; adds "," immediately
(define imm-colon-chunk
  (immediate-chunk colon-chunk))
(provide (contract-out (imm-colon-chunk chunk/c)))

;semi-colon chunk
; adds ";"
(define semi-colon-chunk
  ";")
(provide (contract-out (semi-colon-chunk chunk/c)))

;immediate semi-colon chunk
; adds ";" immediately
(define imm-semi-colon-chunk
  (immediate-chunk semi-colon-chunk))
(provide (contract-out (imm-semi-colon-chunk chunk/c)))

;;;;;;;;;;;;;;;;;;;;;;
;keyword chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;define chunk
(define define-chunk
  'define)
(provide (contract-out (define-chunk chunk/c)))

;immediate define chunk
; adds "define" immediately
(define imm-define-chunk
  (immediate-chunk define-chunk))
(provide (contract-out (imm-define-chunk chunk/c)))

;include chunk
(define include-chunk
  'include)
(provide (contract-out (include-chunk chunk/c)))

;immediate include chunk
; adds "include" immediately
(define imm-include-chunk
  (immediate-chunk include-chunk))
(provide (contract-out (imm-include-chunk chunk/c)))

;ifdef chunk
(define ifdef-chunk
  'ifdef)
(provide (contract-out (ifdef-chunk chunk/c)))

;immediate ifdef chunk
; adds "ifdef" immediately
(define imm-ifdef-chunk
  (immediate-chunk ifdef-chunk))
(provide (contract-out (imm-ifdef-chunk chunk/c)))

;ifndef chunk
(define ifndef-chunk
  'ifndef)
(provide (contract-out (ifndef-chunk chunk/c)))

;immediate ifndef chunk
; adds "ifndef" immediately
(define imm-ifndef-chunk
  (immediate-chunk ifndef-chunk))
(provide (contract-out (imm-ifndef-chunk chunk/c)))

;else chunk
(define else-chunk
  'else)
(provide (contract-out (else-chunk chunk/c)))

;immediate else chunk
; adds "else" immediately
(define imm-else-chunk
  (immediate-chunk else-chunk))
(provide (contract-out (imm-else-chunk chunk/c)))

;endif chunk
(define endif-chunk
  'endif)
(provide (contract-out (endif-chunk chunk/c)))

;immediate endif chunk
; adds "endif" immediately
(define imm-endif-chunk
  (immediate-chunk endif-chunk))
(provide (contract-out (imm-endif-chunk chunk/c)))

;template chunk
(define template-chunk
  'template)
(provide (contract-out (template-chunk chunk/c)))

;immediate template chunk
; adds "template" immediately
(define imm-template-chunk
  (immediate-chunk template-chunk))
(provide (contract-out (imm-template-chunk chunk/c)))

;typename chunk
(define typename-chunk
  'typename)
(provide (contract-out (typename-chunk chunk/c)))

;immediate typename chunk
; adds "typename" immediately
(define imm-typename-chunk
  (immediate-chunk typename-chunk))
(provide (contract-out (imm-typename-chunk chunk/c)))

;typedef chunk
(define typedef-chunk
  'typedef)
(provide (contract-out (typedef-chunk chunk/c)))

;immediate typedef chunk
; adds "typedef" immediately
(define imm-typedef-chunk
  (immediate-chunk typedef-chunk))
(provide (contract-out (imm-typedef-chunk chunk/c)))

;void chunk
(define void-chunk
  'void)
(provide (contract-out (void-chunk chunk/c)))

;immediate void chunk
; adds "void" immediately
(define imm-void-chunk
  (immediate-chunk void-chunk))
(provide (contract-out (imm-void-chunk chunk/c)))

;inline chunk
(define inline-chunk
  'inline)
(provide (contract-out (inline-chunk chunk/c)))

;immediate inline chunk
; adds "inline" immediately
(define imm-inline-chunk
  (immediate-chunk inline-chunk))
(provide (contract-out (imm-inline-chunk chunk/c)))

(define static-chunk
  'static)
(provide (contract-out (static-chunk chunk/c)))

;immediate static chunk
; adds "static" immediately
(define imm-static-chunk
  (immediate-chunk static-chunk))
(provide (contract-out (imm-static-chunk chunk/c)))

;return chunk
(define return-chunk
  'return)
(provide (contract-out (return-chunk chunk/c)))

;immediate return chunk
; adds "return" immediately
(define imm-return-chunk
  (immediate-chunk return-chunk))
(provide (contract-out (imm-return-chunk chunk/c)))

;const chunk
(define const-chunk
  'const)
(provide (contract-out (const-chunk chunk/c)))

;immediate const chunk
; adds "const" immediately
(define imm-const-chunk
  (immediate-chunk const-chunk))
(provide (contract-out (imm-const-chunk chunk/c)))

;struct chunk
(define struct-chunk
  'struct)
(provide (contract-out (struct-chunk chunk/c)))

;immediate struct chunk
; adds "struct" immediately
(define imm-struct-chunk
  (immediate-chunk struct-chunk))
(provide (contract-out (imm-struct-chunk chunk/c)))

;class chunk
(define class-chunk
  'class)
(provide (contract-out (class-chunk chunk/c)))

;immediate class chunk
; adds "class" immediately
(define imm-class-chunk
  (immediate-chunk class-chunk))
(provide (contract-out (imm-class-chunk chunk/c)))

;public chunk
(define public-chunk
  'public)
(provide (contract-out (public-chunk chunk/c)))

;immediate public chunk
; adds "public" immediately
(define imm-public-chunk
  (immediate-chunk public-chunk))
(provide (contract-out (imm-public-chunk chunk/c)))

;protected chunk
(define protected-chunk
  'protected)
(provide (contract-out (protected-chunk chunk/c)))

;immediate protected chunk
; adds "protected" immediately
(define imm-protected-chunk
  (immediate-chunk protected-chunk))
(provide (contract-out (imm-protected-chunk chunk/c)))

;private chunk
(define private-chunk
  'private)
(provide (contract-out (private-chunk chunk/c)))

;immediate private chunk
; adds "private" immediately
(define imm-private-chunk
  (immediate-chunk private-chunk))
(provide (contract-out (imm-private-chunk chunk/c)))

;namespace chunk
(define namespace-chunk
  'namespace)
(provide (contract-out (namespace-chunk chunk/c)))

;immediate namespace chunk
; adds "namespace" immediately
(define imm-namespace-chunk
  (immediate-chunk namespace-chunk))
(provide (contract-out (imm-namespace-chunk chunk/c)))

;;;;;;;;;;;;;;;;;;;;;;
;list chunks;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;attach a chunk to other chunks
; adds to-add-chunk immediately after each of the given chunks
; except: to-add-chunk is NOT added to the final chunk
(define (attach-list-separator to-attach-chunk . chunk-lists)
  (let ([chunks (flatten chunk-lists)])
    (if (empty? chunks)
        null
        (flatten* (map (λ (chunk) (concat-chunk chunk (immediate-chunk to-attach-chunk)))
                       (take chunks (- (length chunks) 1)))
                  (last chunks)))))
(provide (contract-out (attach-list-separator (->* (chunk/c) #:rest nullable-chunk-list/c nullable-chunk-list/c))))

;insert a chunk between other chunks
; concatenates given chunks with add-between-chunk between given chunks
(define (between-chunk add-between-chunk . chunk-lists)
  (let ([chunks (flatten chunk-lists)])
    (if (empty? chunks)
        empty-chunk
        (concat-chunk (add-between chunks
                                   add-between-chunk)))))
(provide (contract-out (between-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

; combine between and attach functionality
;  adds to-add-chunk after each of the given chunks
;    and then adds add-between-chunk between new chunks
(define (between/attach-chunk to-attach-chunk add-between-chunk . chunks)
  (between-chunk add-between-chunk
                 (attach-list-separator to-attach-chunk
                                        chunks)))
(provide (contract-out (between/attach-chunk (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;argument list chunk
; attempts to put chunks on a single line with a space between each chunk
; if that fails, puts chunks on their own lines
(define (arg-list-chunk open-chunk attach-chunk close-chunk . chunk-lists)
  (let ([chunks (flatten chunk-lists)])
    (concat-chunk (immediate-chunk open-chunk)
                  (cond [;no parameters
                         (empty? chunks)
                         empty-chunk]
                        [;one parameter - no commas
                         (= 1 (length chunks))
                         (position-indent-chunk (first chunks))]
                        [;more than one parameter - commas and possibly more than one line
                         else
                         (speculative-chunk (between/attach-chunk attach-chunk
                                                                  space-chunk
                                                                  chunks)
                                            length-equals-one
                                            (position-indent-chunk (between/attach-chunk attach-chunk
                                                                                         new-line-chunk
                                                                                         chunks)))])
                (immediate-chunk close-chunk))))
(provide (contract-out (arg-list-chunk (->* (chunk/c chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;parenthesis argument list chunk
(define (paren-list-chunk . chunks)
  (arg-list-chunk open-paren-chunk
                  comma-chunk
                  close-paren-chunk
                  chunks))
(provide (contract-out (paren-list-chunk (->* () #:rest nullable-chunk-list/c chunk/c))))

;template argument list chunk
(define (template-list-chunk . chunks)
  (arg-list-chunk open-anbr-chunk
                  comma-chunk
                  close-anbr-chunk
                  chunks))
(provide (contract-out (template-list-chunk (->* () #:rest nullable-chunk-list/c chunk/c))))

;statement line list of chunks
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define (smt-list-chunk spacing-chunk . chunks)
  (between/attach-chunk semi-colon-chunk
                        spacing-chunk
                        chunks))
(provide (contract-out (smt-list-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;statement line list of chunks with last semi-colon
; each chunk is expanded on its own line
; - each chunk put on it's own line
(define (top-smt-list-chunk spacing-chunk . chunks)
  (if (empty? (flatten chunks))
      empty-chunk
      (concat-chunk (smt-list-chunk spacing-chunk
                                    chunks)
                    imm-semi-colon-chunk)))
(provide (contract-out (top-smt-list-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;constructor assignment list chunk
; each assignment is separated by a comma
; - first line is indented 2 spaces and begun with a colon
(define (constructor-assignment-list-chunk . assignment-lists)
  (let* ([assigns (flatten assignment-lists)]
         [build (λ (spacing-chunk)
                  (indent-chunk 2
                                (concat-chunk colon-chunk
                                              imm-space-chunk
                                              (position-indent-chunk (between/attach-chunk comma-chunk
                                                                                           spacing-chunk
                                                                                           assigns)))))])
    (if (empty? assigns)
        empty-chunk
        (speculative-chunk (build space-chunk)
                           length-equals-one
                           (build new-line-chunk)))))
(provide (contract-out (constructor-assignment-list-chunk (->* () #:rest nullable-chunk-list/c chunk/c))))

;body chunk
; surrounds chunks with curly brackets
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines with indented
;   open curly bracket is immediately on current line
;   close curly bracket is on it's own line
(define (body-chunk . chunks)
  (let ([build (λ (spacing-chunk)
                 (top-smt-list-chunk spacing-chunk
                                     chunks))])
    (concat-chunk imm-open-crbr-chunk ; open body
                  (if (empty? (flatten chunks)) ; if nothing in body
                      empty-chunk ; have no body
                      (speculative-chunk (concat-chunk space-chunk          ; if body can fit on a single line, put it there
                                                       (build space-chunk)
                                                       space-chunk)
                                         length-equals-one
                                         (indent-chunk 3                    ; if not, increase indent
                                                       (concat-chunk new-line-chunk ; and start a new line
                                                                     (speculative-chunk (build blank-line-chunk)  ; if inner body fits on a single line, put it there
                                                                                        length-equals-one
                                                                                        (concat-chunk new-line-chunk ; if not, put a blank line between start of body and start of inner body
                                                                                                      (build blank-line-chunk)))
                                                                     new-line-chunk)))) ; start a new line after inner body has been printed
                  imm-close-crbr-chunk))) ; close body
(provide (contract-out (body-chunk (->* () #:rest nullable-chunk-list/c chunk/c))))

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
(provide (contract-out (pp-define-chunk (-> chunk/c chunk/c))))

;preprocessor include chunk
; #include<...> chunk
(define (pp-include-chunk included)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                (template-list-chunk included)))
(provide (contract-out (pp-include-chunk (-> chunk/c chunk/c))))

;alternate preprocessor include chunk
; #include<...> chunk
(define (pp-alt-include-chunk included)
  (concat-chunk pp-directive-chunk
                include-chunk
                space-chunk
                "\""
                included
                "\""))
(provide (contract-out (pp-alt-include-chunk (-> chunk/c chunk/c))))

;multiple includes
(define (pp-includes-chunk . chunks)
  (between-chunk new-line-chunk
                 (map pp-include-chunk
                      (flatten chunks))))
(provide (contract-out (pp-includes-chunk (->* () #:rest chunk-list/c chunk/c))))

;preprocessor if-not-defined chunk
(define (pp-ifdef-chunk condition)
  (concat-chunk pp-directive-chunk
                ifdef-chunk
                space-chunk
                condition))
(provide (contract-out (pp-ifdef-chunk (-> chunk/c chunk/c))))

;preprocessor if-not-defined chunk
(define (pp-ifndef-chunk condition)
  (concat-chunk pp-directive-chunk
                ifndef-chunk
                space-chunk
                condition))
(provide (contract-out (pp-ifndef-chunk (-> chunk/c chunk/c))))

;preprocessor if-not-defined chunk
(define pp-else-chunk
  (concat-chunk pp-directive-chunk
                else-chunk))
(provide (contract-out (pp-else-chunk chunk/c)))

;preprocessor endif chunk
(define (pp-endif-chunk condition)
  (concat-chunk pp-directive-chunk
                endif-chunk
                new-line-chunk
                (comment-env-chunk condition)))
(provide (contract-out (pp-endif-chunk (-> chunk/c chunk/c))))

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
(provide (contract-out (pp-conditional-chunk (->* (chunk/c chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c))))

;preprocessor conditional ifdef chunk
(define (pp-conditional-ifdef-chunk condition then [else #false])
  (pp-conditional-chunk ifdef-chunk condition then else))
(provide (contract-out (pp-conditional-ifdef-chunk (->* (chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c))))

;preprocessor conditional ifndef chunk
(define (pp-conditional-ifndef-chunk condition then [else #false])
  (pp-conditional-chunk ifndef-chunk condition then else))
(provide (contract-out (pp-conditional-ifndef-chunk (->* (chunk/c chunk/c) ((or/c chunk/c #false)) chunk/c))))

;preprocessor h file wrapper chunk
(define (pp-header-file-chunk file-name file-setup . chunks)
  (pp-conditional-ifndef-chunk file-name
                               (concat-chunk (pp-define-chunk file-name)
                                             blank-line-chunk
                                             file-setup
                                             blank-line-chunk
                                             (top-smt-list-chunk blank-line-chunk
                                                                 chunks)
                                             new-line-chunk)))
(provide (contract-out (pp-header-file-chunk (->* (chunk/c chunk/c) #:rest chunk-list/c chunk/c))))

;macro defintion chunk
; a macro definition
(define (macro-define-chunk name params chunk)
  (let ([macro-signature (concat-chunk (pp-define-chunk name)
                                       (if (empty? (flatten params))
                                           empty-chunk
                                           (paren-list-chunk params)))])
    (speculative-chunk (concat-chunk macro-signature
                                     space-chunk
                                     chunk)
                       length-equals-one
                       (macro-env-chunk (concat-chunk macro-signature
                                                      new-line-chunk
                                                      (indent-chunk 3 chunk))))))
(provide (contract-out (macro-define-chunk (-> chunk/c nullable-chunk-list/c chunk/c chunk/c))))

;;;;;;;;;;;;;;;;;;;;;;
;general chunks;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;namespace define chunk
(define (namespace-define-chunk name . chunks)
  (let ([chunk (concat-chunk imm-namespace-chunk
                             imm-space-chunk
                             (immediate-chunk name)
                             imm-space-chunk
                             (body-chunk chunks))])
    (speculative-chunk chunk
                       length-equals-one
                       (concat-chunk chunk
                                     space-chunk
                                     (comment-env-chunk name)))))
(provide (contract-out (namespace-define-chunk (->* (chunk/c) #:rest chunk-list/c chunk/c))))

;described statements chunk
(define (described-smts-chunk comment . chunks)
  (concat-chunk (comment-env-chunk comment)
                new-line-chunk
                (between/attach-chunk semi-colon-chunk
                                      new-line-chunk
                                      chunks)))
(provide (contract-out (described-smts-chunk (->* (chunk/c) #:rest chunk-list/c chunk/c))))

;make constant
(define (constize-chunk chunk)
  (concat-chunk chunk
                imm-space-chunk
                imm-const-chunk))
(provide (contract-out (constize-chunk (-> chunk/c chunk/c))))

;;;;;;;;;;;;;;;;;;;;;;
;template chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;make given chunk a template with given template parameters
(define (template-define-chunk params chunk)
  (concat-chunk template-chunk
                (template-list-chunk params)
                new-line-chunk
                (indent-chunk 1
                              chunk)))
(provide (contract-out (template-define-chunk (-> nullable-chunk-list/c chunk/c chunk/c))))

;make a use of a template
(define (template-use-chunk name . args)
  (concat-chunk name
                (if (empty? (flatten args))
                    empty-chunk
                    (template-list-chunk args))))
(provide (contract-out (template-use-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;;;;;;;;;;;;;;;;;;;;;;
;function chunks;;;;;;
;;;;;;;;;;;;;;;;;;;;;;

;function declaration
(define (function-declare-chunk name return-type . params)
  (concat-chunk imm-inline-chunk
                space-chunk
                return-type
                space-chunk
                name
                (if (empty? (flatten params))
                    (concat-chunk imm-open-paren-chunk
                                  imm-void-chunk
                                  imm-close-paren-chunk)
                    (paren-list-chunk params))))
(provide (contract-out (function-declare-chunk (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;static function declaration
(define (static-function-declare-chunk name return-type . params)
  (concat-chunk imm-static-chunk
                imm-space-chunk
                (function-declare-chunk name
                                        return-type
                                        params)))
(provide (contract-out (static-function-declare-chunk (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;void function declaration
(define (void-function-declare-chunk name params)
  (function-declare-chunk name void-chunk params))
(provide (contract-out (void-function-declare-chunk (-> chunk/c nullable-chunk-list/c chunk/c))))

;function defintion
(define (function-define-chunk signature . body)
  (concat-chunk signature
                imm-space-chunk
                (if (empty? (flatten body))
                    (body-chunk)
                    (body-chunk body))))
(provide (contract-out (function-define-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;void function defintion
(define (void-function-define-chunk name params . body)
  (function-define-chunk (void-function-declare-chunk name
                                                      params)
                         body))
(provide (contract-out (void-function-define-chunk (->* (chunk/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c))))

;returning function defintion
(define (returning-function-define-chunk signature body return-expr)
  (function-define-chunk signature
                         (flatten* body
                                   (concat-chunk return-chunk
                                                 imm-space-chunk
                                                 (position-indent-chunk return-expr)))))
(provide (contract-out (returning-function-define-chunk (-> chunk/c nullable-chunk-list/c chunk/c chunk/c))))

;constructor assignment
(define (constructor-assignment-chunk var val)
  (concat-chunk var
                imm-open-paren-chunk
                val
                imm-close-paren-chunk))
(provide (contract-out (constructor-assignment-chunk (-> chunk/c chunk/c chunk/c))))

;constructor defintion
(define (constructor-chunk name params assigns . body)
  (concat-chunk name
                (paren-list-chunk params)
                (if (empty? (flatten assigns))
                    (concat-chunk imm-space-chunk
                                  (body-chunk body))
                    (concat-chunk new-line-chunk
                                  (constructor-assignment-list-chunk assigns)
                                  new-line-chunk
                                  (body-chunk body)))))
(provide (contract-out (constructor-chunk (->* (chunk/c nullable-chunk-list/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c))))

;;;;;;;;;;;;;;;;;;;;;;
;class/struct chunks;;
;;;;;;;;;;;;;;;;;;;;;;

;struct declaration
(define (struct-declare-chunk name)
  (concat-chunk struct-chunk
                space-chunk
                name))
(provide (contract-out (struct-declare-chunk (-> chunk/c chunk/c))))

;template struct declaration
(define (template-struct-declare-chunk name params . args)
  (template-define-chunk params
                         (template-use-chunk (struct-declare-chunk name)
                                             args)))
(provide (contract-out (template-struct-declare-chunk (->* (chunk/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c))))

;struct section
(define (section-define-chunk type . chunks)
  (concat-chunk type
                colon-chunk
                new-line-chunk
                (indent-chunk 1 (smt-list-chunk new-line-chunk chunks))))
(provide (contract-out (section-define-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;struct definition
(define (struct-define-chunk signature . body)
  (concat-chunk signature
                imm-space-chunk
                (body-chunk body)))
(provide (contract-out (struct-define-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;template struct definition
(define (template-struct-define-chunk name params args . body)
  (struct-define-chunk (template-struct-declare-chunk name params args)
                       body))
(provide (contract-out (template-struct-define-chunk (->* (chunk/c nullable-chunk-list/c nullable-chunk-list/c) #:rest nullable-chunk-list/c chunk/c))))

;scope resolution operator
(define (scope-resolution-operator-chunk scope variable)
  (concat-chunk scope
                imm-colon-chunk
                imm-colon-chunk
                variable))
(provide (contract-out (scope-resolution-operator-chunk (-> chunk/c chunk/c chunk/c))))

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
(provide (contract-out (typedef-smt-chunk (-> chunk/c chunk/c chunk/c))))

;function call
(define (function-call-chunk fcn . args)
  (concat-chunk fcn
                (paren-list-chunk args)))
(provide (contract-out (function-call-chunk (->* (chunk/c) #:rest nullable-chunk-list/c chunk/c))))

;member function call
(define (member-function-call-chunk obj fcn . args)
  (concat-chunk obj
                imm-period-chunk
                (position-indent-chunk (function-call-chunk fcn args))))
(provide (contract-out (member-function-call-chunk (->* (chunk/c chunk/c) #:rest nullable-chunk-list/c chunk/c))))
