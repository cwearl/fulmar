#lang racket

(require "basics.rkt")

;chunk structure, procedures, and contracts

;structures

;chunk structure
(struct chunk-struct (name body) #:transparent)

;predicates

;predicate for literal chunk value
(define/contract (chunk-literal? g)
  pred/c
  (list-of? (or? string?
                 symbol?
                 char?
                 indent?)
            1
            g))
(provide chunk-literal?)

;predicate for body of indent chunk body
(define/contract (indent-chunk-body? g)
  pred/c
  (and (list? g)
       (= 2 (length g))
       (indent? (first g))
       (chunk? (second g))))
(provide indent-chunk-body?)

;predicate for non-literal chunk internals
(define/contract (chunk-nl-internal? n b)
  (-> any/c any/c boolean?)
  (if (and (chunk-name? n)
           (chunk-body? b))
      (match n
        ['literal         (chunk-literal? b)]
        ['new-line        (not b)]
        ['pp-directive    (not b)]
        ['concat          (chunk-list? b)]
        ['no-line         (chunk-list? b)]
        ['indent          (indent-chunk-body? b)]
        ['comment         (chunk? b)]
        ['macro           (chunk? b)]
        [_                (error "Given unrecognized chunk name: " n)])
      #false))
(provide chunk-nl-internal?)

;predicate for non-literal chunk value
(define/contract (chunk-nl? g)
  pred/c
  (and (chunk-struct? g)
       (chunk-nl-internal? (chunk-struct-name g)
                           (chunk-struct-body g))))
(provide chunk-nl?)

;TODO: change chunk-name? to only accept valid chunk types
;predicate for chunk name
(define/contract (chunk-name? g)
  pred/c
  (match g
    [(or 'literal
         'new-line
         'pp-directive
         'concat
         'no-line
         'indent
         'comment
         'macro)          #true]
    [_                    #false]))
(provide chunk-name?)

;predicate for non-literal chunk body
(define/contract (chunk-body? g)
  pred/c
  ((or? chunk-literal?
        not
        chunk-list?
        chunk?
        indent-chunk-body?) g))
(provide chunk-body?)

;predicate for general chunk
(define/contract (chunk? g)
  pred/c
  ((or? chunk-nl?
        chunk-literal?) g))
(provide chunk?)

;predicate for list of chunks
(define/contract (chunk-list? g)
  pred/c
  (list-of? chunk? 1 g))
(provide chunk-list?)

;TODO: Determine if nullable-chunk-list? is needed
;predicate for list of chunks (possibly empty)
(define (nullable-chunk-list? g)
  pred/c
  (list-of? chunk? 0 g))
(provide nullable-chunk-list?)

;constructors

;non-literal chunk
(define/contract (nl-chunk n b)
  (-> chunk-name? chunk-body? chunk?)
  (if (chunk-nl-internal? n b)
      (chunk-struct n b)
      (error "Given chunk name and body do not match; given: " n b)))
;(provide nl-chunk)

;TODO: Pick naming convention: literal-chunk or chunk-literal
;(explicit) literal chunk
(define/contract (literal-chunk . g)
  (->* () #:rest chunk-literal? chunk?)
  (flatten* g))
(provide literal-chunk)

;(explicit) spaces chunk
(define/contract (spaces-chunk . g)
  (->* () #:rest chunk-literal? chunk?)
  (foldl (λ (item total)
           (+ total
              (cond [(string? item) (string-length item)]
                    [(symbol? item) (string-length (symbol->string item))]
                    [(char? item)   1]
                    [(indent? item) item]
                    [(list? item)   (apply spaces-chunk item)]
                    [else (error "Unknown literal chunk type; given: " item)])))
         0
         (flatten* g)))
(provide spaces-chunk)

;new line chunk
(define/contract new-line-chunk
  chunk?
  (chunk-struct 'new-line #false))
(provide new-line-chunk)

;preprocessor directive chunk
(define/contract pp-directive-chunk
  chunk?
  (chunk-struct 'pp-directive #false))
(provide pp-directive-chunk)

;concatenation chunk
(define/contract (concat-chunk . chunks)
  (->* () #:rest chunk-list? chunk?)
  (chunk-struct 'concat (flatten* chunks)))
(provide concat-chunk)

;no new line concatenation chunk
(define/contract (no-line-chunk . chunks)
  (->* () #:rest chunk-list? chunk?)
  (chunk-struct 'no-line (flatten* chunks)))
(provide no-line-chunk)

;indent chunk
(define/contract (indent-chunk indent chunk)
  (-> indent? chunk? chunk?)
  (chunk-struct 'indent (list indent chunk)))
(provide indent-chunk)

;comment chunk
(define/contract (comment-chunk chunk)
  (-> chunk? chunk?)
  (chunk-struct 'comment chunk))
(provide comment-chunk)

;macro chunk
(define/contract (macro-chunk chunk)
  (-> chunk? chunk?)
  (chunk-struct 'macro chunk))
(provide macro-chunk)

;accessors

;chunk name accessor
(define/contract (chunk-name g)
  (-> chunk? chunk-name?)
  (cond [(chunk-literal? g) 'literal]
        [(chunk-nl? g) (chunk-struct-name g)]
        [else (error "Unrecognized chunk: " g)]))
(provide chunk-name)

;chunk body accessor
(define/contract (chunk-body g)
  (-> chunk? chunk-body?)
  (cond [(chunk-literal? g) g]
        [(chunk-nl? g) (chunk-struct-body g)]
        [else (error "Unrecognized chunk: " g)]))
(provide chunk-body)
