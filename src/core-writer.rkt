#lang racket

(require "fulmar-core.rkt")

;basic structures, functions, and contracts for fulmar writer

;structures

;logical pivot structure
(struct pivot-struct (IR full-length last-length) #:transparent)

;logical line structure
(struct logical-line-struct (IR) #:transparent)

;logical block structure
(struct logical-block-struct (IR) #:transparent)

;predicates

;predicate for actual printable item
(define/contract (actual-print-item? g)
  (-> any/c boolean?)
  (or (char? g)
      (indent? g)))
(provide actual-print-item?)

;predicate for logical print item
(define/contract (logical-print-item? g)
  (-> any/c boolean?)
  (or (actual-print-item? g)
      (pivot? g)))
(provide logical-print-item?)

;predicate for pivot internal representation
(define/contract (pivot-IR? g)
  (-> any/c boolean?)
  (and (list? g)
       (<= 2 (length g))
       (andmap logical-line? g)))
(provide pivot-IR?)

;predicate for building pivot
(define/contract (pivot-basis? g)
  (-> any/c boolean?)
  (or (pivot-IR? g)
      (null? g)
      (and (list? g)
           (= 1 (length g))
           (logical-line? (first g)))))
(provide pivot-basis?)

;predicate for pivot
(define/contract (pivot? g)
  (-> any/c boolean?)
  (and (pivot-struct? g)
       (pivot-IR? (pivot-struct-IR g))
       (indent? (pivot-struct-full-length g))
       (indent? (pivot-struct-last-length g))))
(provide pivot?)

;predicate for logical line internal representation
(define/contract (logical-line-IR? g)
  (-> any/c boolean?)
  (and (list? g)
       (andmap logical-print-item? g)))
(provide logical-line-IR?)

;predicate for types that can be used to make a logical line
(define/contract (logical-line-item? g)
  (-> any/c boolean?)
  (or (chunk-literal? g)
      (logical-print-item? g)
      (logical-line? g)
      (null? g)
      (and (list? g)
           (andmap logical-line-item? g))))
(provide logical-line-item?)

;predicate for logical line
(define/contract (logical-line? g)
  (-> any/c boolean?)
  (and (logical-line-struct? g)
       (logical-line-IR? (logical-line-struct-IR g))))
(provide logical-line?)

;predicate for logical block internal representation
(define/contract (logical-block-IR? g)
  (-> any/c boolean?)
  (and (list? g)
       (andmap logical-line? g)))
(provide logical-block-IR?)

;predicate for logical block internal representation
(define/contract (logical-block-item? g)
  (-> any/c boolean?)
  (or (logical-line? g)
      (and (list? g)
           (andmap logical-block-item? g))))
(provide logical-block-item?)

;predicate for logical block
(define/contract (logical-block? g)
  (-> any/c boolean?)
  (and (logical-block-struct? g)
       (logical-block-IR? (logical-block-IR g))))
(provide logical-block?)

;accessors

;pivot accessors
(define/contract (pivot-IR pivot)
  (-> pivot? pivot-IR?)
  (pivot-struct-IR pivot))
(provide pivot-IR)
(define/contract (pivot-full-length pivot)
  (-> pivot? indent?)
  (pivot-struct-full-length pivot))
(provide pivot-full-length)
(define/contract (pivot-last-length pivot)
  (-> pivot? indent?)
  (pivot-struct-last-length pivot))
(provide pivot-last-length)

;logical line accessor
(define/contract (logical-line-IR ll)
  (-> logical-line? logical-line-IR?)
  (logical-line-struct-IR ll))
(provide logical-line-IR)

;logical block accessor
(define/contract (logical-block-IR lb)
  (-> logical-block? logical-block-IR?)
  (logical-block-struct-IR lb))
(provide logical-block-IR)

;constructors

;pivot constructor
(define/contract (pivot . lst)
  (->* () #:rest pivot-basis? logical-line?)
  (let ([basis (flatten* lst)])
    (logical-line (cond [(pivot-IR? basis)
                         (pivot-struct (reverse basis)
                                       (apply +
                                              (- (length basis) 1) ;spaces between each pivoted line
                                              (map full-length-logical-line basis)) ;length of full line
                                       (last-length-logical-line lst))]
                        [(null? basis) 0]
                        ; single logical line
                        [else (logical-line-IR (first basis))]))))
(provide pivot)

;logical line constructor
(define/contract (logical-line . items)
  (->* () #:rest logical-line-item? logical-line?)
  (logical-line-struct (build-logical-line-IR items)))
(provide logical-line)

;logical block constructor
(define/contract (logical-block . items)
  (->* () #:rest logical-block-item? logical-block?)
  (logical-line-struct (flatten* items)))
(provide logical-line)

;general procedures

;convert literal chunk to logical line internal representation
(define/contract (chunk-literal->logical-line-IR chunk)
  (-> chunk-literal? logical-line-IR?)
  (cond [(symbol? chunk) (chunk-literal->logical-line-IR (symbol->string chunk))]
        [(string? chunk) (reverse (string->list chunk))]
        ;char or indent
        [else (list chunk)]))
(provide chunk-literal->logical-line-IR)

;simply logical line internal representation
(define/contract (simplify-logical-line-IR lst)
  (-> logical-line-IR? logical-line-IR?)
  (foldl (λ (item llir) (cond [(and (indent? item)
                                    (= 0 item))
                               llir]
                              [(and (indent? item)
                                    (non-empty-list? llir)
                                    (indent? (first llir)))
                               (cons (+ item (first llir))
                                     (rest llir))]
                              [else (cons item llir)]))
         null
         lst))
(provide simplify-logical-line-IR)

;build logical line internal representation
(define/contract (build-logical-line-IR . items)
  (->* () #:rest logical-line-item? logical-line-IR?)
  (simplify-logical-line-IR (flatten* (foldl (λ (item llir)
                                               (append (cond [(chunk-literal? item) (chunk-literal->logical-line-IR item)]
                                                             [(pivot-struct? item) item]
                                                             [(logical-line? item) (logical-line-IR item)]
                                                             [(list? item) (build-logical-line-IR item)])
                                                       llir))
                                             null
                                             items))))
(provide build-logical-line-IR)

;procedure to compute full length of logical line
(define/contract (full-length-logical-line ll)
  (-> logical-line? indent?)
  (foldl (λ (g t) (+ t
                     (cond [(char? g) 1]
                           [(indent? g) g]
                           [(pivot-struct? g) (pivot-struct-full-length g)])))
         (logical-line-IR ll)))
(provide full-length-logical-line)

;procedure to compute length of last logical line
(define/contract (last-length-logical-line ll)
  (-> logical-line? indent?)
  (foldl (λ (g t) (+ t
                     (cond [(char? g) 1]
                           [(indent? g) g]
                           [(pivot-struct? g) (pivot-struct-last-length g)])))
         (logical-line-IR ll)))
(provide last-length-logical-line)
