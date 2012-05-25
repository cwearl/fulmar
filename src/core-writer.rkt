#lang racket

;basic structures, functions, and contracts for fulmar writer

;predicate for basic print item
(define/contract (basic-print-item? g)
  (-> any/c boolean?)
  (or (char? g)
      (indent? g)))
(provide basic-print-item?)

;logical pivot structure
(struct pivot-struct (sections full-length last-length) #:transparent)

;predicate for logical print item
(define/contract (logical-print-item? g)
  (-> any/c boolean?)
  (or (basic-print-item? g)
      (and (pivot-struct? g)
           (non-empty-list? (pivot-struct-sections g))
           (andmap (λ (i) (andmap logical-print-item? i))
                   (pivot-struct-sections g)))))

;predicate for logical line internal representation
(define/contract (logical-line-IR? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap logical-print-item? g)))
(provide logical-line-IR?)

;logical line structure
(struct logical-line (IR) #:transparent)

;predicate for logical block internal representation
(define/contract (logical-block-IR? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap (λ (ll) (and (logical-line? ll)
                            (logical-line-IR? (logical-line-IR ll))))
               g)))
(provide logical-block-IR?)

;logical block structure
(struct logical-block (IR) #:transparent)

;predicate for logical pivot IR
(define/contract (pivot-IR? g)
  (-> any/c boolean?)
  (and (non-empty-list? g)
       (andmap logical-line-IR? g)))
(provide pivot-IR?)

;convert literal chunk to logical line internal representation
(define/contract (literal-chunk->logical-line-IR chunk)
  (-> literal-chunk? logical-line-IR?)
  (cond [(symbol? chunk) (literal-chunk->logical-line-IR (symbol->string chunk))]
        [(string? chunk) (reverse (string->list chunk))]
        [(eq? chunk 0) null]
        ;char or positive indent
        [else (list chunk)]))
(provide literal-chunk->logical-line-IR)

;convert literal chunk(s) to logical line internal representation
(define/contract (literal-chunks->logical-line-IR . chunks)
  (->* () #:rest (listof literal-chunk?) logical-line-IR?)
  (define (logical-append n t)
    (cond [(null? n) t]
          [(and (indent? (first n))
                (indent? (first t)))
           (cons (+ (first n)
                    (first t))
                 (rest t))]
          [else (append n t)]))
  (foldl logical-append (map literal-chunk->logical-line-IR chunks)))
(provide literal-chunk->logical-line-IR)

;procedure to compute length of logical section
(define/contract (length-logical-line-IR sec)
  (-> logical-section-IR? indent?)
  (foldl (λ (g t) (+ t
                     (cond [(char? g) 1]
                           [(indent? g) g]
                           [(pivot-struct? g) (pivot-struct-length g)])))
         sec))
(provide length-logical-line-IR)

;contract for pivot-struct
;(provide/contract (struct pivot-struct ([sections logical-line-IR?]
;                                        [length indent?])))

;procedure to build pivot
(define/contract (pivot . lst)
  (->* () #:rest logical-line-IR? pivot-struct?)
  (pivot-struct lst
                (apply max (map length-logical-line-IR lst))))
(provide pivot)

;predicate for pivot
(define/contract (pivot? g)
  (-> any/c boolean?)
  (and (pivot-struct? g)
       (non-empty-list? (pivot-struct-sections g))
       (andmap logical-line-IR? (pivot-struct-sections g))
       (indent? (pivot-struct-length g))
       (indent? (pivot-struct))
(provide pivot?)

;procedure to build pivot from explict list
(define/contract (pivot-lst lst)
  (-> logical-line-IR? pivot-struct?)
  (apply pivot lst))

;logical line
(struct logical-line-struct (sections length) #:transparent)
(provide/contract (struct logical-line-struct ([sections logical-line-IR?]
                                               [length indent?])))

(define/provide-test-suite test-basic-line-string?
  (test-case
   "Test basic-line-string?"
   (check-true (basic-line-string? (list 0 #\a 4)))
   (check-false (basic-line-string? (list 0 #\a 4 "a")))
   (check-equal? 'is-this 'construct-needed?)))