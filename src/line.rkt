#lang racket

(require "basics.rkt")
(require "sequence.rkt")

;structures, functions, and contracts for line (and pivot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;line;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;structures

;line structure
(struct line-struct (IR) #:transparent)

;predicates

;predicate for print item
(define/contract (print-item? g)
  pred/c
  ((or? pivot?
        seq?
        char?
        indent?) g))
(provide print-item?)

;predicate for line internal representation
(define/contract (line-IR? g)
  pred/c
  (list-of? print-item? 1 g))
(provide line-IR?)

;predicate for building line
(define/contract (line-input? g)
  pred/c
  (list-of? (or? line?
                 print-item?) 0 g))
(provide line-input?)

;predicate for line
(define/contract (line? g)
  pred/c
  (and (line-struct? g)
       (line-IR? (line-struct-IR g))))
(provide line?)

;constructors

;line constructor
(define/contract (line . items)
  (->* () #:rest line-input? line?)
  (line-struct (simplify-line-IR (build-line-IR items))))
(provide line)

;accessors

;line accessor
(define/contract (line-IR l)
  (-> line? line-IR?)
  (line-struct-IR l))
(provide line-IR)

;transformers

;return last/current print item
(define/contract (line-last line)
  (-> line? print-item?)
  (let ([ir (line-IR line)])
    (if (list? ir)
        (first ir)
        ir)))
(provide line-last)

;return line containing all but the last/current print item
(define/contract (line-rest line)
  (-> line? line?)
  (let ([ir (line-IR line)])
    (line-struct (if (and (list? ir)
                          (< 1 (length ir)))
                     (rest ir)
                     0))))
(provide line-rest)

;general procedures

;build line internal representation
(define/contract (build-line-IR . g)
  (->* () #:rest line-input? line-IR?)
  (let ([items (flatten* g)])
    (if (null? items)
        0
        ;foldl (as opposed to foldr) is used to reverse list as it build it
        (foldl (λ (item lir) 
                 (cond [(print-item? item) (cons item lir)]
                       [(line? item) (append (line-IR item) lir)]
                       [else
                        (error "Unrecognized input for line; unrecognized: " item "; given: " items)]))
               null
               items))))
(provide build-line-IR)

;simply line internal representation
(define/contract (simplify-line-IR g)
  (-> line-IR? line-IR?)
  ;foldr is used to keep list in same order as given
  (let ([new-lir (foldr (λ (item lir)
                          (cond [(and (indent? item)
                                      (= 0 item))
                                 lir]
                                [(and (indent? item)
                                      (non-empty-list? lir)
                                      (indent? (first lir)))
                                 (cons (+ item (first lir))
                                       (rest lir))]
                                [else (cons item lir)]))
                        null
                        (flatten* g))])
    (if (null? new-lir)
        0
        new-lir)))
(provide simplify-line-IR)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pivot;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;structures

;pivot structure
(struct pivot-struct (IR length) #:transparent)

;predicates

;predicate for pivot internal representation
(define/contract (pivot-IR? g)
  pred/c
  (list-of? line? 2 g))
(provide pivot-IR?)

;predicate for building pivot
(define/contract (pivot-input? g)
  pred/c
  (list-of? (or? pivot? line?) 0 g))
(provide pivot-input?)

;predicate for pivot
(define/contract (pivot? g)
  pred/c
  (and (pivot-struct? g)
       (pivot-IR? (pivot-struct-IR g))
       (indent? (pivot-struct-length g))))
(provide pivot?)

;predicate for output of pivot
(define/contract (pivot-output? g)
  pred/c
  ((or? pivot?
        line?
        indent?) g))
(provide pivot-output?)

;constructors

;pivot constructor
(define/contract (pivot . g)
  (->* () #:rest pivot-input? pivot-output?)
  (build-pivot g))
(provide pivot)

;accessors

;pivot IR accessor
(define/contract (pivot-IR pivot)
  (-> pivot? pivot-IR?)
  (pivot-struct-IR pivot))
(provide pivot-IR)

;pivot full length accessor
(define/contract (pivot-length pivot)
  (-> pivot? indent?)
  (pivot-struct-length pivot))
(provide pivot-length)

;transformers

;return last/current line
(define/contract (pivot-last pivot)
  (-> pivot? line?)
  (first (pivot-IR pivot)))
(provide pivot-last)

;return line containing all but the last/current line
(define/contract (pivot-rest pivot)
  (-> pivot? pivot-output?)
  (let ([ir (line-IR line)])
    (line-struct (if (and (list? ir)
                          (< 1 (length ir)))
                     (rest ir)
                     0))))
(provide pivot-rest)

;general procedures

;procedure to build a pivot
(define/contract (build-pivot g)
  (-> pivot-input? pivot-output?)
  ;foldl is used to reverse list
  (let ([pir (foldl (λ (item pir)
                      (cond [(line? item)
                             (cons item pir)]
                            [(pivot? item)
                             (append (pivot-IR item) pir)]
                            [else
                             (error "Unrecognized pivot input; unrecognized: " item "; given: " g)]))
                    null
                    (flatten* g))])
    (cond [(pivot-IR? pir)
           (pivot-struct pir
                         (pivot-full-line-length pir))]
          [(and (= 1 (length pir))
                (line? (first pir)))
           (first pir)]
          [(null? pir)
           0]
          [else
           (error "Unknown pivot input; given: " g)])))
(provide build-pivot)

;procedure to compute full length of pivot
(define/contract (pivot-full-line-length g)
  (-> (non-empty-listof line?) indent?)
  (+ (- (length g) 1) ;spaces between each pivoted line
     (foldl + 0 (map full-line-length g))))
(provide pivot-full-line-length)

;procedure to compute full length of line
(define/contract (full-line-length l)
  (-> line? indent?)
  (foldl + 0 (map (λ (g) (cond [(char? g) 1]
                               [(indent? g) g]
                               [(pivot? g) (pivot-length g)]))
                  (line-IR l))))
(provide full-line-length)
