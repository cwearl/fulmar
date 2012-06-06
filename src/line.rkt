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
  (flat-list-of? print-item? 1 g))
(provide line-IR?)

;predicate for line intermediate internal representation
(define/contract (line-IIR? g)
  pred/c
  (flat-list-of? print-item? 0 g))
(provide line-IIR?)

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
(define/contract (line . g)
  (->* () #:rest line-input? line?)
  (let ([items (simplify-line-IIR (build-line-IIR g))])
    (line-struct (if (null? items)
                     (list 0)
                     items))))
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
  (first (line-IR line)))
(provide line-last)

;return line containing all but the last/current print item
(define/contract (line-rest line)
  (-> line? line?)
  (let ([ir (line-IR line)])
    (line-struct (if (< 1 (length ir))
                     (rest ir)
                     (list 0)))))
(provide line-rest)

;return first print item
(define/contract (line-first line)
  (-> line? print-item?)
  (last (line-IR line)))
(provide line-first)

;return line containing all but the first print item
(define/contract (line-tser line)
  (-> line? line?)
  (let ([ir (line-IR line)])
    (line-struct (if (< 1 (length ir))
                     (drop-right ir 1)
                     (list 0)))))
(provide line-tser)

;add given print item to line in a sequence after the last/current item in line
(define/contract (add-to-last li item)
  (-> line? print-item? line?)
  (line li item))
(provide add-to-last)

;add given print item to line in a sequence before the first item in line
(define/contract (add-to-first item li)
  (-> print-item? line? line?)
  (line item li))
(provide add-to-first)

;add given print item to line in a sequence with the last/current item in line
(define/contract (seq-with-last li item)
  (-> line? print-item? line?)
  (line (line-rest li)
        (let ([last (line-last li)])
          (cond [(and (pivot? last)
                      (pivot? item))
                 ;both are pivots...
                 (pivot (pivot-rest last)
                        (let* ([new-line-prefix (pivot-last last)]
                               [item-line (pivot-first item)]
                               [new-item (line-first item-line)]
                               [new-line-suffix (line-tser item-line)])
                          (line (seq-with-last new-line-prefix
                                               new-item)
                                new-line-suffix))
                        (pivot-tser item))]
                [(pivot? last)
                 ;last is a pivot - item is not
                 (pivot (pivot-rest last)
                        (seq-with-last (pivot-last last)
                                       item))]
                [(pivot? item)
                 ;item is a pivot - last is not
                 (pivot (seq-with-first last
                                        (pivot-first item))
                        (pivot-tser item))]
                [else
                 ;neither last nor item is a pivot
                 (seq last item)]))))
(provide seq-with-last)

;add given print item to line in a sequence with the first item in line
(define/contract (seq-with-first item li)
  (-> print-item? line? line?)
  (line (let ([first (line-first li)])
          (cond [(and (pivot? first)
                      (pivot? item))
                 ;both are pivots...
                 (pivot (pivot-rest item)
                        (let* ([new-line-prefix (pivot-last item)]
                               [item-line (pivot-first first)]
                               [new-item (line-first item-line)]
                               [new-line-suffix (line-tser item-line)])
                          (line (seq-with-last new-line-prefix
                                               new-item)
                                new-line-suffix))
                        (pivot-tser first))]
                [(pivot? first)
                 ;first is a pivot - item is not
                 (pivot (seq-with-first item
                                        (pivot-first first))
                        (pivot-tser first))]
                [(pivot? item)
                 ;item is a pivot - first is not
                 (pivot (pivot-rest item)
                        (seq-with-last (pivot-last item)
                                       first))]
                [else
                 ;neither last nor item is a pivot
                 (seq item first)]))
        (line-tser li)))
(provide seq-with-first)

;general procedures

;build line internal representation
(define/contract (build-line-IIR . g)
  (->* () #:rest line-input? line-IIR?)
  (let ([items (flatten* g)])
    ;foldl (as opposed to foldr) is used to reverse list as it build it
    (foldl (λ (item lir)
             (cond [(print-item? item)
                    (cons item lir)]
                   [(line? item)
                    (append (line-IR item) lir)]
                   [else
                    (error "Unrecognized input for line; unrecognized: " item "; given: " items)]))
           null
           items)))
(provide build-line-IIR)

;simply line internal representation
(define/contract (simplify-line-IIR g)
  (-> line-IIR? line-IIR?)
  ;foldr is used to keep list in same order as given
  (foldr (λ (item lir)
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
         (flatten* g)))
(provide simplify-line-IIR)

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
  (flat-list-of? line? 2 g))
(provide pivot-IR?)

;predicate for pivot intermediate internal representation
(define/contract (pivot-IIR? g)
  pred/c
  (flat-list-of? line? 0 g))
(provide pivot-IIR?)

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
  (let ([lines (build-pivot-IIR g)])
    (cond [(null? lines)
           0]
          [(= 1 (length lines))
           (first lines)]
          [else
           (pivot-struct lines
                         (pivot-full-line-length lines))])))
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
(define/contract (pivot-last g)
  (-> pivot? line?)
  (first (pivot-IR g)))
(provide pivot-last)

;return pivot containing all but the last/current line
(define/contract (pivot-rest g)
  (-> pivot? pivot-output?)
  (let ([pir (pivot-IR g)])
    (if (= 2 (length pir))
        (second pir)
        (pivot-struct (rest pir)
                      (pivot-full-line-length (rest pir))))))
(provide pivot-rest)

;return first line
(define/contract (pivot-first g)
  (-> pivot? line?)
  (last (pivot-IR g)))
(provide pivot-first)

;return pivot containing all but the last/current line
(define/contract (pivot-tser g)
  (-> pivot? pivot-output?)
  (let ([pir (pivot-IR g)])
    (if (= 2 (length pir))
        (first pir)
        (pivot-struct (drop-right pir 1)
                      (pivot-full-line-length (drop-right pir 1))))))
(provide pivot-tser)

;general procedures

;procedure to build a pivot intermediate internal representation
(define/contract (build-pivot-IIR g)
  (-> pivot-input? pivot-IIR?)
  ;foldl is used to reverse list
  (foldl (λ (item pir)
           (cond [(line? item)
                  (cons item pir)]
                 [(pivot? item)
                  (append (pivot-IR item) pir)]
                 [else
                  (error "Unrecognized pivot input; unrecognized: " item "; given: " g)]))
         null
         (flatten* g)))
(provide build-pivot-IIR)

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
                               [(seq? g) (full-seq-length g)]
                               [(pivot? g) (pivot-length g)]
                               [else
                                (error "Unrecognized print item; given: " g)]))
                  (line-IR l))))
(provide full-line-length)

;procedure to compute full length of sequence
(define/contract (full-seq-length s)
  (-> seq? indent?)
  (foldl + 0 (map (λ (g) (cond [(char? g) 1]
                               [(indent? g) g]
                               [else
                                (error "Unrecognized print item; given: " g)]))
                  (seq-IR s))))
(provide full-seq-length)
