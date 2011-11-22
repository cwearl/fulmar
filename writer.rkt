#lang racket

(require "fulmar-core.rkt")

;;;;;;;;;;;;;;;;;;;
;;helper functions;
;;;;;;;;;;;;;;;;;;;

;TODO: take environment into account
;return n spaces
(define/contract (build-indentation context)
  (-> context/c string?)
  (make-string (context-indent context) #\ ))
(provide build-indentation)

;TODO: take environment into account
;finish line
(define/contract (finish-line lines context)
  (-> written-lines/c context/c string?)
  (car lines))
(provide finish-line)

;check line length
; returns true if line length is less than or equal to the max length for the current context
; returns false otherwise
(define/contract (check-line-length new-length context lines)
  (-> natural-number/c context/c written-lines/c boolean?)
  (<= (+ new-length
         (string-length (car lines)))
      (context-line-length context)))
(provide check-line-length)

;checks if given string is just spaces
(define/contract (is-whitespace? string)
  (-> written-line/c boolean?)
  (letrec ([is-whitespace-list? (λ (lst) (cond [(empty? lst) #true]
                                               [(eq? #\  (car lst)) (is-whitespace-list? (cdr lst))]
                                               [else #false]))])
    (is-whitespace-list? string)))
(provide is-whitespace?)

;add '#' to proper place in given line
(define/contract (add-hash-character line)
  (-> written-line/c written-line/c)
  (if (is-whitespace? line)
      (string-append "#"
                     (substring line 1))
      (string-append (substring line 0 (- (string-length line) 2))
                     "#")))
(provide add-hash-character)

;;;;;;;;;;;;;;;;;;;
;;nekot handlers;;;
;;;;;;;;;;;;;;;;;;;

;add empty nekot
; - equivalent to identity function...
(define/contract (add-empty body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  lines)
(provide add-empty)

;add a literal string to current line
(define/contract (add-literal string context lines)
  (-> string? context/c written-lines/c written-lines/c)
  (if (apply check-line-length (string-length string) context lines)
      (list* (string-append (car lines) string)
             (cdr lines))
      (list* (string-append (build-indentation context) string)
             (finish-line lines context)
             (cdr lines))))
(provide add-literal)

;add spaces to current line
(define/contract (add-spaces count context lines)
  (-> natural-number/c context/c written-lines/c written-lines/c)
  (cond [(= 0 count)
         lines]
        [(check-line-length count context lines)
         (list* (string-append (car lines) (make-string count #\ ))
                (cdr lines))]
        [else (list* ""
                     (finish-line lines context)
                     (cdr lines))]))
(provide add-spaces)

;add new line
; - guarantees last line of lines is a new line
; - if last line is blank, resets indent of that line
;   else                   finishes last line and starts new one
(define/contract (add-new-line body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  (if (is-whitespace? (car lines))
      (list* ""
             (cdr lines))
      (list* ""
             (finish-line lines context)
             (cdr lines))))
(provide add-new-line)

;add blank line
(define/contract (add-blank-lines count context lines)
  (-> natural-number/c context/c written-lines/c written-lines/c)
  (cond [(= 0 count)
         lines]
        [(is-whitespace? (car lines))
         (add-blank-lines count context (cdr lines))]
        [else (append (make-list (+ count 1) "")
                      (list (finish-line lines context))
                      (cdr lines))]))
(provide add-blank-lines)

;add preprocessor directive
(define/contract (add-pp-directive body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  (cons (add-hash-character (car lines))
        (cdr lines)))
(provide add-pp-directive)

;add concatenated nekots
(define/contract (add-concatenated nekots context lines)
  (-> (listof nekot/c) context/c written-lines/c written-lines/c)
  (for/fold ([new-lines lines]) ([nekot (in-list nekots)])
    (write-nekot nekot new-lines)))
(provide add-concatenated)

;error nekot...
(define/contract (unknown-nekot-type body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  (error "Unrecognized nekot/chunk; given: " body))
(provide unknown-nekot-type)

;write nekot
(define/contract (write-nekot nekot lines)
  (-> nekot/c written-lines/c written-lines/c)
  (let* ([context-obj (nekot-context nekot)]
         [nekot-writer (match (nekot-name nekot)
                         ['empty        add-empty]
                         ['literal      add-literal]
                         ['spaces       add-spaces]
                         ['new-line     add-new-line]
                         ['blank-lines  add-blank-lines]
                         ['pp-directive add-pp-directive]
                         ['concat       add-concatenated]
                         [_             unknown-nekot-type])]
         [new-lines (if (equal? (car lines) "")
                        (cons (build-indentation context-obj)
                              (cdr lines))
                        lines)])
    (nekot-writer (nekot-body nekot) context-obj new-lines)))
(provide write-nekot)

;Write file
(define/contract (write-file nekot)
  (-> nekot/c written-lines/c)
  (write-nekot nekot (list "")))
(provide write-file)
