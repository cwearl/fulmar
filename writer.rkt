#lang racket

(require "fulmar-core.rkt")

;error nekot...
(define/contract (unknown-nekot-type body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  (error "Unrecognized nekot/chunk; given: " body))
(provide unknown-nekot-type)

;add empty nekot
; - equivalent to identity function...
(define/contract (add-empty body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  lines)
(provide add-empty)

;TODO: take environment into account
;return n spaces
(define/contract (build-indentation context)
  (-> context/c string?)
  (let ([length (context-indent context)])
    (make-string length #\ )))
(provide build-indentation)

;TODO: take environment into account
;finish line
(define/contract (finish-line string context)
  (-> string? context/c string?)
  string)
;(provide finish-line)

;current line length
(define/contract (line-length lines)
  (-> written-lines/c natural-number/c)
  (string-length (car lines)))
;(provide line-length)

;speculative line length
(define/contract (speculative-line-length string lines)
  (-> string? written-lines/c natural-number/c)
  (+ (line-length lines)
     (string-length string)))
;(provide speculative-line-length)

;check line length
; returns true if line length is less than or equal to the max length for the current context
; returns false otherwise
(define/contract (check-line-length int/string context lines)
  (-> (or/c natural-number/c string?) context/c written-lines/c boolean?)
  (<= (if (exact-nonnegative-integer? int/string)
          (+ int/string
             (line-length lines))
          (speculative-line-length int/string lines))
      (context-line-length context)))
;(provide check-line-length)

;add a literal string to current line
(define/contract (add-literal string context lines)
  (-> string? context/c written-lines/c written-lines/c)
  (append (if (check-line-length string context lines)
              (list (string-append (car lines) string))
              (list (string-append (build-indentation context) string)
                    (finish-line (car lines) context)))
          (cdr lines)))
;(provide add-literal)

;add spaces to current line
(define/contract (add-spaces count context lines)
  (-> natural-number/c context/c written-lines/c written-lines/c)
  (cond [(= 0 count)
         lines]
        [(check-line-length count context lines)
         (cons (string-append (car lines) (make-string count #\ ))
               (cdr lines))]
        [else (append (list (build-indentation context);(make-string count #\ )
                            (finish-line (car lines) context))
                      (cdr lines))]))
;(provide add-spaces)

;add new line
(define/contract (add-new-line body context lines)
  (-> any/c context/c written-lines/c written-lines/c)
  (if (equal? (car lines)
              (build-indentation context))
      lines
      (append (list (build-indentation context)
                    (finish-line (car lines) context))
              (cdr lines))))
;(provide add-new-line)

;TODO: add check that blank line isn't accidentally repeated
;add blank line
(define/contract (add-blank-lines count context lines)
  (-> natural-number/c context/c written-lines/c written-lines/c)
  (cond [(= 0 count)
         lines]
        [(is-whitespace? (car lines))
         (add-blank-lines count context (cdr lines))]
        [else (append (list (build-indentation context))
                      (make-list count "")
                      (list (finish-line (car lines)))
                      (cdr lines))]))
;(provide add-blank-line)

;checks if given string is just spaces
(define/contract (is-whitespace? string)
  (-> written-line/c boolean?)
  (letrec ([is-whitespace-list? (λ (lst) (cond [(empty? lst) #true]
                                               [(eq? #\  (car lst)) (is-whitespace-list? (cdr lst))]
                                               [else #false]))])
    (is-whitespace-list? string)))
;(provide is-whitespace?)

;add '#' to proper place in given line
; - assumes that line is result from build-indentation
(define/contract (add-hash-character line)
  (-> written-line/c written-line/c)
  (if (is-whitespace? line)
      (string-append "#" line)
      (string-append line "#")))
;(provide add-hash-character)

;add preprocessor directive
(define/contract (add-pp-directive directive context lines)
  (-> string? context/c written-lines/c written-lines/c)
  (append (string-append (add-hash-character (car lines))
                         directive)
          (cdr lines)))
;(provide add-pp-directive)

;add concatenated nekots
(define/contract (add-concatenated nekots context lines)
  (-> (listof nekot/c) context/c written-lines/c written-lines/c)
  (for/fold ([new-lines lines]) ([nekot (in-list nekots)])
    (write-nekot nekot new-lines)))
;(provide add-concatenated)

;write nekot
(define/contract (write-nekot nekot lines)
  (-> nekot/c written-lines/c written-lines/c)
  (let ([nekot-writer (match (nekot-name nekot)
                        ['empty        add-empty]
                        ['literal      add-literal]
                        ['spaces       add-spaces]
                        ['new-line     add-new-line]
                        ['blank-lines  add-blank-lines]
                        ['pp-directive add-pp-directive]
                        ['concat       add-concatenated]
                        [_             unknown-nekot-type])])
    (nekot-writer (nekot-body nekot) (nekot-context nekot) lines)))
;(provide write-nekot)

;Write file
(define/contract (write-file nekot)
  (-> nekot/c written-lines/c)
  (write-nekot nekot (list "")))
(provide write-file)
