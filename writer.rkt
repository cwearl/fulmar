#lang racket

(require "fulmar-core.rkt")
(require "chunk-core.rkt")

;;;;;;;;;;;;;;;;;;;
;;helper functions;
;;;;;;;;;;;;;;;;;;;

;checks if given string is just spaces
(define/contract (is-whitespace? string)
  (-> written-line/c boolean?)
  (letrec ([is-whitespace-list? (λ (lst) (cond [(empty? lst) #true]
                                               [(eq? #\  (car lst)) (is-whitespace-list? (cdr lst))]
                                               [else #false]))])
    (is-whitespace-list? (string->list string))))
(provide is-whitespace?)

;give n spaces
(define/contract (make-whitespace n)
  (-> natural-number/c string?)
  (make-string n #\ ))
(provide make-whitespace)

;remove whitespace from the end of a line
(define/contract (remove-whitespace line)
  (-> written-line/c written-line/c)
  (list->string (reverse (cdr (foldl (λ (char result) (if (equal? char #\ )
                                                           (cons (cons #\  (car result))
                                                                 (cdr result))
                                                           (cons '()
                                                                 (append (cons char (car result))
                                                                         (cdr result)))))
                                      '(())
                                      (string->list line))))))
(provide remove-whitespace)

;build indentation for new line given current context
(define/contract (build-indentation context [char #\ ])
  (-> context/c string?)
  (if (or (empty-env? (context-env context))
          (macro-env? (context-env context)))
      (make-whitespace (context-indent context))
      ;environment has to have comment in it somewhere: comment, comment-macro, or macro-comment
      (string-append (make-whitespace (context-initial-position context))
                     "/*"
                     (string char)
                     (let ([remaining (- (context-indent context)
                                         (context-initial-position context))])
                       (if (< 0 remaining)
                           (make-whitespace remaining)
                           "")))))
(provide build-indentation)

;finish line
(define/contract (finish-line given-line context)
  (-> written-line/c context/c string?)
  (let* ([line (remove-whitespace given-line)]
         [length (string-length line)]
         [max (context-line-length context)]
         [env (context-env context)]
         [env-spaces (cond [;empty environment
                            (empty-env? env)
                            0]
                           [;comment environment
                            (comment-env? env)
                            2]
                           [;macro environment
                            (macro-env? env)
                            1]
                           [;comment-macro or macro-comment environment
                            (or (comment-macro-env? env)
                                (macro-comment-env? env))
                            4]
                           [;unknown environment
                            else
                            (error "Contract for finish line should prevent this case from coming up; good luck! Given: " given-line context)])])
    (cond [;empty environment
           (empty-env? env)
           line]
          [;empty line
           (equal? (remove-whitespace (build-indentation context))
                   line)
           (cond [;comment or comment-macro line
                  (or (comment-env? env)
                      (comment-macro-env? env))
                  ""]
                 [; macro or macro-comment line
                  (or (macro-env? env)
                      (macro-comment-env? env))
                  (string-append (make-whitespace max)
                                 "\\")])]
          [else
           ;non-empty line
           (string-append line
                          (if (< (+ length env-spaces) max)
                              (make-whitespace (- max length env-spaces))
                              " ")
                          (cond [(comment-env? env) "*/"]
                                [(macro-env? env) "\\"]
                                [(comment-macro-env? env) "\\ */"]
                                [(macro-comment-env? env) "*/ \\"]))])))
(provide finish-line)

;check speculative line
(define/contract (check-speculative-line-length first-part second-part context)
  (-> (or/c natural-number/c string?) string? context/c boolean?)
  (<= (+ (if (string? first-part)
             (string-length first-part)
             first-part)
         (string-length second-part))
      (context-line-length context)))
(provide check-speculative-line-length)

;check if lengths match
(define/contract (match-lengths first-line second-line)
  (-> (or/c natural-number/c string?) string? boolean?)
  (= (if (string? first-line)
         (string-length first-line)
         first-line)
     (string-length second-line)))
(provide match-lengths)

;add '#' to proper place in given line
(define/contract (add-hash-character line)
  (-> written-line/c written-line/c)
  (cond [(= 0 (string-length line)) "#"]
        [(is-whitespace? line)
         (string-append "#"
                        (substring line 1))]
        [else
         (string-append (substring line 0 (- (string-length line) 1))
                        "#")]))
(provide add-hash-character)

;;;;;;;;;;;;;;;;;;;
;;nekot handlers;;;
;;;;;;;;;;;;;;;;;;;

;add empty nekot
; - equivalent to identity function...
(define/contract (add-empty body context line)
  (-> any/c context/c written-line/c written-lines/c)
  (list (if (equal? (build-indentation context)
                    line)
            ""
            line)))
(provide add-empty)

;add a literal string to current line
(define/contract (add-literal string context line)
  (-> string? context/c written-line/c written-lines/c)
  (cond [(= 0 (string-length string))
         (list line)]
        [(or (check-speculative-line-length string line context)
             (>= (string-length (build-indentation context))
                 (string-length line)))
         (list (string-append line string))]
        [else
         (list (string-append (build-indentation context) string)
               (finish-line line context))]))
(provide add-literal)

;add spaces to current line
(define/contract (add-spaces count context line)
  (-> natural-number/c context/c written-line/c written-lines/c)
  (cond [(= 0 count)
         (list line)]
        [(check-speculative-line-length count line context)
         (list (string-append line (make-whitespace count)))]
        [else
         (list ""
               (finish-line line context))]))
(provide add-spaces)

;add new line
(define/contract (add-new-line body context line)
  (-> any/c context/c written-line/c written-lines/c)
  (list ""
        (finish-line line context)))
(provide add-new-line)

;add preprocessor directive
(define/contract (add-pp-directive body context line)
  (-> any/c context/c written-line/c written-lines/c)
  (list (add-hash-character line)))
(provide add-pp-directive)

;add concatenated nekots
(define/contract (add-concatenated nekots context line)
  (-> (non-empty-listof nekot/c) context/c written-line/c written-lines/c)
  (for/fold ([lines (list line)]) ([nekot (in-list nekots)])
    (append (write-nekot nekot (car lines))
            (cdr lines))))
(provide add-concatenated)

;add a bottom-level list of chunks to current line
; - forces all chunks to go on the same line (except for new line)
; - no added spaces or new lines
(define/contract (add-bot-list nekots context line)
  (-> (non-empty-listof nekot/c) context/c written-line/c written-lines/c)
  (define/contract (add-bot-list-internal nekots context lines)
    (-> (listof nekot/c) context/c written-lines/c written-lines/c)
    (if (empty? nekots)
        lines
        (let* ([nekot (car nekots)]
               [name (nekot-name nekot)]
               [body (nekot-body nekot)]
               [context-obj (nekot-context nekot)]
               [line (car lines)]
               [new-line (if (equal? "" line)
                             (build-indentation context)
                             line)])
          (add-bot-list-internal (cdr nekots)
                                 context
                                 (append (match name
                                           ['empty        (add-empty body context-obj new-line)]
                                           ['literal      (list (string-append new-line body))]
                                           ['spaces       (list (string-append new-line (make-whitespace body)))]
                                           ['new-line     (add-new-line body context-obj new-line)]
                                           ['pp-directive (add-pp-directive body context-obj new-line)]
                                           ['concat       (add-concatenated body context-obj new-line)]
                                           ['bot-list     (add-bot-list body context-obj new-line)]
                                           ['low-list     (add-low-list body context-obj new-line)]
                                           [_             ((unknown-nekot-type name) body context-obj new-line)])
                                         (cdr lines))))))
  (add-bot-list-internal nekots context (list line)))
(provide add-bot-list)

;add a low-level list of chunks to current line
; - attempts to put chunks on a single line with a space between each chunk
; - if that fails, puts chunks on their own lines
(define/contract (add-low-list chunks context line)
  (-> (non-empty-listof chunk/c) context/c written-line/c written-lines/c)
  (let ([attempt (add-concatenated (map (λ (chunk) (chunk context))
                                        (add-between chunks (spaces-chunk 1)))
                                   context
                                   line)])
    (if (or (= 1 (length attempt))
            (and (= 2 (length attempt))
                 (equal? "" (car attempt))))
        attempt
        (let* ([new-context (reindent (- (string-length line)
                                         (context-indent context))
                                      context)])
          (add-concatenated (map (λ (chunk) (chunk new-context))
                                 (add-between chunks new-line-chunk))
                            new-context
                            line)))))
(provide add-low-list)

;error nekot...
(define/contract (unknown-nekot-type name)
  (-> symbol? (-> any/c context/c written-line/c written-lines/c))
  (λ (body context line)
    (error "Unrecognized nekot/chunk; given: " name body context line)))
;(define/contract (unknown-nekot-type body context line)
;  (-> any/c context/c written-line/c written-lines/c)
;  (error "Unrecognized nekot/chunk; given: " body context line))
(provide unknown-nekot-type)

;write nekot
(define/contract write-nekot
  (case-> (-> nekot/c written-lines/c)
          (-> nekot/c written-line/c written-lines/c))
  (case-lambda [(nekot)
                (write-nekot nekot "")]
               [(nekot line)
                (let* ([name (nekot-name nekot)]
                       [context-obj (nekot-context nekot)]
                       [nekot-writer (match name
                                       ['empty        add-empty]
                                       ['literal      add-literal]
                                       ['spaces       add-spaces]
                                       ['new-line     add-new-line]
                                       ['pp-directive add-pp-directive]
                                       ['concat       add-concatenated]
                                       ['bot-list     add-bot-list]
                                       ['low-list     add-low-list]
                                       [_             (unknown-nekot-type name)])]
                       [new-line (if (equal? line "")
                                     (build-indentation context-obj)
                                     line)])
                  (nekot-writer (nekot-body nekot) context-obj new-line))]))
(provide write-nekot)
