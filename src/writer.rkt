#lang racket

(require "fulmar-core.rkt")
(require "core-chunk.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;checks if given string is just spaces
(define (is-whitespace? string)
  (letrec ([is-whitespace-list? (λ (lst) (cond [(empty? lst) #true]
                                               [(eq? #\  (car lst)) (is-whitespace-list? (cdr lst))]
                                               [else #false]))])
    (is-whitespace-list? (string->list string))))
(provide (contract-out (is-whitespace? (-> written-line/c boolean?))))

;give n spaces
(define (make-whitespace n)
  (make-string n #\ ))
(provide (contract-out (make-whitespace (-> natural-number/c string?))))

;remove whitespace from the end of a line
(define (remove-whitespace line)
  (list->string (reverse (second (foldl (λ (char result) (if (equal? char #\ )
                                                             (list (cons #\  (first result))
                                                                   (second result))
                                                             (list null
                                                                   (flatten* char
                                                                             (first result)
                                                                             (second result)))))
                                        (list null null)
                                        (string->list line))))))
(provide (contract-out (remove-whitespace (-> written-line/c written-line/c))))

;build indentation for new line given current context
(define (build-indentation context [char #\ ])
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
(provide (contract-out (build-indentation (-> context/c string?))))

;finish line
(define (finish-line given-line context)
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
                            (error "Contract for finish-line should prevent this case from coming up; good luck! Given: " given-line context)])])
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
                  (string-append (make-whitespace (- max 1))
                                 "\\")])]
          [;comment environment
           (comment-env? env)
           (string-append line
                          (if (equal? #\  (last (string->list line)))
                              "*/"
                              " */"))]
          [else
           ;non-empty line
           (string-append line
                          (if (< (+ length env-spaces) max)
                              (make-whitespace (- max length env-spaces))
                              " ")
                          (cond [(macro-env? env) "\\"]
                                [(comment-macro-env? env) "\\ */"]
                                [(macro-comment-env? env) "*/ \\"]))])))
(provide (contract-out (finish-line (-> written-line/c context/c written-line/c))))

;check speculative line
(define (check-speculative-line-length first-part second-part context)
  (<= (+ (if (string? first-part)
             (string-length first-part)
             first-part)
         (string-length second-part))
      (context-line-length context)))
(provide (contract-out (check-speculative-line-length (-> (or/c natural-number/c string?) string? context/c boolean?))))

;check if lengths match
(define (match-lengths first-line second-line)
  (= (if (string? first-line)
         (string-length first-line)
         first-line)
     (string-length second-line)))
(provide (contract-out (match-lengths (-> (or/c natural-number/c string?) string? boolean?))))

;add '#' to proper place in given line
(define (add-hash-character line)
  (cond [(= 0 (string-length line)) "#"]
        [(is-whitespace? line)
         (string-append "#"
                        (substring line 1))]
        [else
         (string-append (substring line 0 (- (string-length line) 1))
                        "#")]))
(provide (contract-out (add-hash-character (-> written-line/c written-line/c))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot handlers;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;add a literal string to current line
(define (add-literal mode string context line)
  (match mode
    ['normal (cond [(= 0 (string-length string))
                    (list line)]
                   [(or (check-speculative-line-length string line context)
                        (>= (string-length (build-indentation context))
                            (string-length line)))
                    (list (string-append line string))]
                   [else
                    (list (string-append (build-indentation context) string)
                          (finish-line line context))])]
    ['immediate (list (string-append line string))]
    [_ (error "Unknown printing mode - Contract for add-literal should prevent this case from coming up; good luck! Given: "
              mode
              string
              context
              line)]))
(provide (contract-out (add-literal (-> mode/c string? context/c written-line/c written-lines/c))))

;add spaces to current line
(define (add-spaces mode count context line)
  (match mode
    ['normal (cond [(= 0 count)
                    (list line)]
                   [(check-speculative-line-length count line context)
                    (list (string-append line (make-whitespace count)))]
                   [else
                    (list ""
                          (finish-line line context))])]
    ['immediate (list (string-append line (make-whitespace count)))]
    [_ (error "Unknown printing mode - Contract for add-spaces should prevent this case from coming up; good luck! Given: "
              mode
              count
              context
              line)]))
(provide (contract-out (add-spaces (-> mode/c natural-number/c context/c written-line/c written-lines/c))))

;add new line
(define (add-new-line mode body context line)
  (list ""
        (finish-line line context)))
(provide (contract-out (add-new-line (-> mode/c null/c context/c written-line/c written-lines/c))))

;add preprocessor directive
(define (add-pp-directive mode body context line)
  (list (add-hash-character line)))
(provide (contract-out (add-pp-directive (-> mode/c null/c context/c written-line/c written-lines/c))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;meta-nekot handlers;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;add empty nekot
; - equivalent to identity function...
(define (add-empty mode body context line)
  (list (if (equal? (build-indentation context)
                    line)
            ""
            line)))
(provide (contract-out (add-empty (-> mode/c null/c context/c written-line/c written-lines/c))))

;add concatenated nekots
(define (add-concatenated mode nekots context line)
  (for/fold ([lines (list line)]) ([nekot (in-list nekots)])
    (append (write-nekot mode nekot (car lines))
            (cdr lines))))
(provide (contract-out (add-concatenated (-> mode/c (non-empty-listof nekot/c) context/c written-line/c written-lines/c))))

;add nekot immediately
(define (add-immediate mode nekot context line)
  (write-nekot 'immediate nekot line))
(provide (contract-out (add-immediate (-> mode/c nekot/c context/c written-line/c written-lines/c))))

;add nekot(s) speculatively
(define (add-speculative mode body context line)
  (let* ([attempt (first body)]
         [success? (second body)]
         [backup (third body)]
         [attempted (write-nekot mode attempt line)])
    (if (success? attempted)
        attempted
        (write-nekot mode backup line))))
(provide (contract-out (add-speculative (-> mode/c (list/c nekot/c (-> written-lines/c boolean?) nekot/c) context/c written-line/c written-lines/c))))

;change indent to length of current line
(define (change-indent-to-current mode chunk context line)
  (let* ([diff (- (string-length line)
                  (context-indent context))]
         [new-indent (if (< 0 diff)
                         diff
                         0)])
    (write-nekot mode
                 (chunk-transform chunk (reindent new-indent
                                                  context))
                 line)))
(provide (contract-out (change-indent-to-current (-> mode/c chunk/c context/c written-line/c written-lines/c))))

;error nekot...
(define (unknown-nekot-type name)
  (λ (mode body context line)
    (error "Unrecognized nekot/chunk; given: " name mode body context line)))
(provide (contract-out (unknown-nekot-type (-> symbol? (-> mode/c any/c context/c written-line/c written-lines/c)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot writer;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;write nekot
; mode determines if pretty-printing is on or not
; - normal: pretty-printing ON
; - immediate: pretty-printing OFF
(define write-nekot
  (case-lambda [(nekot)
                (write-nekot 'normal nekot "")]
               [(mode nekot line)
                (let* ([name (nekot-name nekot)]
                       [context-obj (nekot-context nekot)]
                       [nekot-writer (match name
                                       ;normal nekots
                                       ['literal         add-literal]
                                       ['spaces          add-spaces]
                                       ['new-line        add-new-line]
                                       ['pp-directive    add-pp-directive]
                                       ;meta nekots
                                       ['empty           add-empty]
                                       ['concat          add-concatenated]
                                       ['immediate       add-immediate]
                                       ['speculative     add-speculative]
                                       ['position-indent change-indent-to-current]
                                       ;unknown nekot
                                       [_             (unknown-nekot-type name)])]
                       ;TODO: Determine if new-line should be indented in immediate mode
                       [new-line (if (equal? line "")
                                     (build-indentation context-obj)
                                     line)])
                  (nekot-writer mode (nekot-body nekot) context-obj new-line))]))
(provide (contract-out (write-nekot (case-> (-> nekot/c written-lines/c)
                                            (-> mode/c nekot/c written-line/c written-lines/c)))))
