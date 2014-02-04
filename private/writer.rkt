#lang racket

(require "fulmar-core.rkt")
(require "core-chunk.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;give n spaces
(define (make-whitespace n)
  (make-string n #\space))

;remove whitespace from the end of a line
(define (remove-whitespace line)
  (or (let ((length (string-length line)))
        (for/first ([i (in-range (- length 1) -1 -1)]
                    #:when (not (equal? #\space (string-ref line i))))
          (substring line 0 (+ i 1))
          )) 
      ""))

;checks if given string is just spaces
(define (is-whitespace? line)
  (zero? (string-length (remove-whitespace line))))

;build indentation for new line given current context
(define (build-indentation context)
  (match (environment-description (context-env context))
    ['empty
     (make-whitespace (context-indent context))]
    [_ 
     (string-append 
      (make-whitespace (context-initial-position context))
      "/* "
      (let ([remaining (- (context-indent context)
                          (context-initial-position context))])
        (make-whitespace (max 0 remaining))))]))

;finish line
(define (finish-line given-line context)
  (define line (remove-whitespace given-line))
  (if (equal? line (remove-whitespace (build-indentation context)))
      ""
      (match (environment-description (context-env context))
        ['empty line]
        ['comment (string-append line " */")])))

;check speculative line
(define (check-speculative-line-length first-part second-part context)
  (<= (+ (string-length first-part)
         (string-length second-part))
      (context-line-length context)))

;add '#' to proper place in given line
(define (add-hash-character line)
  (cond [(= 0 (string-length line)) "#"]
        [(is-whitespace? line)
         (string-append "#"
                        (substring line 1))]
        [else
         (string-append (substring line 0 (- (string-length line) 1))
                        "#")]))

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
    ['immediate (list (string-append line string))]))

;add spaces to current line
(define (add-spaces mode count context line)
  (define whitespace (make-whitespace count))
  (match mode
    ['normal (cond [(= 0 count)
                    (list line)]
                   [(check-speculative-line-length whitespace line context)
                    (list (string-append line whitespace))]
                   [else
                    (list ""
                          (finish-line line context))])]
    ['immediate (list (string-append line whitespace))]))

;add new line
(define (add-new-line mode body context line)
  (list ""
        (finish-line line context)))

;add preprocessor directive
(define (add-pp-directive mode body context line)
  (list (add-hash-character line)))

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

;add concatenated nekots
(define (add-concatenated mode nekots context line)
  (for/fold ([lines (list line)]) ([nekot (in-list nekots)])
    (append (write-nekot mode nekot (car lines))
            (cdr lines))))

;add nekot immediately
(define (add-immediate mode nekot context line)
  (write-nekot 'immediate nekot line))

;add nekot(s) speculatively
(define (add-speculative mode body context line)
  (match-let* ([(list attempt success? backup) body]
               [attempted (write-nekot mode attempt line)])
    (if (success? attempted)
        attempted
        (write-nekot mode backup line))))

;change indent to length of current line
(define (change-indent-to-current mode chunk context line)
  (let* ([new-indent (max 0 (- (string-length line)
                               (context-indent context)))]
         [new-context (reindent new-indent context)]
         [transformed-chunk (chunk-transform chunk new-context)])
    (write-nekot mode transformed-chunk line)))

;error nekot...
(define (unknown-nekot-type name)
  (λ (mode body context line)
    (error "Unrecognized nekot/chunk; given: " name mode body context line)))

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
