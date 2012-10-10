#lang racket

(require "basics.rkt")
(require "environment.rkt")
(require "context.rkt")
(require "chunk.rkt")
(require "sequence.rkt")
(require "line.rkt")
(require "block.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;
;;helper functions;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;give n spaces
(define/contract (make-whitespace n)
  (-> natural-number/c string?)
  (make-string n #\ ))
(provide make-whitespace)

;remove whitespace from the end of a line
(define/contract (remove-whitespace line)
  (-> line? line?)
  (list->string (reverse (second (foldl (λ (char result) (if (equal? char #\ )
                                                             (list (cons #\  (first result))
                                                                   (second result))
                                                             (list null
                                                                   (flatten* char
                                                                             (first result)
                                                                             (second result)))))
                                        (list null null)
                                        (string->list line))))))
(provide remove-whitespace)

;build indentation for new line given current context
(define/contract (build-indentation context)
  (-> context? line?)
  (if ((or? empty-env? macro-env?) (context-env context))
      (line (context-indent context))
      (line (context-comment-indent context)
            #\/ #\* #\ 
            (max 0 (- (context-indent context)
                      (context-comment-indent context))))))
(provide build-indentation)

;finish line
;(define/contract (finish-line 
(define/contract (finish-line given-line context)
  (-> line? context? line?)
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
(provide finish-line)

;add '#' to proper place in given line
(define/contract (add-hash-character line)
  (-> line? line?)
  (add-to-first #\# line))
(provide add-hash-character)

;;;;;;;;;;;;;;;;;;;;;;;;
;;nekot handlers;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;convert literal chunk into a sequence
(define/contract (literal->seq g)
  (-> chunk-literal? seq-output?)
  (cond [(string? g)
         (literal->seq (string->list g))]
        [(symbol? g)
         (literal->seq (symbol->string g))]
        [((or? char?
               indent?) g)
         g]
        [(list? g)
         (seq (foldl literal->seq null g))]
        [else
         (error "Unrecognized literal chunk; unrecognized: " g)]))
(provide literal->seq)

;abut 

;generate a literal string to current line
(define/contract (generate-literal ch co bl)
  (-> chunk-literal? context? block? block?)
  (let ([item (literal->seq ch)]
        [line (block-last bl)])
    (block (cond [(context-divide? co)
                  (line line item)]
                 [(context-abut? co)
                  (seq-with-last line item)]
                 [else
                  (error "Unrecognized context mode; given: " co "; given: " ch bl)])
           (block-rest bl))))
(provide generate-literal)

;generate new line
(define/contract (generate-new-line co bl)
  (-> context? block? block?)
  (block bl
         (build-indentation co)))
(provide generate-new-line)

;generate preprocessor directive
(define/contract (generate-pp-directive co bl)
  (-> context? block? block?)
  (block (block-rest bl)
         (add-hash-character (block-last bl))))
(provide generate-pp-directive)

;generate concatenate chunk
(define/contract (generate-concat chs co bl)
  (-> chunk-list? context? block? block?)
  (let ([chunks (flatten* chs)]
        [divide-co (to-divide-mode co)])
    (if ((or? context-abut? context-divide?) co)
        (for/fold ([block (generate-block (first chunks) co bl)])
                  ([ch (in-list (rest chunks))])
          (generate-block ch divide-co block))
        (error "Unrecognized context mode; unrecognized: " co "; given: " chs bl))))
(provide generate-concat)

;generate no line chunk
(define/contract (generate-no-line chs co bl)
  (-> chunk-list? context? block? block?)
  (let ([chunks (flatten* chs)]
        [abut-co (to-abut-mode co)])
    (if ((or? context-abut? context-divide?) co)
        (for/fold ([block (generate-block (first chunks) co bl)])
                  ([ch (in-list (rest chunks))])
          (generate-block ch abut-co block))
        (error "Unrecognized context mode; unrecognized: " co "; given: " chs bl))))
(provide generate-no-line)

;generate indent chunk
(define/contract (generate-indent body co bl)
  (-> indent-chunk-body? context? block? block?)
  (generate-block (second body)
                  (increase-indent (first body) co)
                  bl))
(provide generate-indent)

;generate comment chunk
(define/contract (generate-comment ch co bl)
  (-> chunk? context? block? block?)
  (generate-block ch
                  (enter-comment-env co)
                  bl))
(provide generate-comment)

;generate macro chunk
(define/contract (generate-macro ch co bl)
  (-> any/c context? block? block?)
  (generate-block ch
                  (enter-macro-env co)
                  bl))
(provide generate-macro)

;;;;;;;;;;;;;;;;;;;;;;;;
;;generate block;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;generate block
(define/contract generate-block
  (case-> (-> chunk? context? block?)
          (-> chunk? context? block? block?))
  (case-lambda [(ch co)
                (generate-block ch co (block))]
               [(ch co bl)
                (match (chunk-name ch)
                  ['literal      (generate-literal      (chunk-body ch) co bl)]
                  ['new-line     (generate-new-line                     co bl)]
                  ['pp-directive (generate-pp-directive                 co bl)]
                  ['concat       (generate-concat       (chunk-body ch) co bl)]
                  ['no-line      (generate-no-line      (chunk-body ch) co bl)]
                  ['indent       (generate-indent       (chunk-body ch) co bl)]
                  ['comment      (generate-comment      (chunk-body ch) co bl)]
                  ['macro        (generate-macro        (chunk-body ch) co bl)]
                  [_             (error "Given unrecognized chunk; given: " ch "; context: " co)])]))
(provide generate-block)
