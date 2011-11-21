#lang racket

(require "fulmar-core.rkt")
(require "chunk-core.rkt") ; for eval
;(require "chunk-standard.rkt") ; for eval

;input/output structures, procedures, and contracts for fulmar

;;;;;;;;;;;;;;
;;;Output;;;;;
;;;;;;;;;;;;;;

;save a backup
; Overwrites old backup file (if exists) without backing up the backup...
(define/contract (fulmar-backup-file location)
  (-> path? void?)
  (if (file-exists? location)
      (rename-file-or-directory location
                                (string->path (string-append (path->string location) "~r"))
                                #true)
      (void)))
;(provide fulmar-back-up-file)

;Open output port
(define/contract (open-output! location)
  (-> path? output-port?)
  (begin
    (fulmar-backup-file location)
    (open-output-file location #:exists 'replace)))
;(provide open-output!)

;Close output port
(define (close-output! port)
  (-> output-port? void?)
  (close-output-port port))
;(provide close-output!)

;Print written code with port
(define/contract (print-code-with-port! lines port)
  (-> written-lines/c output-port? void?)
  (if (empty? lines)
      (void)
      (begin
        (print-code-with-port! (cdr lines) port)
        (displayln (car lines) port))))
;(provide print-code-with-port!)

;Print written code for a file to a file
(define/contract (print-file! lines port/location)
  (-> written-lines/c (or/c output-port? path?) void?)
  (if (output-port? port/location)
      (print-code-with-port! lines port/location)
      ;(path? port/location)
      (let ([port (open-output! port/location)])
        (print-file! lines port)
        (close-output! port))))
(provide print-file!)

;;;;;;;;;;;;;;
;;;Input;;;;;;
;;;;;;;;;;;;;;

;open input port
(define/contract (open-input location)
  (-> path? input-port?)
  (if (not (file-exists? location))
      (error "Could not open file at: " location)
      (open-input-file location)))
;(test-case
; "Test open-input - does not cover all cases"
; (check-exn exn:fail? (λ () (open-input "none/none/asdf.ext"))))
;(provide open-input)

;close input port
(define/contract (close-input port)
  (-> input-port? void?)
  (close-input-port port))
;(provide close-input)

;TODO: change this namespace (and/or namespace-anchor) to only include core and standard chunks
;defintions needed for eval in read-chunks
(define-namespace-anchor fulmar-chunk-namespace-anchor)
;(provide fulmar-namespace-anchor)
(define fulmar-chunk-namespace (namespace-anchor->namespace fulmar-chunk-namespace-anchor))
;(provide fulmar-namespace)

;read chunks
(define/contract (read-chunks port/location)
  (-> (or/c input-port? path?) (listof chunk/c))
  (if (input-port? port/location)
      (let ([current (read port/location)])
        (if (eof-object? current)
            '()
            (cons (eval current fulmar-chunk-namespace)
                  (read-chunks port/location))))
      ;(path? port/location)
      (let* ([port (open-input port/location)]
             [chunks (read-chunks port)])
        (close-input port)
        chunks)))
;(provide read-chunks)

;read file
(define/contract (read-chunk port/location)
  (-> (or/c input-port? path?) chunk/c)
  (apply top-list-chunk (read-chunks port/location)))
(provide read-chunk)
