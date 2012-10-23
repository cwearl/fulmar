#lang racket

(require "fulmar-core.rkt")
(require "standard-chunk.rkt") ; for eval

;input/output structures, procedures, and contracts for fulmar

;Open output port
; - saves a simple backup
(define (open-output! location)
  (begin
    ; Overwrites old backup file (if exists) without backing up the backup...
    (if (file-exists? location)
        (rename-file-or-directory location
                                  (string->path (string-append (path->string location) "~r~"))
                                  #true)
        (void))
    (open-output-file location #:exists 'replace)))
;(provide open-output!)

;Print written code with port
(define (print-code-with-port! lines port)
  (if (empty? lines)
      (void)
      (begin
        (print-code-with-port! (cdr lines) port)
        (displayln (car lines) port))))
;(provide print-code-with-port!)

;Print written code for a file to a file
(define (print-file! lines port/location)
  (if (output-port? port/location)
      (print-code-with-port! lines port/location)
      ;(path? port/location)
      (let ([port (open-output! port/location)])
        (print-file! lines port)
        (close-output-port port))))
(provide print-file!)

;open input port
(define (open-input location)
  (if (not (file-exists? location))
      (error "Could not open file at: " location)
      (open-input-file location)))
;(provide open-input)

;parse file containing singleton
(define (read-singleton port/location)
  (if (input-port? port/location)
      (read port/location)
      (let* ([port (open-input port/location)]
             [singleton (read-singleton port)])
        (close-input-port port)
        singleton)))
;(provide read-singleton)

;TODO: change this namespace (and/or namespace-anchor) to only include standard chunks
;defintions needed for eval in read-chunks
(define-namespace-anchor fulmar-chunk-namespace-anchor)
;(provide fulmar-namespace-anchor)
(define fulmar-chunk-namespace (namespace-anchor->namespace fulmar-chunk-namespace-anchor))
;(provide fulmar-namespace)

;read chunk
(define (read-chunk port/location)
  (eval (read-singleton port/location)
        fulmar-chunk-namespace))
(provide read-chunk)
