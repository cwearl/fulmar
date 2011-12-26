#lang racket

(require "fulmar-core.rkt")
(require "chunk-standard.rkt") ; for eval

;input/output structures, procedures, and contracts for fulmar

;Open output port
; - saves a simple backup
(define/contract (open-output! location)
  (-> path? output-port?)
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
        (close-output-port port))))
(provide print-file!)

;open input port
(define/contract (open-input location)
  (-> path? input-port?)
  (if (not (file-exists? location))
      (error "Could not open file at: " location)
      (open-input-file location)))

;TODO: change this namespace (and/or namespace-anchor) to only include standard chunks
;defintions needed for eval in read-chunks
(define-namespace-anchor fulmar-chunk-namespace-anchor)
;(provide fulmar-namespace-anchor)
(define fulmar-chunk-namespace (namespace-anchor->namespace fulmar-chunk-namespace-anchor))
;(provide fulmar-namespace)

;read chunk
(define/contract (read-chunk port/location)
  (-> (or/c input-port? path?) chunk/c)
  (if (input-port? port/location)
      (eval (read port/location) fulmar-chunk-namespace)
      ;(path? port/location)
      (let* ([port (open-input port/location)]
             [chunk (read-chunk port)])
        (close-input-port port)
        chunk)))
(provide read-chunk)
