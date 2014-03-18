#lang racket

(require racket/sandbox)
(require fulmar/version)

(require "verify.rkt")

(module+ main
  (define root-dir
    (normalize-path
     (command-line
      #:program "apply"
      #:usage-help "apply fulmar to all .fmr files in a directory (recursing through non-hidden subdirectories) and write the output to corresponding .h and .cpp files."
      #:args (filename)
      filename)))
  
  (define fmr-files (map ((curry build-path) root-dir) (find-fmr root-dir)))
  (define top-fmr-files (filter find-generated fmr-files))
  
  (for ([fmr top-fmr-files])
    (let* ((relative-fmr-string (path->string (find-relative-path root-dir fmr)))
           (out-file (find-generated fmr))
           (out (open-output-file out-file #:exists 'truncate))
           (output (evaluate-fmr fmr fmr-files)))
      (displayln relative-fmr-string)
      (display output out)
      (close-output-port out))))
