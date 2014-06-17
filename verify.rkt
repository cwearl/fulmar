#lang racket

(require "private/generate.rkt")

(module+ main
  (define root-dir
    (normalize-path
     (command-line
      #:program "verify"
      #:usage-help "apply fulmar to all .fmr files in a directory (recursing through non-hidden subdirectories) and compare the output to corresponding .h and .cpp files for testing."
      #:args (filename)
      filename)))
  
  (define fmr-files (map ((curry build-path) root-dir) (find-fmr root-dir)))
  (define top-fmr-files (filter find-generated fmr-files))
  
  (define matched
    (for/and ([fmr top-fmr-files])
      (let* ((relative-fmr-string (path->string (find-relative-path root-dir fmr)))
             (comparison-file (find-generated fmr))
             (comparison (file->string comparison-file))
             (output (evaluate-fmr fmr fmr-files))
             (matched (equal? output comparison)))
        (displayln (format "~a: ~a" (if matched "matched" "FAILED") relative-fmr-string))
        matched)))
  (exit (if matched
            0
            1)))
