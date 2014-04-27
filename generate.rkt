#lang racket

(require "private/generate.rkt")

(module+ main
  (define root-dir
    (normalize-path
     (command-line
      #:program "generate"
      #:usage-help "apply fulmar to all .fmr files in a directory (recursing through non-hidden subdirectories) and replacing the output in the corresponding .h and .cpp files if different."
      #:args (filename)
      filename)))
  
  (define fmr-files (map ((curry build-path) root-dir) (find-fmr root-dir)))
  (define top-fmr-files (filter find-generated fmr-files))
  
  (for ([fmr top-fmr-files])
    (let* ((relative-fmr-string (path->string (find-relative-path root-dir fmr)))
           (comparison-file (find-generated fmr))
           (comparison (file->string comparison-file))
           (output (evaluate-fmr fmr fmr-files))
           (matched (equal? output comparison)))
      (if matched
          (displayln (format "~a: ~a" "matched  " relative-fmr-string))
          (begin (if (file-exists? comparison-file)
                     (rename-file-or-directory comparison-file
                                               (string->path (string-append (path->string comparison-file)
                                                                            "~r~"))
                                               #t)
                     (void))
                 (display-to-file output comparison-file)
                 (displayln (format "~a: ~a" "generated" relative-fmr-string)))))))
