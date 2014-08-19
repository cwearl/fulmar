#lang racket

(provide document)

(define-syntax-rule (document . _)
  (void))

(document document
"The document macro is intended to be useful to the doc-scraper and not
racket itself. If you don't have the doc-scraper, don't bother with it."

"Its purpose is to give the doc-scraper's reader something to look for when
scraping for documentation, while getting completely out of the way when it
comes time to run the code."

"To use it for your own documentation, the first argument should be the name
of whatever it is you're documenting. The rest of the arguments should be
whatever you want to show up in the paragraphs of the documented thing.
One argument per paragraph.")
