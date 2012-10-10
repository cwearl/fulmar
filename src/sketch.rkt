#lang racket

(struct group-struct (IR) #:transparent)
(struct concat-struct (IR) #:transparent)
(struct pivot-struct (IR length) #:transparent)
(struct format-struct (IR change) #:transparent)