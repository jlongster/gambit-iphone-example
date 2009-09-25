#!/usr/bin/env gsi-script

(include "../lib/util/srfi-1.scm")
(include "../lib/vectors.scm")
(include "../lib/obj-loader2.scm")

(define (main filename)
  (compress (string-append filename ".obj.gso") (obj-load filename #f #t)))
