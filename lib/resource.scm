;;;; "resources"
;;; This file implements two ways of finding resources: one calls into
;;; Cocoa to find the resource path of the current bundle, and the
;;; other depends on "config.scm" to define a variable named "root".
;;;
;;; The former seems more scalable, but it requires up to deploy all of
;;; our resources like Xcode, which is annoying, so we choose the
;;; latter.

(c-declare "#import <UIKit/UIKit.h>")

(include "config.scm")

;; Unused for now, but it seems like this might be useful later.
(define get-resource-path
  (c-lambda () char-string #<<end-c-code
   NSString *path = [[NSBundle mainBundle] resourcePath];
   char* buf = malloc(1024*sizeof(char));
   [path getCString:buf maxLength:1024 encoding:NSASCIIStringEncoding];
   ___result = buf;
end-c-code
))

(define (resource path)
  (string-append root "/" path))
