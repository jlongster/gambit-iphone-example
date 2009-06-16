
;;; The C function GET_PATH is provided by the application code,
;;; in app/cocoa-ffi.m last time I checked.

(c-declare "#import <UIKit/UIKit.h>")

(define get-resource-path
  (c-lambda () char-string #<<end-c-code
   NSString *path = [[NSBundle mainBundle] resourcePath];
   char* buf = malloc(1024*sizeof(char));
   [path getCString:buf maxLength:1024 encoding:NSASCIIStringEncoding];
   ___result = buf;
end-c-code
))

(define (resource path)
  (string-append (get-resource-path) "/" path))
