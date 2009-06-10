
;; (define (stack-to-string)
;;   (##continuation-capture
;;    (lambda (k)
;;      (call-with-output-string ""
;;                               (lambda (port)
;;                                 (##cmd-b k port 10 #t))))))

;; (current-exception-handler
;;  (lambda (ex)
;;    (display (stack-to-string))
;;    (exit)))

(include "resource.scm")
(load (resource "lib/ffi/gl"))
(load (resource "lib/graphics"))

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
