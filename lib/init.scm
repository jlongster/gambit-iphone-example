;;; dependencies

(include "resource.scm")

;; (include "util/srfi-1.scm")
;; (include "ffi/gl.scm")
;; (include "graphics.scm")

(load (resource "srfi-1"))
(load (resource "gl"))
(load (resource "graphics"))

;;; environment

;; (current-output-port
;;  (open-file (list path: (resource "log")
;;                   append: #t
;;                   create: 'maybe
;;                   permissions: #o777)))

;;; c interface

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))

(c-define (c-init) () void "init" ""
  (init))
