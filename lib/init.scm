;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

;;;; dependencies

(include "resource.scm")

(include "util/srfi-1.scm")
(include "ffi/gl.scm")
(include "graphics.scm")

;; (load (resource "lib/util/srfi-1"))
;; (load (resource "lib/ffi/gl"))
;; (load (resource "lib/graphics"))

;;;; c interface

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
