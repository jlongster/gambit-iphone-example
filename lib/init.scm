;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

;;;; debugger

;; (include "util/remote-debugger/debuggee.scm")
;; (rdi-set-host! "localhost:20000")

;; (thread-start!
;;  (make-thread
;;   (lambda () (##repl-debug-main))))

;; (thread-sleep! 1)

;;;; dependencies

(include "resource.scm")
(include "util/srfi-1.scm")

(include "ffi/gl.scm")
(include "ffi/osx.scm")
(include "ffi/iphone.scm")

(include "app2.scm")
;; (load (resource "lib/app2"))

;;;; c interface

;; (define (init)
;;   (+ 5 5))

;; (define (render)
;;   (+ 4 4))

;; (define (get-title)
;;   "test")

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
