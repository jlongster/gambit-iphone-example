;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

;;;; debugger

;; (include "util/remote-debugger/debuggee.scm")
;; (rdi-set-host! "localhost:20000")

;; (thread-start!
;;  (make-thread
;;   (lambda () (##repl-debug-main))))

;;;; dependencies

(include "resource.scm")
(include "util/srfi-1.scm")

(include "ffi/gl.scm")
(include "ffi/gl-util.scm")
(include "ffi/osx.scm")
(include "ffi/iphone.scm")
(include "events.scm")

(include "app3.scm")
;; (load (resource "lib/app3"))

;; ;;;; c interface

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
