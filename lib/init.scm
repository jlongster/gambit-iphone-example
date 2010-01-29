;;;; "init"
;;; Pulls together all of the modules for our system and provides a C
;;; interface for the outside world.

;;;; debugger

(include "util/remote-debugger/debuggee.scm")
(rdi-set-host! "localhost:20000")

(thread-start!
 (make-thread
  (lambda () (##repl-debug-main))))

;;;; dependencies

(include "resource.scm")
(include "util/srfi-1.scm")
(include "ffi/ffi.scm")
(include "ffi/gl.scm")
(include "ffi/gl-util.scm")
(include "ffi/osx.scm")
(include "ffi/iphone.scm")
(include "ffi/image.scm")
(include "vectors.scm")
(include "events.scm")

(load (local-resource "lib/apps/app5"))
;;(include "apps/app5.scm")

;; ;;;; c interface

(c-define (c-init) () void "init" ""
  (init))

(c-define (c-render) () void "render" ""
  (render))

(c-define (c-get-title) () char-string "get_title" ""
  (get-title))
