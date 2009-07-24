;;;; "app3"
;;; renders a 3d box, which may bounce around some

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "obj-loader.scm")

;;;;; util

(define (obj->vertex-list obj)
  (list->vector
   (fold-right
    (lambda (el acc)
      (append (list (vector3d-z (triangle-v3 el))
                    (vector3d-y (triangle-v3 el))
                    (vector3d-x (triangle-v3 el))
                    (vector3d-z (triangle-v2 el))
                    (vector3d-y (triangle-v2 el))
                    (vector3d-x (triangle-v2 el))
                    (vector3d-z (triangle-v1 el))
                    (vector3d-y (triangle-v1 el))
                    (vector3d-x (triangle-v1 el)))
              acc))
    '()
    (obj-data-triangles obj))))


;;;;; box throwing

;; (define next-throw (get-random-throw))

;; (define (get-random-throw)
;;   ;; somewhere in the next second
;;   (+ (real-time) (random-real)))

;; (define (possibly-throw)
;;   (if (> (real-time) next-throw)
;;       (begin
;;         (throw)
;;         (set! next-throw (get-random-throw)))))

;; (define (throw)
;;   (let ((start (real-time)))
;;     (add-render-queue
;;      (lambda ()
;;        (define (height)
;;          (let ((timer (- (real-time) start)))
;;            5.))
;;        (glLoadIdentity)
;;        (glColor4f .2 .4 .5 1.)
;;        (glTranslatef 0. (height) -10.)
;;        ))))

;;;;; render queue


;;;;; app

(define box-mesh #f)
(define box-vertices #f)

(define (init)
  ;; opengl
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (perspective 60.
               (/ (UIView-width (current-view))
                  (UIView-height (current-view)))
               .1
               1000.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)

  (glDepthMask GL_TRUE)
  (glDisable GL_CULL_FACE)
  
  ;; meshes
  (set! box-mesh (obj-load (resource "box.obj")))
  (set! box-vertices (vector->GLfloat* (obj->vertex-list box-mesh))))

(define rot 0.)

(define (render)
  (glVertexPointer 3 GL_FLOAT 0 box-vertices)
  (glEnableClientState GL_VERTEX_ARRAY)

  (glColor4f .2 .4 .5 1.)
  (set! rot (+ rot 2.))

  (glLoadIdentity)
  (glTranslatef 0. 0. -5.)
  (glRotatef rot .5 .5 0.)
  (let loop ((i 0))
    (if (< i (length (obj-data-triangles box-mesh)))
        (begin
          (glDrawArrays GL_TRIANGLE_STRIP
                        (* i 3)
                        3)
          (loop (+ i 1))))))

(define (get-title)
  "Box from Blender. - Gambit Scheme")
