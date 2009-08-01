;;;; "app3"
;;; renders a 3d box, which may bounce around some

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "obj-loader.scm")

;;;;; util

(define-type optimized-mesh
  constructor: really-make-optimized-mesh
  obj
  vertex-list)

(define (make-optimized-mesh obj)
  (really-make-optimized-mesh
   obj
   (vector->GLfloat* (obj->vertex-list obj))))

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

;;;;; box

(define box-mesh #f)

(define (load-box)
  ;; meshes
  (set! box-mesh (make-optimized-mesh
                  (obj-load (resource "resources/medium.obj")))))

(define (upload-box)
  (glVertexPointer 3 GL_FLOAT 0 box-vertices)
  (glEnableClientState GL_VERTEX_ARRAY))

(define (render-box)
  (glDrawArrays GL_TRIANGLES
                0
                (* (length (obj-data-triangles box-mesh)) 3)))

;;;;; box throwing

(define (get-random-throw)
  (+ (real-time) (random-real) 1.5))

(define next-throw (get-random-throw))

(define (possibly-throw)
  (if (> (real-time) next-throw)
      (begin
        (throw)
        (set! next-throw (get-random-throw)))))

(define (throw)
  (scene-list-add
   (make-scene-object box-mesh
                      (make-vec3d 0. -5. 25.)
                      ;; (make-vec4d 0. 1. 0. -90.)
                      #f
                      (make-vec3d 0. 20. -10.))))

;;;;; scene object, physics

(define-type scene-object
  constructor: really-make-scene-object
  mesh
  position
  rotation
  velocity
  acceleration
  callback
  %%last-update)

(define (make-scene-object mesh pos rot #!optional vel accel update)
  (really-make-scene-object mesh pos rot vel accel
                            (or update (lambda (el) #t))
                            #f))

(define (scene-object-update obj)
  (let* ((now (real-time))
         (last (or (scene-object-%%last-update obj) now))
         (change (- now last)))
    (scheme-object-apply-acceleration obj change)
    (scheme-object-apply-velocity obj change)
    (scene-object-%%last-update-set! obj now)))

(define (scheme-object-apply-acceleration obj change)
  (let ((velocity (scene-object-velocity obj)))
    (if velocity
        (let* ((acceleration (global-acceleration obj))
               (change (vec3d-scalar-mul acceleration change)))
          (scene-object-velocity-set!
           obj
           (vec3d-add (scene-object-velocity obj)
                      change))))))

(define (scheme-object-apply-velocity obj change)
  (let ((velocity (scene-object-velocity obj)))
    (if velocity
        (let ((change (vec3d-scalar-mul velocity change)))
          (scene-object-position-set!
           obj
           (vec3d-add (scene-object-position obj)
                      change))))))

(define GRAVITY (make-vec3d 0. -20.8 0.))

(define (global-acceleration obj)
  ;; apply gravity
  (let ((accel (scene-object-acceleration obj)))
    (if accel
        (vec3d-add accel GRAVITY)
        GRAVITY)))

;;;;; scene list

(define scene-list '())

(define (scene-list-add obj)
  (set! scene-list (cons obj scene-list)))

(define (scene-list-update)
  (fold (lambda (el acc)
          (if ((scene-object-callback el) el)
              (begin
                (scene-object-update el)
                (cons el acc))
              acc))
        '()
        scene-list))

(define (scene-list->render-queue)
  (map (lambda (el)
         (lambda ()
           (let ((mesh (scene-object-mesh el))
                 (pos (scene-object-position el))
                 (rot (scene-object-rotation el))
                 (vel (scene-object-velocity el)))
             (glVertexPointer 3 GL_FLOAT 0 (optimized-mesh-vertex-list mesh))
             (glEnableClientState GL_VERTEX_ARRAY)
             (glLoadIdentity)
             
             (if pos
                 (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))

             (if rot
                 (glRotatef (vec4d-w rot)
                            (vec4d-x rot)
                            (vec4d-y rot)
                            (vec4d-z rot)))

             (glDrawArrays GL_TRIANGLES
                           0
                           (* (length (obj-data-triangles
                                       (optimized-mesh-obj mesh))) 3)))))
       scene-list))

;;;;; render queue

(define (run-render-queue rq)
  (for-each (lambda (el) (el)) rq))

;;;;; app

(define (init)
  ;; opengl
  (let* ((fov 40.)
         (aspect (/ (UIView-width (current-view))
                    (UIView-height (current-view))))
         (half-fov-y (/ fov aspect 2)))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (perspective fov aspect .1 1000.)
    (lookat (make-vec3d 0. 0. 0.)
            ;; (make-vec3d 0. (sin half-fov-y) (cos half-fov-y))
            (make-vec3d 0. 0. 1.)
            (make-vec3d 0. 1. 0.))
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))

  (glDepthMask GL_TRUE)
  (glDisable GL_CULL_FACE)

  (load-box))

(define (run)
  (possibly-throw))

(define (render)
  (run)
  (scene-list-update)
  (run-render-queue (scene-list->render-queue))
  (##gc))

(define (get-title)
  "This is Medium. - Gambit Scheme")
