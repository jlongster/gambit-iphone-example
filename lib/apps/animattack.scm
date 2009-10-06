;;;; "app6"
;;; dynamic world

(include "../util/srfi-2.scm")
(include "../events#.scm")
(include "../obj-loader2.scm")
(include "../scene.scm")
(include "../physics.scm")
(include "../util-3d.scm")
(include "../texture.scm")
(include "../intersection.scm")

;;; resources

(define cow-mesh (obj-load (resource "cow")))
(define sheep-mesh (obj-load (resource "sheep")))

;;; controls

(define yaw 0.)
(define pitch 0.)

(define (reset-camera)
  (glLoadIdentity)
  (glRotatef (- (exact->inexact yaw)) 1. 0. 0.)
  (glRotatef (exact->inexact pitch) 0. 1. 0.))

(define %%touch-coords (make-table))

(define (record-touch touch)
  (table-set! %%touch-coords touch (UITouch-location touch)))

(define (update-touch/get-movement touch)
  (let* ((old-loc (table-ref %%touch-coords touch))
         (loc (UITouch-location touch))
         (x (- (car old-loc) (car loc)))
         (y (- (cdr old-loc) (cdr loc))))
    (record-touch touch)
    (cons x y)))

(define (remove-touch touch)
  (table-set! %%touch-coords touch))

(define %%intersection-queue '())

(define (queue-intersection x y)
  (set! %%intersection-queue
        (cons (list x y) %%intersection-queue)))

(define (dequeue-intersection)
  (if (null? %%intersection-queue)
      #f
      (let* ((lst (reverse %%intersection-queue))
             (res (car lst)))
        (set! %%intersection-queue (reverse (cdr lst)))
        res)))

(define (intersection-waiting?)
  (not (null? %%intersection-queue)))

(define (render-intersection-buffer)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (fov 40.)
         (aspect (/ width height)))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (perspective fov aspect 1. 1000.)
    (lookat (make-vec3d 0. 0. 0.)
            (make-vec3d 0. 0. 1.)
            (make-vec3d 0. 1. 0.))
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))
  
  (run-render-queue
   (map (lambda (el)
          (lambda ()
            (render-bounding-box el)))
        scene-list)))

(define-event-handler (touches-began touches event)
  (for-each (lambda (el)
              (let ((loc (UITouch-location el)))
                (queue-intersection (car loc) (cdr loc))))
            touches))

;;; util

(define (spread-number fl)
  (- (* fl 2.) 1.))

;;; glass cracking

(define %%crack-vertices #f)
(define %%crack-num-vertices 0)
(define %%crack-lines '())

(define (deviate n)
  (+ n (* (- (random-real) .5) 150.)))

(define (make-crack x y)
  (if %%crack-vertices
      (free %%crack-vertices))

  (set! %%crack-lines
        (append
         (list x y
               (deviate x) (deviate y)
               x y
               (deviate x) (deviate y)
               x y
               (deviate x) (deviate y)
               x y
               (deviate x) (deviate y))
         %%crack-lines))

  (set! %%crack-vertices (vector->float-array
                          (list->vector %%crack-lines)))
  (set! %%crack-num-vertices (length %%crack-lines)))

(define (render-cracks)
  (glLoadIdentity)
  
  (if %%crack-vertices
      (begin
        (glVertexPointer 2 GL_FLOAT 0 %%crack-vertices)
        (glEnableClientState GL_VERTEX_ARRAY)
        (glDisable GL_DEPTH_TEST)
        (glDisable GL_LIGHTING)
        (glColor4f 1. 1. 1. 1.)
        (glDrawArrays GL_LINES 0 %%crack-num-vertices)
        (glEnable GL_LIGHTING)
        (glEnable GL_DEPTH_TEST))))

;;; mouse picking

;; (define (find-entity-intersection x y z)
;;   (let loop ((tail %%entities))
;;     (if (null? tail)
;;         #f
;;         (let* ((obj (car tail))
;;                (mesh (scene-object-mesh obj))
;;                (bb (obj-bounding-box mesh)))
;;           (if (ray-box-intersection (bounding-box-min-x bb)
;;                                     (bounding-box-min-y bb)
;;                                     (bounding-box-min-z bb)
;;                                     (bounding-box-max-x bb)
;;                                     (bounding-box-max-y bb)
;;                                     (bounding-box-max-z bb)
;;                                     0. 0. 0.
;;                                     x y z)
;;               obj
;;               (loop (cdr tail)))))))

(define %%color-index 0)
(define %%color-map (make-vector 256 #f))

(define (get-next-color-index obj)
  ;; return a number in the range [1-255]
  (set! %%color-index (+ (remainder (+ %%color-index 1) 255) 1))
  (vector-set! %%color-map %%color-index obj)
  %%color-index)

(define (lookup-color-index index)
  (vector-ref %%color-map index))

(define (render-bounding-box obj)
  
  (define (quad v1 v2 v3 v4)
    (list v1 v2 v3 v1 v3 v4))

  (define (vec3d-list->vector . args)
    (let ((v (make-vector (* (length args) 3))))
      (let loop ((tail args)
                 (i 0))
        (if (null? tail)
            v
            (let ((vec (car tail)))
              (vector-set! v (* i 3) (vec3d-x vec))
              (vector-set! v (+ (* i 3) 1) (vec3d-y vec))
              (vector-set! v (+ (* i 3) 2) (vec3d-z vec))
              (loop (cdr tail) (+ i 1)))))))

  (let ((pos (scene-object-position obj))
        (rot (scene-object-rotation obj))
        (bbox (obj-bounding-box (scene-object-mesh obj))))

    (define-macro (bb attr)
      `(,(string->symbol
          (string-append "bounding-box-" (symbol->string attr)))
        bbox))

    (glLoadIdentity)
    
    (if pos
        (glTranslatef (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))  
    (if rot
        (glRotatef (vec4d-w rot)
                   (vec4d-x rot)
                   (vec4d-y rot)
                   (vec4d-z rot)))

    (glColor4f (exact->inexact (/ (scene-object-data obj) 255.)) 1. 1. 1.)
    
    (glVertexPointer
     3 GL_FLOAT 0
     (vector->float-array
      (apply vec3d-list->vector
             (append
              ;; Create counter-clockwise quads for each side of the
              ;; bounding box
              
              ;; min-x plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb min-z)))

              ;; max-x plane
              (quad (make-vec3d (bb max-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z)))

              ;; min-z plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb min-z)))

              ;; max-z plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z))                    
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z)))

              ;; min-y plane
              (quad (make-vec3d (bb min-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb min-z))
                    (make-vec3d (bb max-x) (bb min-y) (bb max-z))
                    (make-vec3d (bb min-x) (bb min-y) (bb max-z)))

              ;; max-y plane
              (quad (make-vec3d (bb min-x) (bb max-y) (bb min-z))
                    (make-vec3d (bb min-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb max-z))
                    (make-vec3d (bb max-x) (bb max-y) (bb min-z)))))))
    (glEnableClientState GL_VERTEX_ARRAY)
    (glDisable GL_LIGHTING)
    (glDrawArrays GL_TRIANGLES 0 36)
    (glEnable GL_LIGHTING)))

;;; app

;; x, y, and z should be in world coords relative to the camera
(define (unproject x y z)
  (let* ((screen-x (/ x z))
         (screen-y (/ y z))
         (width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x-max (perspective-xmax pers))
         (y-max (perspective-ymax pers))
         (x-min (perspective-xmin pers))
         (y-min (perspective-ymin pers))
         (x-width (- x-max x-min))
         (y-width (- y-max y-min)))
    (list
     (* (/ (- x-max screen-x) x-width)
        width)
     (* (/ (- y-max screen-y) y-width)
        height))))

;; x and y should be in screen coords [0, width) and [0, height)
(define (project x y)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x-max (perspective-xmax pers))
         (y-max (perspective-ymax pers))
         (x-min (perspective-xmin pers))
         (y-min (perspective-ymin pers))
         (x-width (- x-max x-min))
         (y-width (- y-max y-min)))
    (list
     (- x-max (* (/ x width) x-width))
     (- y-max (* (/ y height) y-width))
     (perspective-znear pers))))

(define SCREEN 10.)

(define (global-update el)
  (update-physics el)
  (let ((pos (scene-object-position el)))
    (if (< (vec3d-z pos) SCREEN)
        (begin
          (impact el)
          (if (not (no-more-life?))
              (begin
                (vec3d-z-set! pos SCREEN)
                (apply make-crack
                       (unproject (vec3d-x pos) (vec3d-y pos) (vec3d-z pos)))
                (scene-object-velocity-set! el (make-vec3d 0. 0. 0.))
                (scene-object-acceleration-set! el (make-vec3d 0. -10. 0.))))))))

(define (%%get-random-time)
  (+ (real-time) (* (random-real) 3.)))

(define (%%get-random-mesh)
  (let ((x (random-integer 100)))
    (cond
     ((< x 50) cow-mesh)
     ((>= x 50) sheep-mesh))))

(define %%next-time (%%get-random-time))
(define %%entities '())

(define (possibly-make-entity)
  (if (> (real-time) %%next-time)
      (let ((entity (make-entity)))
        (scene-list-add entity)
        (set! %%entities (cons entity %%entities))
        (set! %%next-time (%%get-random-time)))))

(define (make-entity)
  (let* ((pos (make-vec3d
               (* (spread-number (random-real)) 7.) -28. 40.))
         (to-eye (vec3d-unit (vec3d-sub (make-vec3d 0. 0. 0.)
                                        pos)))
         (x (* (spread-number (random-real)) 3.16))
         (thrust (+ 15. (* x (abs x))))
         (vel (make-vec3d (* (vec3d-x to-eye) thrust)
                          (+ 25.5 (spread-number (random-real)))
                          (* (vec3d-z to-eye) thrust))))
    (let ((obj (make-scene-object
                (%%get-random-mesh)
                #f
                pos
                (make-vec4d (random-real)
                            (random-real)
                            0.
                            230.)
                1.5
                vel
                #f
                (let ((speed (* (random-real) 10.)))
                  (lambda (this)
                    (scene-object-rotation-set!
                     this
                     (vec4d-add (scene-object-rotation this)
                                (make-vec4d 0. 0. 0. speed)))
                    (let* ((pos (scene-object-position this))
                           (screen-y (cadr (unproject (vec3d-x pos)
                                                      (vec3d-y pos)
                                                      (vec3d-z pos))))
                           (screen-height (UIView-height (current-view))))
                      (if (> screen-y (+ screen-height 100))
                          #f
                          this)))))))
      (scene-object-data-set! obj (get-next-color-index obj))
      obj)))

;; life

(define %%entity-forces
  `((,cow-mesh 2)
    (,sheep-mesh 1)))

(define (entity-force el)
  (let ((mesh (scene-object-mesh el)))
    (and-let* ((x (assq mesh %%entity-forces)))
      (cadr x))))

(define %%life 100)

(define (impact el)
  (let ((f (entity-force el)))
    (set! %%life (- %%life f))))

(define (no-more-life?)
  (<= %%life 0))

;; engine

(define background-texture #f)

(define (init)
  (random-source-randomize! default-random-source)

  (glEnable GL_DEPTH_TEST)
  (glEnable GL_CULL_FACE)
  (glCullFace GL_BACK)
  (glShadeModel GL_SMOOTH)

  (glEnable GL_RESCALE_NORMAL)
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glLightf GL_LIGHT0 GL_LINEAR_ATTENUATION 0.01)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->float-array (vector 1. 1. 1. 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->float-array (vector 5. 5. 2. 1.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))

  (set! GRAVITY (make-vec3d 0. -11. 0.))
  
  (let ((image (LodePNG-decode32f (resource "sky2.pnx"))))
    (set! background-texture (image-opengl-upload
                              (LodeImage-data image)
                              (LodeImage-width image)
                              (LodeImage-height image)))))

(define (run)
  (possibly-make-entity)
  (scene-list-update global-update))

(define (render)
  (if (intersection-waiting?)
      (begin
        (render-intersection-buffer)
        (let loop ()
          (let ((loc (dequeue-intersection)))
            (if loc
                (let ((buf (make-unsigned-int8-array 4))
                      (height (UIView-height (current-view))))
                  (glReadPixels (car loc) (- height (cadr loc)) 1 1
                                GL_RGBA GL_UNSIGNED_BYTE
                                (->void-array buf))
                  (let ((obj (lookup-color-index (unsigned-int8-array-ref buf 0))))
                    (set! scene-list
                          (fold (lambda (el acc)
                                  (if (eq? el obj)
                                      acc
                                      (cons el acc)))
                                '()
                                scene-list)))
                  (loop)))))
        (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))))

  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (let ((width (UIView-width (current-view)))
        (height (UIView-height (current-view))))
    
    ;; 2d
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glOrthof 0.0 1.0 1.0 0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (image-render background-texture)

    ;; ;; 3d
    (let* ((fov 40.)
           (aspect (/ width height)))
      (glMatrixMode GL_PROJECTION)
      (glLoadIdentity)
      (perspective fov aspect 1. 1000.)
      (lookat (make-vec3d 0. 0. 0.)
              (make-vec3d 0. 0. 1.)
              (make-vec3d 0. 1. 0.))
      (glMatrixMode GL_MODELVIEW)
      (glLoadIdentity))

    (run)
    ;(render-grid 100.)
    (run-render-queue (scene-list->render-queue))
    
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glOrthof 0.0
              (exact->inexact width)
              (exact->inexact height)
              0.0 -1.0 1.0)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (if (not (no-more-life?))
        (render-cracks))
    )
  (##gc))

(define (get-title)
  "")
