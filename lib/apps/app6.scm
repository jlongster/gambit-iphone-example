;;;; "app6"
;;; dynamic world

(include "../events#.scm")
(include "../obj-loader2.scm")
(include "../scene.scm")
(include "../physics.scm")

;;; resources

(define sphere-mesh (obj-load (resource "resources/sphere") #t))
(define jlongster-mesh (obj-load (resource "resources/jlongster") #t))

(define line (vector->GLfloat* (vector 0. 0. 0.
                                       1. 0. 0.)))

(define grid-size 100.)

(define (render-grid)
  (define (draw)
    (glScalef grid-size 0. 0.)
    (glDrawArrays GL_LINES 0 2))

  (define offset (/ grid-size 2.))
  
  (define (render-lines)
    (unfold (lambda (i) (> i grid-size))
            (lambda (i)
              (reset-camera)
              ;;(glRotatef 45. 1. 0. 0.)
              (glTranslatef (- offset) -5. (- (exact->inexact i) offset))
              (draw))
            (lambda (i) (+ i 1))
            0)

    (unfold (lambda (i) (> i grid-size))
            (lambda (i)
              (reset-camera)
              ;;(glRotatef 45. 0. 0. 0.)
              (glTranslatef (- (exact->inexact i) offset) -5. (- offset))
              (glRotatef -90. 0. 1. 0.)
              (draw))
            (lambda (i) (+ i 1))
            0))
  
  (glDisable GL_LIGHTING)
  (glVertexPointer 3 GL_FLOAT 0 line)
  (glEnableClientState GL_VERTEX_ARRAY)
  (glColor4f 0. 1. 0. 1.)
  (render-lines)  
  (glEnable GL_LIGHTING))

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

(define-event-handler (touches-began touches event)
  (for-each record-touch touches))

(define-event-handler (touches-moved touches event)
  (let* ((touch (car touches))
         (dist (update-touch/get-movement touch)))
    (set! yaw (+ yaw (* (cdr dist) .2)))
    (set! pitch (+ pitch (* (car dist) .2)))))

(define-event-handler (touches-ended touches event)
  (for-each remove-touch touches))

;;; threading test

(define num-error-threads 20)

(define (make-error-threads)
  (map thread-start!
       (unfold (lambda (i) (> i num-error-threads))
               (lambda (i) (error-thread i))
               (lambda (i) (+ i 1))
               0)))

(define (error-thread i)
  (make-thread
   (lambda ()
     (thread-sleep! i)
     (myerror i))))

;;; app

(define (init)
  (random-source-randomize! default-random-source)

  ;; opengl
  (let* ((fov 40.)
         (aspect (/ (UIView-width (current-view))
                    (UIView-height (current-view)))))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (perspective fov aspect 1. 1000.)
    (lookat (make-vec3d 0. 0. 0.)
            (make-vec3d 0. 0. 1.)
            (make-vec3d 0. 1. 0.))
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))

  (glEnable GL_DEPTH_TEST)
  (glDisable GL_CULL_FACE)
  (glShadeModel GL_SMOOTH)

  (glEnable GL_RESCALE_NORMAL)
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glLightf GL_LIGHT0 GL_LINEAR_ATTENUATION .01)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->GLfloat* (vector .07 .07 .07 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->GLfloat* (vector 5. 5. 2. 1.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->GLfloat* (vector 1. 1. 1. 1.)))

  (glFogx GL_FOG_MODE GL_EXP2)
  (glFogfv GL_FOG_COLOR (vector->GLfloat* (vector 0. 0. 0. 1.)))
  (glFogf GL_FOG_DENSITY .01)
  (glFogf GL_FOG_START 1.)
  (glFogf GL_FOG_END 1000.)
  (glEnable GL_FOG)

  (set! pitch -45.))

(define (make-scene)
  (scene-list-clear)

  (scene-list-add
   (make-scene-object
    sphere-mesh
    (make-vec3d 0. 0. 1.)
    (make-vec3d 20. 0. 15.)))

  (scene-list-add
   (make-scene-object
    jlongster-mesh
    (make-vec3d 0. 2. 8.)
    (make-vec3d 20. -5. 20.)
    (make-vec4d 0. 1. 0. 200.)
    3.3)))

(define (run)
  (scene-list-update update-physics))

(define (render)
  (glClearColor .0 .0 .0 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (run)
  (render-grid)
  (run-render-queue (scene-list->render-queue))
  (##gc))

(define (get-title)
  "")

;;;; scratch

;;(define cow-mesh (obj-load (resource "resources/cow")))

;; (scene-list-add
;;  (make-scene-object
;;   cow-mesh
;;   (make-vec3d 0. 2. 8.)
;;   (make-vec3d 20. -5. 20.)
;;   (make-vec4d 0. 1. 0. 230.)
;;   10.
;;   (make-vec3d -10. 20. -10.)))
