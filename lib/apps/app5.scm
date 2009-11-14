;;;; "app5"
;;; Balls thrown up and pulled down by gravity (determined from
;;; accelerometer)

(include "../events#.scm")
(include "../obj-loader2.scm")
(include "../scene.scm")
(include "../physics.scm")

;;; resources

(define sphere-mesh #f)

;;; throwing

(define (spread-number fl)
  (- (* fl 2.) 1.))

(define (get-random-throw)
  (+ (real-time) (/ (random-real) 9.)))

(define next-throw (get-random-throw))

(define (possibly-throw)
  (if (> (real-time) next-throw)
      (begin
        (throw (make-vec3d (* (spread-number (random-real)) 5.)
                           0.
                           (+ (* (random-real) 35.) 10.))
               (make-vec3d 0. (+ (random-real) 5.) 0.))
        (set! next-throw (get-random-throw)))))

(define (throw pos vel)
  (define (random-color)
    (random-real))
  
  (let ((then (real-time)))
    (scene-list-add
     (make-scene-object sphere-mesh
                        (make-vec3d (random-color)
                                    (random-color)
                                    (random-color))
                        pos
                        ;; (make-vec4d 0. 1. 0. -90.)
                        #f
                        1.5
                        vel
                        #f
                        (lambda (el)
                          (> 3. (- (real-time) then)))
                        ))))

;;; controls

(define CAMERA 0.)

(define (reset-camera)
  (glLoadIdentity)
  (glRotatef (* (/ CAMERA PI) 180) 0. 0. 1.))

(define-event-handler (did-accelerate device accel)
  (let ((v (vec3d-unit (make-vec3d (UIAcceleration-x accel)
                                   (UIAcceleration-y accel)
                                   0.))))
    (set! CAMERA (atan (- (vec3d-x v)) (- (vec3d-y v))))))


;;; app

(define (init)
  (random-source-randomize! default-random-source)

  (set! sphere-mesh (obj-load (local-resource "resources/sphere") #t))
  
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
  (glLightf GL_LIGHT0 GL_CONSTANT_ATTENUATION .6)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->float-array (vector .15 .15 .15 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->float-array (vector 25. 25. 0. 1.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->float-array (vector 1. 1. 1. 1.)))

  (glFogx GL_FOG_MODE GL_EXP2)
  (glFogfv GL_FOG_COLOR (vector->float-array (vector 0. 0. 0. 1.)))
  (glFogf GL_FOG_DENSITY .01)
  (glFogf GL_FOG_START 1.)
  (glFogf GL_FOG_END 1000.)
  (glEnable GL_FOG))

(define (run)
  (possibly-throw)
  (scene-list-update update-physics))

(define (render)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (run)
  (run-render-queue (scene-list->render-queue))
  (##gc))

(define (get-title)
  "")
