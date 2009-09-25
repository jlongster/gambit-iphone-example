;;;; "app3"
;;; renders a 3d box, which may bounce around some

(declare (block)
         (standard-bindings)
         (extended-bindings))

(include "events#.scm")
(include "obj-loader2.scm")
(include "scene.scm")
(include "physics.scm")

;;;;; settings

(define %%settings (make-table))

(define (get-config name #!optional default)
  (table-ref %%settings name default))

(define (set-config name val)
  (table-set! %%settings name val))

;;;;; masses

(define-type mass
  constructor: really-make-mass
  mass
  position
  scene-object)

(define (make-mass mass pos)
  (really-make-mass mass pos #f))

(define masses '())

(define (add-mass mass)
  (set! masses (cons mass masses))
  (let ((obj (make-scene-object mass-mesh
                                (make-vec3d 1. 1. 1.)
                                (mass-position mass)
                                (make-vec4d 0.5 0.5 0. (* (random-real) 360.))
                                1.5)))
    (mass-scene-object-set! mass obj)
    (scene-list-add obj)))

(define (mass-apply-gravity)
  (for-each
   (lambda (mass)
     (for-each
      (lambda (el)
        (if (and (scene-object-velocity el)
                 (scene-object-radius el))
            (let* ((v (vec3d-sub (mass-position mass)
                                 (scene-object-position el)))
                   (d (- (vec3d-length v)
                         (scene-object-radius el)
                         (scene-object-radius (mass-scene-object mass))))
                   (F (* 6.67428e-11 (/ (* (mass-mass mass) OBJECT_MASS)
                                        (* d d))))
                   (accel (vec3d-scalar-mul
                           (vec3d-unit v)
                           (/ F OBJECT_MASS))))
              (scene-object-velocity-set!
               el
               (vec3d-add (scene-object-velocity el)
                          accel)))))
      scene-list))
   masses))

(define (mass-lighting)
  (let loop ((i 0))
    (if (< i 9)
        (begin
          (glDisable (+ GL_LIGHT0 i))
          (loop (+ i 1)))))

  (let loop ((tail masses)
             (i 0))
    (if (and (not (null? tail))
             (< i 9))
        (let ((pos (mass-position (car tail)))
              (light (+ GL_LIGHT0 i)))
          (glEnable light)
          (glLightf light GL_CONSTANT_ATTENUATION .5)
          (glLightfv light GL_AMBIENT (vector->GLfloat* (vector .05 .05 .05 1.)))
          (glLightfv light GL_POSITION
                     (vector->GLfloat* (vector (vec3d-x pos)
                                               (vec3d-y pos)
                                               (vec3d-z pos)
                                               1.)))
          (glLightfv light GL_DIFFUSE (vector->GLfloat* (vector 1. 1. 1. 1.)))
          (loop (cdr tail) (+ i 1))))))

(define OBJECT_MASS 1000.)

;;;;; throwing

(define mass-mesh (obj-load (resource "resources/mass") #t))
(define sphere-mesh (obj-load (resource "resources/sphere") #t))
(define collision-mesh (obj-load (resource "resources/collision") #t))

(define (spread-number fl)
  (- (* fl 2.) 1.))

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
                        #f))))

;;;;; collisions

(define collision-reference (make-table))

(define (reset-table!)
  (set! collision-reference (make-table)))

(define (obj-collided? obj1 obj2)
  ;; sphere collision
  (let ((diff (vec3d-sub (scene-object-position obj1)
                         (scene-object-position obj2)))
        (r1 (scene-object-radius obj1))
        (r2 (scene-object-radius obj2)))
    (and r1 r2
         (< (vec3d-length diff) (+ r1 r2 -1.)))))

;; (define (detect-collisions)
;;   (reset-table!)
;;   (fold
;;    (lambda (obj1 acc)
;;      (or acc
;;          (fold
;;           (lambda (obj2 acc)
;;             (or acc
;;                 (and (not (table-ref collision-reference (list obj1 obj2) #f))
;;                      (not (eq? obj1 obj2))
;;                      (obj-collided? obj1 obj2))))
;;           #f
;;           scene-list)))
;;    #f
;;    scene-list))

(define (detect-collisions)
  (fold
   (lambda (mass acc)
     (let ((obj1 (mass-scene-object mass)))
       (or acc
           (fold
            (lambda (obj2 acc)
              (or acc
                  (and (not (eq? obj1 obj2))
                       (obj-collided? obj1 obj2))))
            #f
            scene-list))))
   #f
   masses))

;;;;; controls

(define (screen-to-space x y)
  (let* ((width (UIView-width (current-view)))
         (height (UIView-height (current-view)))
         (pers (current-perspective))
         (x-width (- (perspective-xmax pers)
                     (perspective-xmin pers)))
         (y-width (- (perspective-ymax pers)
                     (perspective-ymin pers))))
    (vec3d-unit
     (make-vec3d
      (- (perspective-xmax pers)
         (* (/ x width) x-width))
      (- (perspective-ymax pers)
         (* (/ y height) y-width))
      (perspective-znear pers)))))

(define %%touch-times (make-table))

(define (touch-record-time touch)
  (table-set! %%touch-times touch (real-time)))

(define (touch-pop-life-time touch)
  (let ((v (table-ref %%touch-times touch)))
    (table-set! %%touch-times touch)
    (- (real-time) v)))

(define-event-handler (touches-began touches event)
  (let ((now (real-time)))
    (for-each (lambda (touch)
                (touch-record-time touch))
              touches)))

(define-event-handler (touches-ended touches event)
  (for-each (lambda (touch)
              (if (not collided)
                  (let ((loc (UITouch-location touch))
                        (power (touch-pop-life-time touch)))
                    (user-toss-ball (screen-to-space (car loc) (cdr loc))
                                    power))))
            touches))

(define (user-toss-ball dir power)
  (throw (vec3d-scalar-mul dir (get-config "touch-depth"))
         (vec3d-scalar-mul
          (vec3d-unit
           (vec3d-component-mul dir (get-config "touch-dir")))
          (user-force power))))

(define (user-force power)
  (let ((power (max 0. (min 1. power)))
        (f (get-config "touch-force")))
    (- f (* power power f))))

;;;;; app

(define collided #f)

(define reset-camera glLoadIdentity)

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
  (glLightf GL_LIGHT0 GL_CONSTANT_ATTENUATION .6)
  (glLightfv GL_LIGHT0 GL_AMBIENT (vector->GLfloat* (vector .15 .15 .15 1.)))
  (glLightfv GL_LIGHT0 GL_POSITION (vector->GLfloat* (vector 25. 25. 0. 1.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->GLfloat* (vector 1. 1. 1. 1.)))

  (glFogx GL_FOG_MODE GL_EXP2)
  (glFogfv GL_FOG_COLOR (vector->GLfloat* (vector 0. 0. 0. 1.)))
  (glFogf GL_FOG_DENSITY .01)
  (glFogf GL_FOG_START 1.)
  (glFogf GL_FOG_END 1000.)
  (glEnable GL_FOG)

  (current-level)

  (set! GRAVITY (make-vec3d 0. 0. 0.)))

(define (level1)
  (set-config "touch-force" 19.)
  (set-config "touch-dir" (make-vec3d 15. 15. 5.))
  (set-config "touch-depth" 10.)
  (add-mass (make-mass 5.3736e11 (make-vec3d 0. 0. 25.))))

(define (level2)
  (set-config "touch-force" 25.)
  (set-config "touch-dir" (make-vec3d 15. 15. 5.))

  ;(add-mass (make-mass 4.9736e11 (make-vec3d -10. 10. 35.)))
  (add-mass (make-mass 5.3736e13 (make-vec3d 7. -10. 45.)))
  ;(add-mass (make-mass 1.3736e11 (make-vec3d -3. -7. 35.)))
  (add-mass (make-mass 1.3736e11 (make-vec3d 3. 10. 45.))))

(define (reset-and-add mass)
  (reset)
  (add-mass mass))

(define current-level level1)

(define (reset #!optional make-level)
  (set! scene-list '())
  (set! masses '())
  (set! collided #f)
  (if make-level
      (current-level)))

(define (run)
  (if (and (not collided)
           (detect-collisions))
      (let ((now (real-time)))
        (set! collided (real-time))
        (scene-list-add
         (make-scene-object
          collision-mesh
          (make-vec3d .8 .5 .2)
          (make-vec3d 0. 0. 6.)
          (make-vec4d 0. 1. 0. 180.)
          #f
          (make-vec3d 0. 0. 0.)
          (make-vec3d 0. 0. 0.)
          (lambda (el)
            (if (> (real-time) (+ now 1.))
                (begin
                  (scene-object-velocity-set! el #f)
                  (scene-object-acceleration-set! el #f))))))))

  (if (and collided
           (> (real-time) (+ collided 2.)))
      (begin
        ;; (set! current-level (if (eq? current-level level1)
        ;;                         level2
        ;;                         level1))
        (reset #t)))

  (mass-apply-gravity)
  (scene-list-update update-physics))

(define (render)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (run)
  ;(mass-lighting)
  (run-render-queue (scene-list->render-queue))
  (##gc))

(define (get-title)
  "")
