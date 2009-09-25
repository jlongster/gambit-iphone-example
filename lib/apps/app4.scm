
(include "obj-loader2.scm")

(define mesh #f)

(define (init)
  (let* ((fov 40.)
         (aspect (/ (UIView-width (current-view))
                    (UIView-height (current-view))))
         (half-fov-y (/ fov aspect 2)))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (perspective fov aspect 1. 1000.)
    (lookat (make-vec3d 0. 0. 0.)
            ;; (make-vec3d 0. (sin half-fov-y) (cos half-fov-y))
            (make-vec3d 0. 0. 1.)
            (make-vec3d 0. 1. 0.))
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))

  (glEnable GL_DEPTH_TEST)
  (glDisable GL_CULL_FACE)
  (glShadeModel GL_SMOOTH)
  
  (glEnable GL_LIGHTING)
  (glEnable GL_LIGHT0)
  (glLightfv GL_LIGHT0 GL_POSITION (vector->GLfloat* (vector 0. 3. 0. 1.)))
  (glLightfv GL_LIGHT0 GL_DIFFUSE (vector->GLfloat* (vector 1. 1. 1. 1.)))

  (let ((now (real-time)))
    (set! mesh (obj-load (resource "resources/logo.obj") #f))
    (pp (- (real-time) now))))

(define rot 0.)
(define (render)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (glVertexPointer 3 GL_FLOAT 0 (obj-vertices mesh))
  (glEnableClientState GL_VERTEX_ARRAY)
  (glNormalPointer GL_FLOAT 0 (->void* (obj-normals mesh)))
  (glEnableClientState GL_NORMAL_ARRAY)

  (glLoadIdentity)
  (glTranslatef 0. 0. 3.)
  (glRotatef rot 0.3 1. 0.)
  (glRotatef 90. 0. 0. 1.)
  (set! rot (+ rot .2))

  (glMaterialfv GL_FRONT_AND_BACK
                GL_DIFFUSE
                (vector->GLfloat*
                 (vector .8 .4 .1)))
  
  (glDrawElements GL_TRIANGLES
                  (obj-num-indices mesh)
                  GL_UNSIGNED_SHORT
                  (->void* (obj-indices mesh))))

(define (get-title)
  "")
