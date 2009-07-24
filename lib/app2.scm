
;;;; settings

(define AGE_OF_DEATH 1.) ; in seconds

;;;; events

(define-event-handler (touches-began touches event)
  (for-each (lambda (touch)
              (let ((loc (UITouch-location touch)))
                (add-point (car loc) (cdr loc))))
            touches))

(define-event-handler (touches-moved touches event)
  (for-each (lambda (touch)
              (let ((loc (UITouch-location touch)))
                (add-point (car loc) (cdr loc))))
            touches))

(define-event-handler (did-accelerate device accel)
  ;; (add-point (+ (/ (UIAcceleration-x accel) 2.) .5)
  ;;            (+ (/ (UIAcceleration-y accel) 2.) .5))
  
  (define (shift n weight)
    (+ n (* weight 70)))
  
  (add-point (shift 150 (UIAcceleration-x accel))
             (shift 100 (- (UIAcceleration-y accel)))
             '(.8 .5 .2)))

;;;; util

(define (display?)
  (let* ((time (real-time))
         (rounded (floor time)))
    (< (- time rounded) .05)))

(define %%screen-width #f)
(define %%screen-height #f)

(define (select-view)
  ;; This part of the ffi crashes right now
  ;; (let* ((bounds (UIView-bounds (current-view)))
  ;;        (size (CGRect-size bounds)))
  ;;   (set! %%screen-width (CGSize-width size))
  ;;   (set! %%screen-height (CGSize-height size)))
  (set! %%screen-width (UIView-width (current-view)))
  (set! %%screen-height (UIView-height (current-view))))

(define (view-space-x n)
  (exact->inexact (/ n %%screen-width)))

(define (view-space-y n)
  (exact->inexact (/ n %%screen-height)))

;;;; app

(define-type point
  x y
  color
  birthtime)

(define points '())

(define (add-point x y #!optional color)
  (set! points (cons (make-point x y
                                 (or color '(.2 .8 .5))
                                 (real-time))
                     points)))

(define (point-render point)
  (glLoadIdentity)
  (let ((color (point-color point)))
    (glColor4f
     (car color)
     (cadr color)
     (caddr color)
     (- 1.
        (/ (point-age point) AGE_OF_DEATH))))
  (glTranslatef (view-space-x (point-x point))
                (view-space-y (point-y point))
                0.)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4))

(define (point-age point)
  (- (real-time) (point-birthtime point)))

(define (point-dead? point)
 (> (point-age point) AGE_OF_DEATH))

(define vertices
  (vector->GLfloat* (vector -.05 -.0375
                            .05  -.0375
                            -.05  .0375
                            .05   .0375)))

(define (select-geometry)
  (glVertexPointer 2 GL_FLOAT 0 vertices)
  (glEnableClientState GL_VERTEX_ARRAY))

(define (init)
  (glEnable GL_BLEND)
  (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))

(define (render)
  (select-view)
  (select-geometry)

  (set! points
        (fold-right
         (lambda (point acc)
           (point-render point)
           (if (point-dead? point)
               acc
               (cons point acc)))
         '()
         points))
  (##gc))

(define (get-title)
  (number->string (real-time)))
