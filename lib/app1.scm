
;;;; config

(define vertices
  (vector->GLfloat* (vector -.05 -.05
                            .05  -.05
                            -.05  .05
                            .05   .05)))

;; You can use this to play around with per-vertex colors,
;; works fine if you uncomment the code in SELECT-BOX
;; (define colors
;;   (vector->GLubyte* (vector 255 255 0   255
;;                             0   255 255 255
;;                             0   0   0   0
;;                             255 0   255 255)))

(define num-boxes 100)
(define zoom-level 5)
(define rotate-speed 2)

;;;; rendering

(define (select-box)
  (glVertexPointer 2 GL_FLOAT 0 vertices)
  (glEnableClientState GL_VERTEX_ARRAY)

  ;; The folowing works too, but you have to uncomment the statement
  ;; which defines `colors' at the top of this file
  ;; (glColorPointer 4 GL_UNSIGNED_BYTE 0 colors)
  ;; (glEnableClientState GL_COLOR_ARRAY)
  )

(define (draw-box n)
  (define (ratio n #!optional mult)
    (let ((r (/ n num-boxes)))
      (exact->inexact (if mult (* r mult) r))))

  (define (screen-space f)
    (+ (/ f 2.) .75))

  (glLoadIdentity)
  (glColor4f .2 (ratio n) .5 1.)
  (glTranslatef (ratio n)
                (screen-space
                 (sin (+ (real-time)
                         (/ n zoom-level))))
                .0)
  (glRotatef (* (real-time)
                (* rotate-speed 100.))
             0. 0. 1.)
  (glDrawArrays GL_TRIANGLE_STRIP 0 4))

(define (init)
  (void))

(define (render)
  (select-box)
  (unfold (lambda (i) (>= i num-boxes))
          (lambda (i) (draw-box i))
          (lambda (i) (+ i 1))
          0))

(define (get-title)
  "Hello, World.  - Gambit Scheme")
