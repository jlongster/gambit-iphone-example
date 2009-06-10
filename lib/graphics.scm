
(define (render)
  (let ((v (vector->GLfloat* (vector -.5 -.5
                                     .5  -.5
                                     -.5  .5
                                     .5   .5)))
        (c (vector->GLubyte* (vector 255 255 0   255
                                     0   255 255 255
                                     0   0   0   0
                                     255 0   255 255))))
    (glVertexPointer 2 GL_FLOAT 0 v)
    (glColorPointer 4 GL_UNSIGNED_BYTE 0 c)
    
    (glEnableClientState GL_VERTEX_ARRAY)
    (glEnableClientState GL_COLOR_ARRAY)
    
    (glDrawArrays GL_TRIANGLE_STRIP 0 4)
    
    (free v)
    (free c)))

(define (get-title)
  "Hello, World.  - Gambit")
