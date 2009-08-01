
(define PI 3.14159265)

(define (perspective fovy aspect znear zfar)
  (let* ((xmax (* znear (tan (/ (* fovy PI) 360.))))
         (xmin (- xmax))
         (ymin (/ xmin aspect))
         (ymax (/ xmax aspect)))
    (glFrustumf xmin xmax ymin ymax znear zfar)))

(define (lookat eye center up)
  (let* ((z (vec3d-unit (vec3d-sub eye center)))
         (x (vec3d-unit (vec3d-cross up z)))
         (y (vec3d-unit (vec3d-cross x z))))
    (glMultMatrixf
     (vector->GLfloat*
      (vector (vec3d-x x) (vec3d-y x) (vec3d-z x) 0.
              (vec3d-x y) (vec3d-y y) (vec3d-z y) 0.
              (vec3d-x z) (vec3d-y z) (vec3d-z z) 0.
              0.          0.          0.          1.)))
    (glTranslatef (- (vec3d-x eye))
                  (- (vec3d-y eye))
                  (- (vec3d-z eye)))))
