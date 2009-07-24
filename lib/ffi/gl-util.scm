
(define PI 3.14159265)

(define (perspective fovy aspect znear zfar)
  (let* ((ymax (* znear (tan (/ (* fovy PI) 360.))))
         (ymin (- ymax))
         (xmin (/ ymin aspect))
         (xmax (/ ymax aspect)))
    (glFrustumf ymin ymax xmin xmax znear zfar)))
