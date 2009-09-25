
(load "lodepng")

(let ((image (LodePNG-decode32f "sky.png")))
  (pp (list (LodeImage-width image)
            (LodeImage-height image))))
