;;;; "lodepng"
;;; Library for loading PNG images.

(c-declare "#include \"ffi/lodepng/lodepng-src.c\"")
(c-declare "#include \"ffi/lodepng/util.c\"")

(c-define-type __lodepng_image
               (type "__lodepng_image" 'loadpng_image "__release_loadpng_image"))

(define LodeImage-data
  (c-lambda (__lodepng_image) unsigned-int8-array  "___result_voidstar = ___arg1.data;"))

(define LodeImage-width
  (c-lambda (__lodepng_image) int "___result = ___arg1.w;"))

(define LodeImage-height
  (c-lambda (__lodepng_image) int "___result = ___arg1.h;"))

(define LodePNG-decode32f
  (c-lambda (char-string) __lodepng_image #<<end-c-code
   __lodepng_image *img = malloc(sizeof(__lodepng_image));
   LodePNG_decode32f(&img->data, &img->w, &img->h, ___arg1);
   ___result_voidstar = img;
end-c-code
))

