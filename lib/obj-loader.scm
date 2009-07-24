
(declare (block)
         (standard-bindings)
         (extended-bindings))

;; ----------------------------------------
;; vertices & triangles
;; ----------------------------------------
(define-type vector3d
  id: 863A066E-B9D4-493E-8E6E-AF72A7208C88
  x y z)

(define-type triangle
  id: E82033E8-B6F3-4ECD-8162-ED8DDC6079D6
  v1 v2 v3)

(define-type obj-data
  id: 5582EB25-5587-49F7-93E2-FA13DC2CD207
  constructor: really-make-obj-data
  vertices
  vertices-finalized
  triangles
  triangles-finalized)

(define (make-obj-data)
  (really-make-obj-data '() #f '() #f))

(define (push-vertex mesh vx)
  (obj-data-vertices-set!
   mesh
   (cons vx (obj-data-vertices mesh))))

(define (push-triangle mesh triangle)
  (obj-data-triangles-set!
   mesh
   (cons triangle (obj-data-triangles mesh))))

(define (push-face mesh face)
  (define (get-vertex i)
    (vector-ref (obj-data-vertices mesh)
                (- (vector-ref face i) 1)))

  (finalize-vertices mesh)
  (push-triangle mesh
                 (make-triangle (get-vertex 0)
                                (get-vertex 1)
                                (get-vertex 2)))
  (if (= (vector-length face) 4)
      (push-triangle
       mesh
       (make-triangle (get-vertex 0)
                      (get-vertex 2)
                      (get-vertex 3)))))

(define (finalize-vertices mesh)
  (if (not (obj-data-vertices-finalized mesh))
      (begin
        (obj-data-vertices-set!
         mesh
         (list->vector (reverse (obj-data-vertices mesh))))
        (obj-data-vertices-finalized-set! mesh #t))))

(define (finalize-triangles mesh)
  (if (not (obj-data-triangles-finalized mesh))
      (begin
        (obj-data-triangles-set!
         mesh
         (reverse (obj-data-triangles mesh)))
        (obj-data-triangles-finalized-set! mesh #t))))

;; ----------------------------------------
;; obj file parsing
;; ----------------------------------------
(define (maybe-cdr lst)
  (if lst (cdr lst) lst))

(define (read-next-declaration)
  (if (eq? (peek-char) #\#)
      #f
      (let ((token (read (current-input-port))))
        (maybe-cdr (assq token
                         '((v . vertex)
                           (f . face)
                           (usemtl . usemtl)))))))

(define (read-next-vertex mesh)
  (let ((x (exact->inexact (read)))
        (y (exact->inexact (read)))
        (z (exact->inexact (read))))
    (push-vertex mesh (make-vector3d x y z))))

(define (read-next-face mesh)
  (define (read-all-vertices)
    (let loop ((acc '()))
      (if (eq? (peek-char) #\newline)
          (reverse acc)
          (loop (cons (read) acc)))))

  (let* ((vertices (list->vector (read-all-vertices)))
         (num (vector-length vertices)))
    (if (and (not (= num 3))
             (not (= num 4)))
        (raise "Invalid number of vertices for this face"))
    (push-face mesh vertices)))

(define (read-next-line mesh)
  (let ((type (read-next-declaration)))
    (case type
      ((vertex) (read-next-vertex mesh))
      ((face) (read-next-face mesh))
      ((mtlswitch) (read-next-mtlswitch mesh))))
  (let ((next (peek-char)))
    (if (eq? next #!eof)
        #f
        (begin
          (if (not (eq? next #\newline))
              (read-line))
          #t))))

(define (obj-file-loop)
  (let ((mesh (make-obj-data)))
    (let loop ()
      (if (read-next-line mesh)
          (loop)
          (begin
            (finalize-triangles mesh)
            mesh)))))

(define (obj-load file)
   (with-input-from-file file
    (lambda ()
      (obj-file-loop))))
