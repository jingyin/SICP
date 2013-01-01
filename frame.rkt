#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define one (- 1 0.01))

; painter that draws the outline of fthe designated frame
(define outline-segments
  (list (make-segment
         (make-vect 0 0)
         (make-vect one 0.0))
        (make-segment
         (make-vect 0 0)
         (make-vect 0 one))
        (make-segment
         (make-vect one 0)
         (make-vect one one))
        (make-segment
         (make-vect 0 one)
         (make-vect one one))))
(define outline (segments->painter outline-segments))

; painter that draws an X by connecting opposite corners of the frame
(define x-segments
  (list (make-segment
         (make-vect one 0)
         (make-vect 0 one))
        (make-segment
         (make-vect 0 0)
         (make-vect one one))))
(define x (segments->painter x-segments))

; painter that connects the mid-points of the frame
(define diamond-segments
  (list (make-segment
         (make-vect 0.5 0)
         (make-vect one 0.5))
        (make-segment
         (make-vect one 0.5)
         (make-vect 0.5 one))
        (make-segment
         (make-vect 0.5 one)
         (make-vect 0 0.5))
        (make-segment
         (make-vect 0 0.5)
         (make-vect 0.5 0))))
(define diamond (segments->painter diamond-segments))

(paint outline)
(paint x)
(paint diamond)

(define (transform-painter:2 painter origin corner1 corner2)
  (define (transform-frame frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (make-frame new-origin
                    (vector-sub (m corner1) new-origin)
                    (vector-sub (m corner2) new-origin)))))
  (lambda (frame)
    (painter (transform-frame frame))))

(define (flip-vert:2 painter)
  (transform-painter:2 painter
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(paint einstein)
(paint (flip-vert:2 einstein))

(define (flip-horiz:2 painter)
  (transform-painter:2 painter
                       (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(paint (flip-horiz:2 einstein))

(define (rotate90:2 painter)
  (transform-painter:2 painter
                       (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)))

(paint (rotate90:2 einstein))

(define (rotate180:2 painter)
  (transform-painter:2 painter
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 0.0)))

(paint (rotate180:2 einstein))

(define (rotate270:2 painter)
  (transform-painter:2 painter
                       (make-vect 0.0 1.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0)))

(paint (rotate270:2 einstein))

(define (beside:2 painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter:2 painter1
                                           (make-vect 0.0 0.0)
                                           split-point
                                           (make-vect 0.0 1.0)))
          (paint-right (transform-painter:2 painter2
                                            split-point
                                            (make-vect 1.0 0.0)
                                            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(paint (beside:2 einstein (rotate90:2 einstein)))

(define (below:2 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below (transform-painter:2 painter1
                                            (make-vect 0.0 0.0)
                                            (make-vect 1.0 0.0)
                                            split-point))
          (paint-above (transform-painter:2 painter2
                                            split-point
                                            (make-vect 1.0 0.5)
                                            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-below frame)
        (paint-above frame)))))

(paint (below:2 einstein (rotate90:2 einstein)))

(define (below:3 painter1 painter2)
  (rotate90:2 (beside:2 (rotate270:2 painter1) (rotate270:2 painter2))))

(paint (below:3 einstein (rotate90:2 einstein)))