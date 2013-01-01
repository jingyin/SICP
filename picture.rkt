#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(paint einstein)
(define e2 (beside einstein (flip-vert einstein)))
(define e4 (below e2 e2))

(paint e4)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(paint (flipped-pairs einstein))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(paint (right-split einstein 4))
(paint (up-split einstein 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split einstein 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 4))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define flipped-pairs2 (square-of-four identity flip-vert identity flip-vert))

(paint (flipped-pairs2 einstein))

(define (square-limit2 painter n)
  ((square-of-four flip-horiz identity rotate180 flip-vert) (corner-split painter n)))

(paint (square-limit2 einstein 4))

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first second) painter (- n 1))))
          (first painter (second smaller smaller))))))

(define right-split2 (split beside below))
(define up-split2 (split below beside))

(paint (right-split2 einstein 4))
(paint (up-split2 einstein 4))