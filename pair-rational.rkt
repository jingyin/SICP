#lang racket

(provide make-rat)
(provide numer)
(provide denom)

(define (make-rat n d)
  (let ((normalizer (if (< d 0)
                        -1
                        1))
        (g (gcd n d)))
    (cons (/ n g normalizer) (/ d g normalizer))))

(define (numer r) (car r))
(define (denom r) (cdr r))
