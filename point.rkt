#lang racket

(provide x-point)
(provide y-point)
(provide make-point)

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
