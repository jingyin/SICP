#lang racket

(provide make-rect)
(provide rect-height)
(provide rect-width)

; Uses midpoint and width/height
(define (make-rect m w h)
  (cons m (cons w h)))

(define (rect-height r)
  (cdr (cdr r)))

(define (rect-width r)
  (car (cdr r)))
