#lang racket

(provide make-rect)
(provide rect-height)
(provide rect-width)

(require "point.rkt")
(require racket/trace)


(define (make-rect ll ur)
  (cons ll ur))
(define (rect-height r)
  (- (y-point (cdr r))
     (y-point (car r))))
(define (rect-width r)
  (- (x-point (cdr r))
     (x-point (car r))))
