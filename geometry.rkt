#lang racket

(require "segment.rkt")
(require "point.rkt")
(require "numerical.rkt")
;(require "rect.rkt")
(require "rect-alt.rkt")
(require racket/trace)

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (midpoint-segment seg)
  (let ((s (start-segment seg))
        (e (end-segment seg)))
    (make-point (average (x-point s) (x-point e))
                (average (y-point s) (y-point e)))))

(print-point (make-point 1 2))
(print-point (make-point 2 3))
(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 2 3))))

(define (rect-perimeter r)
  (* 2
     (+ (rect-height r)
        (rect-width r))))

(define (rect-area r)
  (* (rect-height r)
     (rect-width r)))

;(define r (make-rect (make-point 1 2) 3 4))
(define r (make-rect (make-point 1 2) (make-point 3 4)))
(rect-perimeter r)
(rect-area r)
