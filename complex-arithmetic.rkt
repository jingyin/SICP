#lang racket

;(require "complex-tagged.rkt")
(require "complex-generic.rkt")

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define c1 (make-from-real-imag 2 4))
(define c2 (make-from-mag-ang 4 1.57))

c1
c2
(add-complex c1 c2)
(sub-complex c1 c2)
(mul-complex c1 c2)
(div-complex c1 c2)
