#lang racket

(require "numerical.rkt")

(provide real-part imag-part magnitude angle make-from-real-imag make-from-mag-ang)

(define (real-part z)
  (* (magnitude z) (cons (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))
