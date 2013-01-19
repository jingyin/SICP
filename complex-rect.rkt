#lang racket

(require "numerical.rkt")

(provide real-part imag-part magnitude angle make-from-real-imag make-from-mag-ang)

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cons a)) (* r (sin a))))
