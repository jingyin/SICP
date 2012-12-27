#lang racket

(require "pair-rational.rkt")

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(print-rat one-half)
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (make-rat 0 -1))
(print-rat (make-rat 1 -1))
(print-rat (make-rat -2 -2))
(print-rat (make-rat 2 3))
(print-rat (make-rat 2 -4))
(print-rat (make-rat 0 4))
(print-rat (make-rat -2 4))
