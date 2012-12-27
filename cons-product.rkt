#lang racket

(provide cons)
(provide car)
(provide cdr)

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (define (iter acc p)
    (if (= 0 (remainder p 2))
        (iter (+ acc 1) (/ p 2))
        acc))
  (iter 0 z))

(define (cdr z)
  (define (iter acc p)
    (if (= 0 (remainder p 3))
        (iter (+ acc 1) (/ p 3))
        acc))
  (iter 0 z))
