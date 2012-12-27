#lang racket

(provide cons)
(provide car)
(provide cdr)

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else error "Argument not 0 or 1 - CONS" m)))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))
