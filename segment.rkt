#lang racket

(provide start-segment)
(provide end-segment)
(provide make-segment)

(define (make-segment s e) (cons s e))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
