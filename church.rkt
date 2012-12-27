#lang racket

(require "numerical.rkt")

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; should just be identity function
((zero inc) 0)
((zero inc) 1)
((zero inc) 2)
(newline)

; should apply inc once
((one inc) 0)
((one inc) 1)
((one inc) 2)
(newline)

; should apply inc twice
((two inc) 0)
((two inc) 1)
((two inc) 2)
(newline)

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define three (add one two))
; should apply inc thrice
((three inc) 0)
((three inc) 1)
((three inc) 2)
(newline)
