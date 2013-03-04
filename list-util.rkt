#lang racket

(require "accumulate.rkt")

(provide enumerate-interval flatmap list-identical? and-list)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; flattens lists of lists
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; a logical-and implementation that works for lists
(define (and-list l)
  (if (null? l)
      true
      (and (car l) (and-list (cdr l)))))

(define (list-identical? l)
  (define (enumerate x n)
    (if (= n 0)
        '()
        (cons x (enumerate x (- n 1)))))
  (if (null? l)
      true
      (equal? l (enumerate (car l) (length l)))))
