#lang racket

(require "accumulate.rkt")
(require racket/trace)

(provide enumerate-interval flatmap list-identical? and-list or-list zip generate)

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
      #t
      (and (car l) (and-list (cdr l)))))

; a logical-or implementation that works for lists
(define (or-list l)
  (if (null? l)
      #f
      (or (car l) (or-list (cdr l)))))

(define (list-identical? l)
  (define (enumerate x n)
    (if (= n 0)
        '()
        (cons x (enumerate x (- n 1)))))
  (if (null? l)
      true
      (equal? l (enumerate (car l) (length l)))))

(define (zip . args)
  (let ((is-null? (map null? args)))
    (if (or-list is-null?)
        (if (and-list is-null?)
            '()
            (error "lists of unequal length - ZIP"))
        (cons (map car args)
              (apply zip (map cdr args))))))

(define (generate t n)
  (if (= n 0)
      '()
      (cons t (generate t (- n 1)))))
