#lang racket

(require "accumulate.rkt")

(provide enumerate-interval flatmap)

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; flattens lists of lists
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

