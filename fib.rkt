#lang racket

(require "numerical.rkt")

(define (fib n)
  (define (fib-iter a b p q n)
    (cond ((= n 0) b)
          ; should square the matrix now
          ((even? n) (fib-iter a
                               b
                               (+ (square q) (square p))
                               (+ (* p q) (* q (+ q p)))
                               (/ n 2)))
          (else (fib-iter (+ (* a q) (* a p) (* b q))
                          (+ (* a q) (* b p))
                          p
                          q
                          (- n 1)))))
  (fib-iter 1 0 0 1 n))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
(fib 11)
(fib 12)
(fib 13)
(fib 100)
