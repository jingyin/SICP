#lang racket

(require racket/trace)

(provide average)
(provide square)
(provide cube)
(provide sine)
(provide divides?)
(provide identity)
(provide inc)
(provide prime?)
(provide fib)

(define (average x y) (/ (+ x y) 2.0))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (sine angle)
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (prime? n)
  (define (next-divisor k)
    (if (= k 2)
        3
        (+ 2 k)))
  (define (find-divisor curr)
    (cond ((> (square curr) n) n)
          ((divides? curr n) curr)
          (else (find-divisor (next-divisor curr)))))
  (define smallest-divisor (find-divisor 2))
  (and (not (= n 1)) (= smallest-divisor n)))
;(trace prime?)

(define (fib n)
  ; b is fib(count)
  (define (iter a b count)
    (if (= count n)
        b
        (iter (+ a b) a (+ count 1))))
  (iter 1 0 0))
