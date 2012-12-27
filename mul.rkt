#lang racket

(define (mul a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (fast-mul-rec a b)
    (cond ((= b 0) 0)
          ((even? b) (double (fast-mul-rec a (halve b))))
          (else (+ a (fast-mul-rec a (- b 1))))))
  (define (fast-mul-iter a b sum)
    (cond ((= b 0) sum)
          ((even? b) (fast-mul-iter (double a) (halve b) sum))
          (else (fast-mul-iter a (- b 1) (+ a sum)))))
  (fast-mul-iter a b 0))
  ;(fast-mul-rec a b))

(mul 2 3)
(mul 5 4)
(mul 10 9)
