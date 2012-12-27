#lang racket

(provide cbrt) 
(require "numerical.rkt")

; This routine is not actually good enough :)
; Doesn't work for smaller numbers due to the epsilon too great for smaller numbers
; Doesn't work for greater numbers due to the epsilon too small for greater numbers
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define epsilon 0.00000001)

(define (great-enough? guess prev-guess x)
  (< (abs (- 1 (/ guess prev-guess))) epsilon))

;(define (sqrt-iter2 guess x)
;  (if (great-enough? (improve guess x) guess x)
;      guess
;      (sqrt-iter2 (improve guess x) x)))

; This is the smallest 9e+X number taking us more than a couple of seconds
; Due to the nature of floating point number precision, the numbers never get within epsilon of each other
; (sqrt-iter 1 9e+214)
; This would give inaccurate results if epsilon is 0.0001 under the original good-enough? routine
; Due to us terminating early because of the greater epsilon compared to x
; (sqrt-iter 1 0.00000001)

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (great-enough? (improve guess) guess x)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1))

(sqrt 2.0)
(sqrt 0.00000001)
(sqrt 9e+214)

(define (cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (cube-iter guess)
    (if (great-enough? (improve guess) guess x)
        guess
        (cube-iter (improve guess))))
  (cube-iter 1))

(cbrt 1.0)
(cbrt 2.0)
(cbrt 0.000000001)
(cbrt 27e+180)
