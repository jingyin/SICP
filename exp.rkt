#lang racket

(require "numerical.rkt")

(define (fast-exp b n)
  (define (exp-rec b n)
    (if (= n 0)
        1
        (* b (exp-rec b (- n 1)))))
  (define (exp-iter b counter product)
    (if (= counter 0)
        product
        (exp-iter b (- counter 1) (* b product))))
   (define (fast-exp-rec b n)
     (cond ((= n 0) 1)
           ((even? n) (square (fast-exp-rec b (/ n 2))))
           (else (* b (fast-exp-rec b (- n 1))))))
   (define (fast-exp-iter b n a)
     (cond ((= n 0) a)
           ((even? n) (fast-exp-iter (square b) (/ n 2) a))
           (else (fast-exp-iter b (- n 1) (* a b)))))
   (fast-exp-iter b n 1))
   ;(fast-exp-rec b n))
   ;(exp-iter b n 1))
   ;(exp-rec b n))

(fast-exp 2 4)
(fast-exp 3 31)
(fast-exp 2 31)
(fast-exp 8 41)
;(fast-exp 10109 401000)
