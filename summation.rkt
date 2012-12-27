#lang racket

(require "numerical.rkt")
(require racket/trace)

(define (sum term next a b)
  (define (sum-rec curr)
    (if (> curr b)
        0
        (+ (term curr)
           (sum-rec (next curr)))))
  (define (sum-iter acc curr)
    (if (> curr b)
        acc
        (sum-iter (+ acc (term curr)) (next curr))))
  ;(trace sum-iter)
  ;(sum-rec a))
  (sum-iter 0 a))

(define (sum-integers a b)
  (sum identity inc a b))

(define (sum-cubes a b)
  (sum cube inc a b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (* 8 (sum pi-term pi-next a b)))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f add-dx (+ a (/ dx 2.0)) b) dx))

(sum-integers 1 10)
(sum-cubes 1 10)
(pi-sum 1 10000)
(integral cube 0 1 0.0001)

(define (simpson-integral f a b n)
  (define m (/ (- b a) (* 3.0 n)))
  (define h (* 3.0 m))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (f (+ a (* h k)))))
  (* m (sum simpson-term inc 0 n)))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)

(define (product term next a b)
  (define (rec curr)
    (if (> curr b)
        1
        (* (term curr) (rec (next curr)))))
  (define (iter acc curr)
    (if (> curr b)
        acc
        (iter (* acc (term curr)) (next curr))))
  (iter 1 a))
  ;(rec a))

(define (factorial n)
  (product identity inc 1 n))

(factorial 6)
(factorial 10)

(define (pi-product a b)
  (define (pi-term k)
    (- 1 (/ 1.0 (square (+ 1 (* 2 k))))))
  (* 4.0 (product pi-term inc a b)))

(pi-product 1 100)
(pi-product 1 1000)
(pi-product 1 1000000)

(define (accumulate combiner null-value term next a b)
  (define (rec curr)
    (if (> curr b)
        null-value
        (combiner (term curr)
                  (rec (next curr)))))
  (define (iter acc curr)
    (if (> curr b)
        acc
        (iter (combiner acc (term curr)) (next curr))))
  ;(rec a))
  (iter null-value a))

(define (acc-sum term next a b)
  (accumulate + 0 term next a b))

(define (acc-prod term next a b)
  (accumulate * 1 term next a b))

(acc-sum identity inc 1 10)
(acc-prod identity inc 1 10)

(define (filtered-accumulate combiner null-value term next pred a b)
  (define (rec curr)
    (if (> curr b)
        null-value
        (if (pred curr)
            (combiner (term curr)
                      (rec (next curr)))
            (rec (next curr)))))
  (define (iter acc curr)
    (if (> curr b)
        acc
        (if (pred curr)
            (iter (combiner (term curr) acc)
                  (next curr))
            (iter acc (next curr)))))
  (iter null-value a))
  ;(rec a))

(define (sum-primes a b)
  (filtered-accumulate + 0 identity inc prime? a b))

(sum-primes 1 100)

(define (sum-non-primes a b)
  (filtered-accumulate + 0 identity inc (lambda (x) (not (prime? x))) a b))

(sum-non-primes 1 100)

(define (prod-rel-prime n)
  (define (rel-prime? k) (= (gcd k n) 1))
  (filtered-accumulate * 1 identity inc rel-prime? 1 (- n 1)))

(prod-rel-prime 10)
