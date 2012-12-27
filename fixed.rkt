#lang racket

(require "numerical.rkt")
(require racket/trace)

(define tolerance 0.00001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  ;(trace try)
  (try first-guess))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0)
(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 2)

(fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0)

; no average damping for x^x = 1000
(fixed-point (lambda (y) (/ (log 1000) (log y))) 2.0)
(fixed-point (lambda (y) (average y (/ (log 1000) (log y)))) 2.0)

(define (cont-frac n d k)
  (define (rec curr)
    (if (> curr k)
        0
        (/ (n curr) (+ (d curr) (rec (+ curr 1))))))
  (define (iter acc curr)
    (if (= curr 0)
        acc
        (iter (/ (n curr) (+ (d curr) acc)) (- curr 1))))
  (iter 0 k))
  ;(rec 1))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (cond ((= (remainder i 3) 2) (* 2 (/ (+ i 1) 3)))
                        (else 1)))
                10))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (cond ((> i 1) (- (square x)))
                     (else x)))
             (lambda (i) (- (* 2 i) 1))
             k))

(tan-cf (/ (half-interval-method sin 2.0 4.0) 4) 10)

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqr-root x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqr-root 2)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(cube-root 2)

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt2 x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt2 2)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))
(sqrt3 2)

(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
(sqrt4 2)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)

(define (double f)
  (lambda (x) (f (f x))))

(((double double) inc) 5)
(((double (double double)) inc) 5)
((double (double inc)) 5)
((double (double (double inc))) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (define (iter delayed curr)
    (if (> curr n)
        delayed
        (iter (compose f delayed) (+ curr 1))))
  (define (rec curr)
    (if (> curr n)
        identity
        (compose f (rec (+ curr 1)))))
  (iter (lambda (x) x) 1))
  ;(rec 1))

((repeated square 2) 5)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3.0)))

(define (repeated-smooth f n)
  (repeated smooth n))

;(trace average-damp)
;(trace compose)
(define (nth-root x n)
  ; by experimentation, for a given n, we need the smallest k s.t. 2^k > n
  (define (find-smallest-beyond)
    (define (iter pow exponent)
      ; 2^exponent = pow
      (if (> pow n)
          (- exponent 1)
          (iter (* 2 pow) (+ exponent 1))))
    (iter 1 0))
  (define (root y)
    (/ x (expt y (- n 1))))
  (fixed-point ((repeated average-damp (find-smallest-beyond)) root) 1.0))

(nth-root 2 2)
(nth-root 2 3)
(nth-root 2 4)
(nth-root 2 8)
(nth-root 2 9)
(nth-root 2 16)

(define (iterative-improve good-enough? improve-guess guess)
  (define (iter g)
    (if (good-enough? g)
        g
        (iter (improve-guess g))))
  (iter guess))

(define (sqrt5 x)
  (iterative-improve (lambda (g) (< (abs (- (square g) x)) 0.0001))
                     (lambda (g) (average g (/ x g)))
                     1.0))
(sqrt5 2)

(define (fixed-point2 f guess)
  (iterative-improve (lambda (g) (< (abs (- g (f g))) 0.0001)) f guess))

(fixed-point2 (lambda (y) (+ 1 (/ 1 y))) 1.0)
