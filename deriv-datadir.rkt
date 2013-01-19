#lang racket

(require "getput.rkt")
(provide deriv)

(define (variable? v)
  (symbol? v))

(define (same-variable? v x)
  (and (variable? v) (variable? x) (eq? x v)))

(define (addend s) (car s))
(define (augend s) (cadr s))

(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (deriv-prod exp var)
  (make-sum (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))
            (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))))

(define (deriv-exp exp var)
  (make-product (exponent exp)
                (make-product (deriv (base exp) var)
                              (make-exponentiation (base exp) (- (exponent exp) 1)))))

(define (=number? exp n)
  (and (number? exp) (number? n) (= exp n)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (base exp) (car exp))
(define (exponent exp) (cadr exp))

(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-prod)
(put 'deriv '** deriv-exp)

(deriv '(+ x 4) 'x)
(deriv '(+ x (* 5 x)) 'x)
(deriv '(+ x (* 5 x)) 'y)
(deriv '(** x 4) 'x)
