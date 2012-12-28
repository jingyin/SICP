#lang racket

(require racket/trace)
(require "accumulate.rkt")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff
                                                   (* higher-terms x)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1)) t)))

(count-leaves (list 1 2 (list 3 (list 4 5) 6 7) 8 9))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (map (lambda (col) (dot-product row col)) cols)) m)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(transpose m)
(dot-product (list 1 2 3 4) (list 1 2 3 4))
(matrix-*-matrix m (transpose m))

(define fold-right accumulate)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))

(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

(define (rev-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(rev-r (list 1 2 3 4))

(define (rev-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(rev-l (list 1 2 3 4))
