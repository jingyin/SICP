#lang racket

(require "numerical.rkt")
(require "accumulate.rkt")

(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons '() (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 0 10)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '() (filter even? (map fib (enumerate-interval 0 n)))))

(sum-odd-squares (list 1 (list 2 3 (list 4 5) (list 6 7))))
(even-fibs 14)

(define (list-fib-squares n)
  (accumulate cons '() (map square (map fib (enumerate-interval 0 n)))))

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))
