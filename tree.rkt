#lang racket

(require racket/trace)
(require "numerical.rkt")

(define (count-leaves x)
  (cond ((null? x ) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 4))))

(define l1 (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

(define l2 (list (list 7)))
(car (car l2))

(define l3  (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))

(define (deep-reverse x)
  (define (iter acc a)
    (cond ((null? a) acc)
          ((not (pair? a)) a)
          (else (iter (cons (deep-reverse (car a)) acc) (cdr a)))))
  (iter '() x))

(define x (list (list 1 2) (list 3 4)))
(define y (list 1 2 3 4))
(define z (list (list 1 2) (list 4 5 (list 5 6 7)) 5 6 (list 7 8)))
(deep-reverse y)
(deep-reverse x)
(reverse x)
(deep-reverse z)

(define (fringe t)
  (cond ((null? t) '())
        ((not (pair? t)) (list t))
        (else (append (fringe (car t)) (fringe (cdr t))))))

(fringe (list 1 2))
(fringe (list x x))
(fringe y)
(fringe z)

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree l1 10)
(scale-tree l2 2)
(scale-tree l3 3)

(define (scale-tree2 tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree2 subtree factor)
             (* subtree factor))) tree))

(scale-tree2 l1 10)
(scale-tree2 l2 2)
(scale-tree2 l3 3)

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree l1)
(square-tree l2)
(square-tree l3)

(define (square-tree2 tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree2 subtree)
             (square subtree))) tree))

(square-tree2 l1)
(square-tree2 l2)
(square-tree2 l3)

(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree))) tree))

(define (square-tree3 tree) (tree-map square tree))
(square-tree3 l1)
(square-tree3 l2)
(square-tree3 l3)

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
