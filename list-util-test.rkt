#lang racket

(require rackunit)
(require "list-util.rkt")

(check-false (list-identical? '(1 2 3)))
(check-true (list-identical? '(1 1 1)))
(check-true (list-identical? '()))
(check-true (list-identical? '((1 2) (1 2) (1 2))))
(check-false (list-identical? '((1 2) (1 2) (1 3))))

(check-true (and-list '()))
(check-true (and-list '(#t #t #t)))
(check-false (and-list '(#f #t #t)))
