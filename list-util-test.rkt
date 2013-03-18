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

(check-false (or-list '()))
(check-true (or-list '(#t #f #f)))
(check-false (or-list '(#f #f #f)))

(check-true (equal? '() (zip '() '() '())))
(check-true (equal? '((1 2 3) (4 5 6) (7 8 9)) (zip '(1 4 7) '(2 5 8) '(3 6 9))))

(check-true (equal? '(1 1 1) (generate 1 3)))
