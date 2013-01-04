#lang racket

(define (equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((or (null? x) (null? y)) #f)
        ((and (pair? x) (pair? y))
          (and (equal? (car x) (car y))
               (equal? (cdr x) (cdr y))))
        ((or (pair? x) (pair? y)) #f)
        (else (eq? x y))))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))
