#lang racket

(provide union-set intersection-set element-of-set? adjoin-set make-set)
(require "accumulate.rkt")

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (multiplicity x set)
  (cond ((null? set) 0)
        ((equal? x (car set))
         (+ 1 (multiplicity x (cdr set))))
        (else (multiplicity x (cdr set)))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((<= (multiplicity (car set1) set1) (multiplicity (car set1) set2))
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((> (multiplicity (car set1) set1) (multiplicity (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

(define (make-set . elements) elements)
