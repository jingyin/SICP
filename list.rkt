#lang racket

(require "numerical.rkt")

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define l (list 1 2 3 4))
(list-ref l 0)
(list-ref l 1)
(list-ref l 2)
(list-ref l 3)

(define (length items)
  (define (iter a count)
    (if (null? a)
        count
        (iter (cdr a) (+ 1 count))))
  (iter items 0))

(length (list 1 2 3 4))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append (list 1 2 3 4) (list 1 4 9 16))

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(last-pair (list 23 72 149 32))

(define (reverse l)
  (define (iter acc a)
    (if (null? a)
        acc
        (iter (cons (car a) acc) (cdr a))))
  (iter '() l))

(reverse (list 1 4 9 16 25))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (no-more? l) (null? l))
  (define (except-first-denomination l) (cdr l))
  (define (first-denomination l) (car l))
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values)))))

(cc 100 us-coins)
(cc 20 uk-coins)

(define (same-parity x . rest)
  (cons x (filter (if (odd? x) odd? even?) rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 1 3 4 5 6 7)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))
(define (scale-list items factor)
  (map (lambda (x) (* factor x)) items))

(scale-list (list 1 2 3 4 5) 10)

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(square-list (list 1 2 3 4 5))

(define (square-list2 items)
  (map square items))

(square-list2 (list 1 2 3 4 5))

(define (for-each f items)
  (cond ((null? items) (void))
        (else (f (car items)) (for-each f (cdr items)))))

(for-each (lambda (x) (display x) (newline)) (list 57 321 88))
