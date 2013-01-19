#lang racket

(require "accumulate.rkt")
(require "list-util.rkt")
(require "numerical.rkt")

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j) (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 6)

; assume s has distinct elements
(define (permutation s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutation (remove x s))))
               s)))

(permutation (list 1 2 3 4))

(define (unique-pair n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- n i))))
           (enumerate-interval 1 n)))

(unique-pair 10)

(define (unique-triplet n)
  (flatmap (lambda (k)
             (flatmap (lambda (j)
                        (map (lambda (i) (list i j k))
                             (enumerate-interval 1 (- n k j))))
                      (enumerate-interval 1 (- n k))))
           (enumerate-interval 1 n)))

(unique-triplet 5)

(define (triplet-summed n)
  (filter (lambda (s) (= (accumulate + 0 s) n)) (unique-triplet n)))

(triplet-summed 10)

(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (cons (list new-row k) rest-of-queens))
  (define (contains l i)
    (if (null? l)
        false
        (or (= (car l) i) (contains (cdr l) i))))
  (define (safe? k positions)
    ;true)
    (let ((kth-row (car (car positions))))
      (not (contains (flatmap (lambda (pair)
                              (let ((row (car pair))
                                    (col (cadr pair)))
                                (filter (lambda (r) (and (>= r 1) (<= r board-size)))
                                        (list row (+ row (- k col)) (- row (- k col))))))
                              (cdr positions))
                     kth-row))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row
                                                   k
                                                   rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(newline)

; assume terminal font is monospace
(define (compute-and-display-queens board-size)
  (define (display-solution solution)
    (define (display-entry pair)
      (define (display-n str n)
        (cond ((= n 0) (void))
              (else (display str)
                    (display-n str (- n 1)))))
      (display-n "-" (- (car pair) 1))
      (display "+")
      (display-n "-" (- board-size (car pair)))
      (newline))
    (let ((solution-by-row (sort solution (lambda (x y) (< (cadr x) (cadr y))))))
      (for-each display-entry solution-by-row)
      (newline)))
  (for-each display-solution (queens board-size)))

(compute-and-display-queens 7)
