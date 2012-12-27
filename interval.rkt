#lang racket

(require "numerical.rkt")

(define (make-interval a b)
  (if (> a b)
      (error "Upper-bound greater than lower-bound make-interval" a b)
      (cons a b)))

(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

(define (mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (>= a 0) (>= c 0)) (make-interval (* a c)
                                                  (* b d)))
          ((and (>= a 0) (>= d 0)) (make-interval (* b c)
                                                  (* b d)))
          ((>= a 0) (make-interval (* b c)
                                   (* a d)))
          ((and (>= b 0) (>= c 0)) (make-interval (* a d)
                                                  (* b d)))
          ((and (>= b 0) (>= d 0))
            (let ((min1 (* a d))
                  (min2 (* b c))
                  (max1 (* b d))
                  (max2 (* a c)))
              (make-interval (min min1 min2) (max max1 max2))))
          ((>= b 0) (make-interval (* b c)
                                   (* a c)))
          ((and (< b 0) (>= c 0)) (make-interval (* a d)
                                                 (* b c)))
          ((and (< b 0) (>= d 0)) (make-interval (* a d)
                                                 (* a c)))
          (else (make-interval (* b d)
                               (* a c))))))
  ; textbook implementation
  ;(let ((p1 (* (lower-bound x) (lower-bound y)))
        ;(p2 (* (lower-bound x) (upper-bound y)))
        ;(p3 (* (upper-bound x) (lower-bound y)))
        ;(p4 (* (upper-bound x) (upper-bound y))))
    ;(make-interval (min p1 p2 p3 p4)
                   ;(max p1 p2 p3 p4)))) 
(define (div-interval x y)
  (let ((reciprocal (if (or (> (lower-bound y) 0)
                            (< (upper-bound y) 0))
                        (make-interval (/ 1.0 (upper-bound y))
                                       (/ 1.0 (lower-bound y)))
                        (error "Divided interval spans zero" (lower-bound y) (upper-bound y)))))
    (mul-interval x reciprocal)))

(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]")
  (newline))

(define i (make-interval 1 2))
(define j (make-interval -2 -1))
(print-interval i)
(print-interval j)
(print-interval (add-interval i j))
(print-interval (sub-interval i j))
(print-interval (mul-interval i j))
(print-interval (div-interval i j))
;(print-interval (div-interval (make-interval 1 2) (make-interval -2 1)))

; To prove that the width of multiplying or dividing intervals isn't a function of widths of the two intervals
; We need to show that for intervals with the same widths, multiplying them yield intervals with different widths
; Essentially, the potential to map the same inputs to different outputs

; Counterexample for multiplication
; Consider intervals ([1,2];[3,4]) and ([3,4];[4,5]), each interval has width of 1/2
; Yet, multiplying the two intervals in each set yields intervals with widths 5/4 and 2 respectively

; Counterexample for division
; Consider intervals ([1,1];[4,6]) and ([1,1];[6,8])
; Dividing the first interval by the second in each set yields intervals with widths 1/24 and 1/48 respectively

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p)) (* c (+ 1 p))))

(define (percent i)
  (/ (width i) (center i)))

(define (print-interval-percent i)
  (display "(")
  (display (center i))
  (display ",")
  (display (* 100 (percent i)))
  (display "%)")
  (newline))

(define i1 (make-center-percent 50 0.01))
(define i2 (make-center-percent 100 0.02))

(print-interval-percent i1)
(print-interval-percent (div-interval i1 i1))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (inv-par1 r1 r2)
  (div-interval (add-interval r1 r2)
                (mul-interval r1 r2)))

(define (inv-par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (add-interval (div-interval one r1)
                  (div-interval one r2))))

(print-interval-percent (par1 i1 i2))
(print-interval-percent (par2 i1 i2))

(print-interval-percent (inv-par1 i1 i2))
(print-interval-percent (inv-par2 i1 i2))

(define (exaggerated-example i)
  (div-interval (mul-interval i (mul-interval i (mul-interval i (mul-interval i i))))
                (mul-interval i (mul-interval i (mul-interval i i)))))

(print-interval-percent i1)
(print-interval-percent (exaggerated-example i1))
