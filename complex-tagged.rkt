#lang racket

(require "tagged-obj.rkt")
(require "numerical.rkt")
(provide real-part imag-part magnitude angle make-from-real-imag make-from-mag-ang)

(define (real-part-rect z) (car z))
(define (imag-part-rect z) (cdr z))
(define (magnitude-rect z)
  (sqrt (+ (square (real-part-rect z))
           (square (imag-part-rect z)))))
(define (angle-rect z)
  (atan (imag-part-rect z)
        (real-part-rect z)))
(define (make-from-real-imag-rect x y)
  (attach-tag 'rect (cons x y)))
(define (make-from-mag-ang-rect r a)
  (attach-tag 'rect (cons (* r (cons a))
                          (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))
(define (make-from-mag-ang-polar x y)
  (attach-tag 'polar (cons x y)))

(define (rect? x)
  (eq? (type-tag x) 'rect))
(define (polar? x)
  (eq? (type-tag x) 'polar))

(define (real-part z)
  (cond ((rect? z) (real-part-rect (contents z)))
        ((polar? z) (real-part-polar (contents z)))
        (else (error "Unknown type - REAL-PART" z))))
(define (imag-part z)
  (cond ((rect? z) (imag-part-rect (contents z)))
        ((polar? z) (imag-part-polar (contents z)))
        (else (error "Unknown type - IMAG-PART" z))))
(define (magnitude z)
  (cond ((rect? z) (magnitude-rect (contents z)))
        ((polar? z) (magnitude-polar (contents z)))
        (else (error "Unknown type - MAGNITUDE" z))))
(define (angle z)
  (cond ((rect? z) (angle-rect (contents z)))
        ((polar? z) (angle-polar (contents z)))
        (else (error "Unknown type - ANGLE" z))))
(define (make-from-real-imag x y)
  (make-from-real-imag-rect x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))