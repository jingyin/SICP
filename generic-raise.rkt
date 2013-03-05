#lang racket

(require "tagged-obj-original.rkt")
(require "getput.rkt")
(require "func-util.rkt")
(require racket/trace)

(provide apply-generic get-raise)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-raise a1 a2))
                      (t2->t1 (get-raise a2 a1)))
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else error "No method for these types" (list op type-tags)))))
              (error "No method for these types - APPLY-GENERIC" (list op type-tags)))))))

; return a function that raises a1 to a2
; if such a coercion is not possible, return false
(define (get-raise a1 a2)
  (define (identity x) x)
  (define (get-coercion-rec a1 a2 acc)
    (if a1
        (if (eq? (type-tag a1) (type-tag a2))
            acc
            (let ((raise-f (get 'raise (list (type-tag a1)))))
              (if raise-f
                  ; would be easier if we can just use raise-f as is (no stripping of type tags)
                  (get-coercion-rec (raise-f (contents a1)) a2 (compose (compose raise-f contents) acc))
                  #f)))
        #f))
  ;(trace get-coercion-rec)
  (get-coercion-rec a1 a2 identity))
