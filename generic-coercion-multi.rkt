#lang racket

(require "tagged-obj.rkt")
(require "getput.rkt")
(require "coercion-table.rkt")
(require "list-util.rkt")
(require racket/trace)

(provide apply-generic)

(define (apply-generic op . args)
  (define (find-first p l)
    (if (null? l)
        false
        (if (p (car l))
            (car l)
            (find-first p (cdr l)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (> (length args) 1)
              (let ((types (map type-tags args)))
                (if (list-identical? types)
                    (error "No method for these types" (list op type-tags))
                    ; find a type for which we can coerce all other types into
                    ; coerce to that type and call apply-generic
                    (let ((generic-type
                           (find-first (lambda (t)
                                        (and-list (map (lambda (s)
                                                        (if (eq? s t)
                                                            true
                                                            (get-coercion s t))) types))) types)))
                      (if generic-type
                          (apply-generic op (map (lambda (s)
                                                  (if (eq? (type-tag s) generic-type)
                                                      s
                                                      ((get-coercion (type-tag s) generic-type) s))) args))
                          (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))
