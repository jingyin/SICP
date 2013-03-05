#lang racket

(provide attach-tag type-tag contents)

(define (attach-tag type-tag contents) (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        (else (error "Bad tagged datum - TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum - CONTENTS" datum))))
