#lang racket

;(require "set-unordered-list.rkt")
;(require "multiset-unordered-list.rkt")
(require "set-ordered-list.rkt")

(define s (make-set 1 1 2 3 3 4))
(define t (make-set 2 2 4 5 5 6))

(intersection-set s t)
(union-set s t)
(element-of-set? 1 s)
(element-of-set? 1 t)
(element-of-set? 5 s)
(element-of-set? 5 t)
(adjoin-set 1 s)
(adjoin-set 1 t)
