#lang racket

(require "set-btree.rkt")
(require "tree-list.rkt")

(define s (make-set 1 2 3 4))
(define t (make-set 2 4 5 6))

(define t->l tree->list-2)

(intersection-set s t)
(t->l (intersection-set s t))
(union-set s t)
(t->l (union-set s t))
(newline)
(element-of-set? 1 s)
(element-of-set? 1 t)
(element-of-set? 5 s)
(element-of-set? 5 t)
(adjoin-set 1 s)
(t->l (adjoin-set 1 s))
(adjoin-set 1 t)
(t->l (adjoin-set 1 t))
