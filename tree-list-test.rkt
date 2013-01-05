#lang racket

(require "tree-list.rkt")
(require "set-btree.rkt")

(define t1 (make-set 1 2 3 4 5 6 7))
(define t2 (make-set 3 1 2 4 5 6 7))
(define t3 (make-set 7 6 5 4 3 2 1))

(tree->list-1 t1)
(tree->list-2 t1)
(tree->list-1 t2)
(tree->list-2 t2)
(tree->list-1 t3)
(tree->list-2 t3)
