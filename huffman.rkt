#lang racket

(require "list-util.rkt")

; routines dealing with leaf nodes
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

; routines dealing with non-terminal nodes
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; decoding bit stream given the huffman encoded tree
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH bit"))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; maintains a sorted list of tree nodes based on the total weight
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; given a list of pairs, build up the sorted list of tree nodes
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define decoded (decode sample-message sample-tree))
decoded

; encoding symbol stream given the huffman encoded tree
(define (encode message tree)
  (define (encode-symbol symbol branch)
    (if (leaf? branch)
        '()
        (let ((left (left-branch branch))
              (right (right-branch branch)))
          (cond ((memq symbol (symbols left)) (cons 0 (encode-symbol symbol left)))
                ((memq symbol (symbols right)) (cons 1 (encode-symbol symbol right)))
                (else (error "no recorded symbol in the tree" symbol))))))
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(encode decoded sample-tree)

(define (generate-huffman-tree pairs)
  (define (successive-merge trees)
    (cond ((null? trees) (error "empty set in SUCCESSIVE-MERGE"))
          ((null? (cdr trees)) (car trees))
          (else (let ((first (car trees))
                      (second (cadr trees))
                      (rest (cddr trees)))
                  (successive-merge (adjoin-set (make-code-tree first second) rest))))))
  (successive-merge (make-leaf-set pairs)))

(define frequency-pairs '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
(define lyrics '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))

(define lyrics-tree (generate-huffman-tree frequency-pairs))
(define encoded (encode lyrics lyrics-tree))
encoded
(length encoded)
(decode encoded lyrics-tree)
(* 3 (length lyrics))

(define fp (map (lambda (i) (list i (expt 2 i))) (enumerate-interval 0 4)))
(generate-huffman-tree fp)
(define fp2 (map (lambda (i) (list i (expt 2 i))) (enumerate-interval 0 9)))
(generate-huffman-tree fp2)
