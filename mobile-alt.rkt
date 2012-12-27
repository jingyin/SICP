#lang racket

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (total-weight mobile)
  (define (total-weight-br branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (if (null? mobile)
      0
      (+ (total-weight-br (left-branch mobile))
         (total-weight-br (right-branch mobile)))))

(define m1 (make-mobile (make-branch 20 30) (make-branch 30 20)))
(define m2 (make-mobile (make-branch 20 m1) (make-branch 30 m1))) 
(define m3 (make-mobile (make-branch 20 100) (make-branch 40 m1)))
(define m4 (make-mobile (make-branch 20 m3) (make-branch 10 m2)))
(total-weight m1)
(total-weight m2)
(total-weight m3)
(total-weight m4)

; it's balanced if all submobile is balanced and the total weight
(define (balanced? mobile)
  ; returns (list total-weight balanced?)
  (define (weight-balanced?-br branch)
    (if (pair? (branch-structure branch))
        (weight-balanced? (branch-structure branch))
        (list (branch-structure branch) true)))
  (define (weight-balanced? m)
    (let ((wbl (weight-balanced?-br (left-branch m)))
          (wbr (weight-balanced?-br (right-branch m))))
      (let ((twl (car wbl))
            (bl (cadr wbl))
            (twr (car wbr))
            (br (cadr wbr)))
        (list (+ twl twr)
              (and bl br (= (* twl (branch-length (left-branch m)))
                            (* twr (branch-length (right-branch m)))))))))
  (cadr (weight-balanced? mobile)))

(balanced? m1)
(balanced? m2)
(balanced? m3)
(balanced? m4)
