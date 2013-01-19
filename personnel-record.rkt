#lang racket

(require "generic.rkt")
(require "getput.rkt")
(require "tagged-obj.rkt")
(require "list-util.rkt")


(define (find-tagged t r)
  (define (match-tag p)
    (if (pair? p)
        (eq? t (car p))
        (error "Not a pair - MATCH-TAG" p)))
  (if (null? r)
      (error "Tag not found - FIND-TAGGED" t)
      (if (match-tag (car r))
          (cadr (car r))
          (find-tagged t (cdr r)))))

(define (install-division-A-package)
  (define (get-salary r) (find-tagged 'salary r))
  (define (get-address r) (find-tagged 'address r))
  (define (find-record f name)
    (if (null? f) 
        '()
        (if (member (list 'name name) (car f))
            (make-record (car f))
            (find-record (cdr f) name))))
  (define (tag x) (attach-tag 'division-A x))
  (define (make-record r) (tag r))
  (define (make-file data) (tag data))
  (put 'get-salary '(division-A) get-salary)
  (put 'get-address '(division-A) get-address)
  (put 'find-record '(division-A division-A) find-record)
  (put 'make-file 'division-A make-file)
  'done)

(define (install-division-B-package)
  (define (get-salary r) (find-tagged 'salary (caddr r)))
  (define (get-address r) (find-tagged 'address (caddr r)))
  (define (find-record f name)
    (if (null? f)
        '()
        (if (eq? (cadr (car f)) name)
            (make-record (car f))
            (find-record (cdr f) name))))
  (define (tag x) (attach-tag 'division-B x))
  (define (make-record r) (tag r))
  (define (make-file data) (tag data))
  (put 'get-salary '(division-B) get-salary)
  (put 'get-address '(division-B) get-address)
  (put 'find-record '(division-B division-B) find-record)
  (put 'make-file 'division-B make-file)
  'done)

(define (get-salary r) (apply-generic 'get-salary r))
(define (get-address r) (apply-generic 'get-address r))
(define (find-record r n)
  (define (typify n)
    (attach-tag (type-tag r) n))
  (apply-generic 'find-record r (typify n)))

(define (find-employee-record files name)
  (let ((r (flatmap (lambda (f) (find-record f name)) files)))
    (if (and r (not (null? r)))
        r
        false)))

(for-each (lambda (x) (x)) (list install-division-A-package install-division-B-package))

; populate the two file structures
(define division-A-file ((get 'make-file 'division-A)
  '(((name John) (salary 100) (address WA)) ((name Jake) (salary 90) (address CA)))))

(define division-B-file ((get 'make-file 'division-B)
  '((name James ((salary 80) (address DC))) (name Jane ((salary 110) (address MA))))))

(define john-rec (find-employee-record (list division-A-file division-B-file) 'John))
(define jake-rec (find-employee-record (list division-A-file division-B-file) 'Jake))
(define james-rec (find-employee-record (list division-A-file division-B-file) 'James))
(define jane-rec (find-employee-record (list division-A-file division-B-file) 'Jane))

john-rec
jake-rec
james-rec
jane-rec

(get-salary john-rec)
(get-address john-rec)
(get-salary jake-rec)
(get-address jake-rec)
(get-salary james-rec)
(get-address james-rec)
(get-salary jane-rec)
(get-address jane-rec)
