#lang racket

(require "numerical.rkt")
(require racket/trace)

(provide prime?)

(define (smallest-divisor n)
  (define (next-divisor k)
    (define (next-divisor-naive) (+ k 1))
    (define (next-divisor-fast)
      (if (= k 2)
          3
          (+ k 2)))
    (next-divisor-fast))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))))
  (find-divisor n 2))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n)
  (define (naive-prime?)
    (= n (smallest-divisor n)))
  (define (fast-prime? times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? (- times 1)))
          (else false)))
  ;(fast-prime? 10))
  (naive-prime?))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (timed-prime-test n)
  (newline)
  (display n)
  (define (report-prime elapsed-time)
    (display "***")
    (display elapsed-time))
  (define (start-prime-test start-time)
    (if (prime? n)
        (report-prime (- (current-inexact-milliseconds) start-time))
        (display "")))
  (start-prime-test (current-inexact-milliseconds)))

(timed-prime-test 199)
(timed-prime-test 1999)
(timed-prime-test 19999)
(newline)

(define (search-for-primes gt k)
  (define (search-for-primes-iter n more-to-go)
    (cond ((> more-to-go 0) (timed-prime-test n)
                            (search-for-primes-iter (+ n 1)
                                                    (- more-to-go
                                                       (if (prime? n)
                                                           1
                                                           0))))))
  (search-for-primes-iter (+ gt 1) k))

;(trace search-for-primes)
(search-for-primes 1000 1)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(newline)

(define (pass-fermat-test? n)
  (define (pass-fermat-test-iter? a n)
    (if (= a n)
        true
        (and (= (expmod a n n) a)
             (pass-fermat-test-iter? (+ a 1) n))))
  (pass-fermat-test-iter? 1 n))

(define (carmichael? n)
  (and (pass-fermat-test? n)
       (not (prime? n))))

;(trace pass-fermat-test?)
(carmichael? 199)
(carmichael? 1999)
(carmichael? 561)
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)
(newline)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-mr a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (square-check x m)
  (if (and (not (= x 1)) (not (= x (- m 1))) (= 1 (remainder (square x) m)))
      0
      (remainder (square x) m)))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (square-check (expmod-mr base (/ exp 2) m) m))
        (else (remainder (* base (expmod-mr base (- exp 1) m)) m))))

;(trace expmod-mr)
(define (fast-prime-mr? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime-mr? n (- times 1)))
        (else false)))

(define (prime-mr? n)
  (fast-prime-mr? n 10))

(prime-mr? 199)
(prime-mr? 1999)
(prime-mr? 561)
(prime-mr? 1105)
(prime-mr? 1729)
(prime-mr? 2465)
(prime-mr? 2821)
(prime-mr? 6601)


