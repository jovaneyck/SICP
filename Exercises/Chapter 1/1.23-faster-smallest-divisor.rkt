#lang racket

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (square n)
  (* n n))

(define (divides? a b)
  (= (remainder b a) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-inexact-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (report-non-prime)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

(define (report-non-prime) #f)

(define (prime? n)
  (= n (smallest-divisor n)))

(require rackunit)

(check-eq? 3 (next 2))
(check-eq? 5 (next 3))

(check-true (timed-prime-test 199))
(check-true (timed-prime-test 1009))
(check-true (timed-prime-test 10007))
(check-true (timed-prime-test 100003))
(check-true (timed-prime-test 1000003))
(check-true (timed-prime-test 10000019))
(check-true (timed-prime-test 1000000007))
(check-true (timed-prime-test 3999999979))
(check-false (timed-prime-test 4))
(check-false (timed-prime-test 1024))