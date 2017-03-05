#lang racket

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 100))

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

(require rackunit)
#|
> (sqrt 10)
3.1622776601683795
|#

(check-true (timed-prime-test 199))
(check-true (timed-prime-test 1009))
(check-true (timed-prime-test 10007))
(check-true (timed-prime-test 100003))
(check-true (timed-prime-test 1000003))
(check-true (timed-prime-test 10000019))
(check-true (timed-prime-test 1000000007))
(check-true (timed-prime-test 3999999979))