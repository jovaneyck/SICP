#lang racket

(define (accumulate-filter filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

(define (inc n) (+ n 1))
(define (id n) n)
(define (every term) #t)

(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
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

(require rackunit)
(check-eq? (accumulate-filter every + 0 id 1 inc 10) 55)

;a. the sum of the squares of the prime numbers in the interval a to b (assuming that you have a prime? predicate already written)
(define (sum-of-squares-primes a b)
  (accumulate-filter prime? + 0 square a inc b))

(check-eq? (sum-of-squares-primes 2 10) 87)
;b. the product of all the positive integers less than n that are relatively prime to n (i.e., all positive integers i < n such that GCD(i,n) = 1). 
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;mimicking partial application by wrapping around procedures and returning procedures. I miss lambdas/partial application right now!
(define (pred-builder n)
  (define (relatively-prime i)
    (= 1 (gcd i n)))
  relatively-prime)

(define (product-positive-relatively-prime n)
  (accumulate-filter (pred-builder n) * 1 id 1 inc n)) ; partial application heyo!

(check-eq? (product-positive-relatively-prime 10) 189)
(println "Done")
