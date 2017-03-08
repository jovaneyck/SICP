#lang racket

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc n) (+ n 1))
(define (id n) n)

(define (factorial n)
  (product-iter id 1 inc n))

(define (approx-pi n)
  (define (factor n)
    (define nominator
      (if (odd? n)
          (+ n 1)
          (+ n 2)))
    (define denominator
      (if (odd? n)
          (+ n 2)
          (+ n 1)))
    (/ nominator denominator))
  (* 4 (product-iter factor 1 inc n)))

(define (product-rec term a next b)
  (if (> a b)
      1
      (*
       (term a)
       (product-rec term (next a) next b))))


(require rackunit)

(check-eq? (product-iter id 1 inc 10) (* 1 2 3 4 5 6 7 8 9 10))

(define (check-fac? in expected)
  (check-eq? (factorial in) expected))

(check-fac? 1 1)
(check-fac? 2 2)
(check-fac? 3 6)
(check-fac? 4 24)
(check-fac? 20 2432902008176640000)

(check-eqv? (exact->inexact (approx-pi 10000)) 3.1417497057380523)
(check-= (exact->inexact (approx-pi 10000)) pi 0.001)

(check-eq? (product-rec id 1 inc 10) (* 1 2 3 4 5 6 7 8 9 10))

(time (void (product-iter id 1 inc 10000)));should be faster & less prone to stack busts
(time (void (product-rec  id 1 inc 10000)))

(println "Done")