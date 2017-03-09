#lang racket

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (inc n) (+ n 1))
(define (id n) n)

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate-rec combiner null-value term (next a) next b))))

(require rackunit)

(check-eq? (product id 1 inc 10) (* 1 2 3 4 5 6 7 8 9 10))
(check-eq? (sum id 1 inc 10) (+ 1 2 3 4 5 6 7 8 9 10))
(check-eq? (accumulate-rec + 0 id 1 inc 10) (+ 1 2 3 4 5 6 7 8 9 10))