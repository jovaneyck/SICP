#lang racket

(define (sum-recursive term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recursive term (next a) next b))))

(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(require rackunit)

(define (identity x) x)
(define (inc n) (+ n 1))

(check-eq? (sum-recursive identity 1 inc 10) 55)
(check-eq? (sum-iterative identity 1 inc 10) 55)

(println "Done")