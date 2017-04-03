#lang racket

(define zero
  (λ (f) (λ (x) x)))
(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))
