#lang racket

(define (double f)
  (λ (x) (f (f x))))

(require rackunit)

(define (inc x) (+ x 1))
(define inc2 (double inc))
(check-eq? (inc2 6) 8)

(check-eq? (((double (double double)) inc) 5) 21)
;(double double)
; \x -> (double (double x))) (quadruple)

;(double (double double))
; \x -> ((double double) (double double)) x) (x16)
(println "Done")