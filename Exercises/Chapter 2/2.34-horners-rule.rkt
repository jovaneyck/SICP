#lang racket

(require rackunit)

(define (horner-eval x coefficient-sequence)
  (foldr (lambda (coefficient acc) (+ coefficient (* x acc)))
         0
         coefficient-sequence))

(check-equal?
 (horner-eval 1337 (list 10))
 10)

(check-equal?
 (horner-eval 5 (list 3 2))
 13)

(check-equal?
 (horner-eval 5 (list 2 3 2))
 67)

(check-equal?
 (horner-eval 2 (list 1 3 0 5 0 1))
 79)
