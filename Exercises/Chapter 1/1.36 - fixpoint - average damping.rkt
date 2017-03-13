#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (println next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(require rackunit)

(println "FP")
(define solution
  (fixed-point (λ (x) (/ (log 1000) (log x))) 2.0))
(check-= (exact->inexact solution) 4.5555 0.0001)

(println "FP with average damping")
(define damped-solution
  (fixed-point (λ (x) (/ (log 1000) (log x))) 2.0))
(check-= (exact->inexact damped-solution) 4.5555 0.0001)

(println "Done")