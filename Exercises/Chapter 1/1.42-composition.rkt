#lang racket

;builtin racket\compose exists
(define (compose f g)
  (λ (x) (f (g x))))

(require rackunit)

(define (square x) (* x x))
(define (inc x) (+ x 1))

(check-eq? ((compose square inc) 6) 49)
(println "Done")