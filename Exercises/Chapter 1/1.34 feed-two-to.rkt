#lang racket

(define (f g)
  (g 2))

(require rackunit)

(define (square n) (* n n))

(check-eq? (f square) 4)
(check-eq? (f (λ (z) (* z (+ z 1)))) 6)

;(f f) crashes:

;application: not a procedure;
;expected a procedure that can be applied to arguments
;given: 2
;arguments.:

(println "Done")