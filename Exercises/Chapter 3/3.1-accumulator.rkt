#lang racket

(require rackunit)

(define (make-accumulator start)
  (λ(x)
    (begin (set! start (+ start x))
           start)))

(print "Running tests...")

(begin
  (define A (make-accumulator 5))
  (check-eq? (A 10) 15)
  (check-eq? (A 10) 25))

(check-eq? ((make-accumulator 2) 4) 6)

(check-eq? (+ 1 1) 2)
(println "...done!")