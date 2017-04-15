#lang racket

(define (same-parity . numbers)
  (let ([f (if (even? (car numbers)) even? odd?)])
    (filter f numbers)))

(require rackunit)

(check-equal? (same-parity 2) (list 2))
(check-equal? (same-parity 1 2) (list 1))
(check-equal? (same-parity 1 2 3) (list 1 3))
(check-equal? (same-parity 2 1 3 4 7 6) (list 2 4 6))
(println "Done")