#lang racket

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (divisible-by? number divisor)
  (= 0 (remainder number divisor)))

(define (factor f pair)
  (define (iter acc pair)
    (if (not (divisible-by? pair f))
        acc
        (iter (+ 1 acc) (/ pair f))))
  (iter 0 pair))

(define (car pair)
  (factor 2 pair))

(define (cdr pair)
  (factor 3 pair))

(require quickcheck)

(define car-of-cons-is-always-first
  (property ([x arbitrary-natural]
             [y arbitrary-natural])
            (equal? x (car (cons x y)))))
(quickcheck car-of-cons-is-always-first)

(define cdr-of-cons-is-always-second
  (property ([x arbitrary-natural]
             [y arbitrary-natural])
            (equal? y (cdr (cons x y)))))
(quickcheck cdr-of-cons-is-always-second)
