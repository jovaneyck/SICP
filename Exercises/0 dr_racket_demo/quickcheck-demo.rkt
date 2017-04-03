#lang racket

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