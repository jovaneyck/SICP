#lang racket

(require quickcheck)

;quickcheck works as expected: define some properties that take arbitraries and verify them
(define strings-are-strings
  (property ([a-random-string arbitrary-string])
            (string? a-random-string)))

(quickcheck strings-are-strings)

(define (cons x y)
  (λ (m) (m x y)))
(define (car pair)
  (pair (λ (p q) p)))
(define (cdr pair)
  (pair (λ (p q) q)))

(define car-of-cons-is-always-first
  (property ([x arbitrary-integer]
             [y arbitrary-integer])
            (equal? x (car (cons x y)))))
(quickcheck car-of-cons-is-always-first)

(define cdr-of-cons-is-always-second
  (property ([x arbitrary-integer]
             [y arbitrary-integer])
            (equal? y (cdr (cons x y)))))
(quickcheck cdr-of-cons-is-always-second)