#lang racket

(define zero
  (λ (f) (λ (x) x)))
(define one
  (λ (f) (λ (x) (f x))))
(define two
  (λ (f) (λ (x) (f (f x)))))

(define (succ n)
  (λ (f) (λ (x) (f     ((n f) x)))))

(define (add a b)
  (λ (f) (λ (x) ((a f) ((b f) x)))))

(define (to-int n)
  (define (inc n) (+ n 1))
  ((n inc) 0))

(require rackunit)

(check-eq? (to-int zero) 0)
(check-eq? (to-int (succ zero)) 1)
(check-eq? (to-int (succ (succ zero))) 2)

(check-eq? (to-int one) 1)
(check-eq? (to-int (succ one)) 2)
(check-eq? (to-int two) 2)
(check-eq? (to-int (succ two)) 3)

(check-eq? (to-int (add zero zero)) 0)
(check-eq? (to-int (add (succ zero) (succ (succ zero)))) 3)
(check-eq? (to-int (add one one)) 2)
(check-eq? (to-int (add two one)) 3)
(check-eq? (to-int (add two two)) 4)
(displayln "done")