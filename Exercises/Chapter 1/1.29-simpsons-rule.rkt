#lang racket
; don't know how to declare "local variables", so using defines with arity 0.
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (coeff k)
    (cond
      ((= k 0) 1)
      ((= k n) 1)
      ((even? k) 2)
      (else 4)))
  (define (y k)
    (f (+ a (* k h))))
  (define (factor k)
    (* (coeff k) (y k)))
  (define (sum k acc)
    (if (> 0 k)
        acc
        (sum (- k 1) (+ acc(factor k)))))
  (* (/ h 3) (sum n 0)))

(require rackunit)

(define (cube x) (* x x x))

(check-eqv? (integral cube 0 1 100) (/ 1 4))
(check-eqv? (integral cube 0 1 1000) (/ 1 4))
(check-eqv? (integral cube 0 1 8) (/ 1 4)) ;way better approximation, only needs 8 refinements
(println "Done.")