#lang racket

; section 1.2.2
; phi golden ratio, which satisfies
; phi^2 = phi + 1
; phi = (phi + 1) / phi
; phi = 1 + (1 / phi)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio (fixed-point (λ (phi) (+ 1 (/ 1 phi))) 1))

(require rackunit)

(check-= (exact->inexact golden-ratio) 1.61803 0.0001)