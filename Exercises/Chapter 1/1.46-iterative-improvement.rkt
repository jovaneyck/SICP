#lang racket

(define (iterative-improve good-enough? improve)
  (define first-guess 1.0)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter first-guess))

(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))

(require rackunit)

(define (sqrt x)
  (iterative-improve
   (λ (guess) (< (abs (- (square guess) x)) 0.0001))
   (λ (guess) (average guess (/ x guess)))))

(define (fixed-point f)
  (iterative-improve
   (λ (guess) (< (abs (- (f guess) guess)) 0.00001))
   f))

(check-= (sqrt 16) 4 0.0001)
(check-= (fixed-point (λ (phi) (+ 1 (/ 1 phi)))) 1.618 0.0001)
(println "Done")