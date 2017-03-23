#lang racket

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

(define dx  0.00001)
(define (deriv g)
  (λ (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (λ (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (λ (x) (+ (* x x x) (* a x x) (* b x) c)))

(require rackunit)

(let ((a 1) (b 1) (c 1))
  (check-= (newtons-method (cubic a b c) 1) -1 0.00001))
(let ((a 1) (b 2) (c 1))
  (check-= (newtons-method (cubic a b c) 1) -0.56984 0.00001))
(let ((a -3) (b -2) (c 1))
  (check-= (newtons-method (cubic a b c) 1) 0.34337 0.00001))
(let ((a 3) (b -2) (c 1))
  (check-= (newtons-method (cubic a b c) 1) -3.62736 0.00001))
(println "Done")