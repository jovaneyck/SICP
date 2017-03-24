#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ;(println (exact->inexact guess))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (define (iter n acc)
    (cond ((= n 0) (error "must repeat at least once"))
          ((= n 1) acc)
          (else (iter (- n 1) (compose f acc)))))
  (iter n f))

(define (nth-root number-damps n x)
  (let ((init-guess 1.0) ; note: 1 iso 1.0 makes a HUGE difference performance-wise!
        (damped (repeated average-damp number-damps))
        (f (λ (y) (/ x (expt y (- n 1))))))
    (fixed-point (damped f) init-guess)))

(require rackunit)
(check-= (exact->inexact (nth-root 1 3 8  )) 2 .1)
(check-= (exact->inexact (nth-root 2 4 16 )) 2 .1)
(check-= (exact->inexact (nth-root 2 5 32 )) 2 .1)
(check-= (exact->inexact (nth-root 2 6 64 )) 2 .1)
(check-= (exact->inexact (nth-root 2 7 128)) 2 .1)
(check-= (exact->inexact (nth-root 3 8 256)) 2 .1)
(check-= (exact->inexact (nth-root 3 15 2)) 1.04 .01)
(check-= (exact->inexact (nth-root 4 16 2)) 1.04 .01)
(check-= (exact->inexact (nth-root 4 31 2)) 1.02 .01)
(check-= (exact->inexact (nth-root 5 32 2)) 1.02 .01)

(define (nb-required-damps n)
  (define (log2 x)
    (/ (log x) (log 2)))
  (inexact->exact (floor (log2 n))))

(check-eq? (nb-required-damps 3) 1)
(check-eq? (nb-required-damps 4) 2)
(check-eq? (nb-required-damps 7) 2)
(check-eq? (nb-required-damps 31) 4)
(check-eq? (nb-required-damps 32) 5)

(define (nth-r n x)
  (define number-damps (nb-required-damps n))
  (let ((init-guess 1.0)
        (damped (repeated average-damp number-damps))
        (f (λ (y) (/ x (expt y (- n 1))))))
    (fixed-point (damped f) init-guess)))

(check-= (exact->inexact (nth-r 32 2)) 1.02 .01)
(check-= (exact->inexact (nth-r 1234567 2)) 1.0 .01)

(println "Done")