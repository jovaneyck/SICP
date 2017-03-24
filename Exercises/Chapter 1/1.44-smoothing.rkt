#lang racket

(define (repeated f n)
  (define (iter n acc)
    (if (= n 1)
      acc
      (iter (- n 1) (compose f acc))))
  (iter n f))

(define (avg x y z) (/ (+ x y z) 3))

(define dx 0.0001)
(define (smooth f)
  (λ (x)
    (let ((less (f (- x dx)))
          (exact (f x))
          (more (f (+ x dx))))
      (avg less exact more))))

(define (n-fold-smooth n f)
  ((repeated smooth n) f))

(require rackunit)

(check-= ((smooth (λ (x) (exp x))) 13) 442413.393 .001)
(check-= ((n-fold-smooth 3 (λ (x) (exp x))) 13) 442413.396 .001)