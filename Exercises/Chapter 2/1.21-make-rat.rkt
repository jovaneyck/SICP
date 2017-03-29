#lang racket

(define (make-rat n d)
  (let (
        (g (gcd n d))
        (sign
         (if (< (* n d) 0)
             -1
             1)))
    (cons (* sign (abs (/ n g))) (abs (/ d g)))))

(define (numer rat)
  (car rat))
(define (denom rat)
  (cdr rat))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))

(require rackunit)
(check-eq? 3 (numer (make-rat 3 4)))
(check-eq? 4 (denom (make-rat 3 4)))

(check-true (equal-rat? (make-rat 1 2) (make-rat 2 4)))
(check-false (equal-rat? (make-rat 1 2) (make-rat 1 3)))

(check-equal? (make-rat 6 8) (make-rat 3 4) "should simplify rational numbers")

(check-equal? (make-rat -3 -4) (make-rat 3 4) "two minus signs result in plus sign")
(check-equal? (numer (make-rat -3 4)) -3 "minus sign should stay on numerator")
(check-equal? (denom (make-rat -3 4)) 4 "minus sign should stay on numerator")
(check-equal? (numer (make-rat 3 -4)) -3 "minus sign should transfer to numerator")
(check-equal? (denom (make-rat 3 -4)) 4 "minus sign should transfer to numerator")

(displayln "Done")