#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (average x y)
  (/ (+ x y) 2))

(define (average-point fst snd)
  (let (
        (x (average (x-point fst) (x-point snd)))
        (y (average (y-point fst) (y-point snd))))
    (make-point x y)))

(define (midpoint-segment segment)
  (let (
        (start (start-segment segment))
        (end (end-segment segment)))
    (average-point start end)))

(require rackunit)

(check-equal? (x-point (make-point 3 4)) 3)
(check-equal? (y-point (make-point 3 4)) 4)

(check-equal?
 (midpoint-segment
  (make-segment
   (make-point 0 0)
   (make-point 2 2)))
 (make-point 1 1))

(check-equal?
 (midpoint-segment
  (make-segment
   (make-point 3.0 5.0)
   (make-point 2.0 1.0)))
 (make-point 2.5 3.0))

(displayln "Done")