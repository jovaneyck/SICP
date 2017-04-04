#lang racket

(require rackunit)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(check-equal?
 (add-interval
  (make-interval 0.95 1.05)
  (make-interval 0.01 0.02))
 (make-interval 0.96 1.07))

;2.8
(define (sub-interval x y)
  (add-interval
   x
   (make-interval
    (* -1.0 (lower-bound y))
    (* -1.0 (upper-bound y)))))

(check-equal?
 (sub-interval
  (make-interval 0.95 1.05)
  (make-interval 0.01 0.02))
 (make-interval 0.94 1.03))