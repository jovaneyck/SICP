#lang racket

(require rackunit)
(require quickcheck)
 (require rackunit/quickcheck)

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

;2.9
(define (width interval)
  (/
   ( - (upper-bound interval) (lower-bound interval))
   2))

(check-equal? (width (make-interval 1.0 4.0)) 1.5)

; would be nice if I understood enough LISP to express this in function of the make-interval constructor instead
(define arbitrary-interval (arbitrary-pair arbitrary-rational arbitrary-rational)) 

;(2,3) + (4,6) = (6,9)
;widths: 1 + 2 = 3
(define width-of-addition-is-sum-of-widths
  (property ([fst arbitrary-interval]
             [snd arbitrary-interval])
            (equal? (+ (width fst) (width snd)) (width (add-interval fst snd)))))
(check-property width-of-addition-is-sum-of-widths)

; (1, 2) * (3,4) = (3, 8)
; widths: 1 1 -> 5
; (1.5, 2) * (-1, 1) = (-2, 2)
; widths: 0.5 2 -> 4
#|
(define width-of-multiplication-is-not-sum-of-widths
  (property ([fst arbitrary-interval]
             [snd arbitrary-interval])
            (equal? (+ (width fst) (width snd)) (width (mul-interval fst snd)))))
(check-property width-of-multiplication-is-not-sum-of-widths)
|#