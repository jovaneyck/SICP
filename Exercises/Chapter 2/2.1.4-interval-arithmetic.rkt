#lang racket

(require rackunit)
(require quickcheck)
(require rackunit/quickcheck)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;2.11 - let's rework multiplication!
(define (mul-interval x y)
  (define (pos? n) (or (positive? n) (zero? n)))
  (define (neg? n) (negative? n))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond
      [(and (pos? lx) (pos? ux) (pos? ly) (pos? uy))
       (make-interval (* lx ly) (* ux uy))]
      [(and (neg? lx) (neg? ux) (neg? ly) (neg? uy))
       (make-interval (* ux uy) (* lx ly))]
      [(and (neg? lx) (neg? ux) (pos? ly) (pos? uy))
       (make-interval (* lx uy) (* ux ly))]
      [(and (pos? lx) (pos? ux) (neg? ly) (neg? uy))
       (make-interval (* ux ly) (* lx uy))]
      [(and (pos? lx) (pos? ux) (neg? ly) (pos? uy))
       (make-interval (* ux ly) (* ux uy))]
      [(and (neg? lx) (pos? ux) (pos? ly) (pos? uy))
       (make-interval (* lx uy) (* ux uy))]
      [(and (neg? lx) (neg? ux) (neg? ly) (pos? uy))
       (make-interval (* lx uy) (* lx ly))]
      [(and (neg? lx) (pos? ux) (neg? ly) (neg? uy))
       (make-interval (* ux ly) (* lx ly))]
      [(and (neg? lx) (pos? ux) (neg? ly) (pos? uy))
       (make-interval
        (min (* lx uy) (* ux ly))
        (max (* lx ly) (* ux uy)))])))

(define (spans-zero i)
  (and
   (<= (lower-bound i) 0)
   (<= 0 (upper-bound i))))

(define (div-interval x y)
  (if (spans-zero y)
      (error "Cannot divide by an interval spanning 0")
      (mul-interval
       x
       (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y))))))

;2.7
(define (make-interval a b)
  (if (> a b)
      (error "invalid interval" a b)
      (cons a b)))
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
   (make-interval (* -1.0 (upper-bound y)) (* -1.0 (lower-bound y)))))

(check-equal?
 (sub-interval
  (make-interval 0.96 1.05)
  (make-interval 0.01 0.02))
 (make-interval 0.94 1.04))

;2.9
(define (width interval)
  (/
   ( - (upper-bound interval) (lower-bound interval))
   2.0))

(check-equal? (width (make-interval 1.0 4.0)) 1.5)

;(2,3) + (4,6) = (6,9)
;widths: 1 + 2 = 3
(let
    ((fst (make-interval 1 2))
     (snd (make-interval 3 4)))
  (check-= (width (mul-interval fst snd)) 2.5 0.001))
(let
    ((fst (make-interval 1.5 2.0))
     (snd (make-interval -1.0 1.0)))
  (check-equal? (width (mul-interval fst snd)) 2.0))

(define generator-interval
  (generator-bind
   (choose-integer -100 100)
   (λ (lower)
     (generator-bind
      (choose-integer lower 100)
      (λ (upper) (generator-unit (make-interval lower upper)))))))

(define sum-of-widths-eq-width-of-sum
  (property ([fst generator-interval]
             [snd generator-interval])
            (equal? (+ (width fst) (width snd)) (width (add-interval fst snd)))))
(check-property sum-of-widths-eq-width-of-sum)

#|
(define width-of-multiplication-is-not-sum-of-widths
  (property ([fst arbitrary-interval]
             [snd arbitrary-interval])
            (equal? (+ (width fst) (width snd)) (width (mul-interval fst snd)))))
(check-property width-of-multiplication-is-not-sum-of-widths)
|#

;2.10
(check-equal?
 (div-interval (make-interval 1 2) (make-interval 4 8))
 (make-interval 0.125 0.5))

(check-exn
 exn:fail?
 (λ () (div-interval (make-interval 1 2) (make-interval -1 0))))
(check-exn
 exn:fail?
 (λ () (div-interval (make-interval 1 2) (make-interval 0 1))))
(check-exn
 exn:fail?
 (λ () (div-interval (make-interval 1 2) (make-interval -1 1))))

(check-equal?
 (mul-interval (make-interval -1 2) (make-interval 3 4))
 (make-interval -4 8))
(check-equal?
 (mul-interval (make-interval -2 -1) (make-interval 3 4))
 (make-interval -8 -3))
(check-equal?
 (mul-interval (make-interval 1 2) (make-interval -3 4))
 (make-interval -6 8))
(check-equal?
 (mul-interval (make-interval -2 -1) (make-interval -4 -3))
 (make-interval 3 8))
(check-equal?
 (mul-interval (make-interval -2 -1) (make-interval -3 4))
 (make-interval -8 6))
(check-equal?
 (mul-interval (make-interval 1 2) (make-interval -4 -3))
 (make-interval -8 -3))
(check-equal?
 (mul-interval (make-interval 1 2) (make-interval -3 4))
 (make-interval -6 8))
(check-equal?
 (mul-interval (make-interval -2 1) (make-interval -4 -3))
 (make-interval -4 8))
(check-equal?
 (mul-interval (make-interval -1 2) (make-interval -3 4))
 (make-interval -6 8))
(check-equal?
 (mul-interval (make-interval -2 1) (make-interval -3 4))
 (make-interval -8 6))

;2.11
(define reworked-multiplication-has-same-result
  (property ([fst generator-interval]
             [snd generator-interval])
            (equal? (mul-interval-old fst snd) (mul-interval fst snd))))
(check-property reworked-multiplication-has-same-result)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(check-equal? (center (make-center-width 40.0 3.5)) 40.0)
(check-equal? (width (make-center-width 40.0 3.5)) 3.5)

;2.12
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))
(define (percent i)
  (let (
        [c (center i)]
        [w (width i)]
        )
    ;(error "what" c w)
    (* (/ w c) 100)
    ))

(check-equal? (center (make-center-percent 3.0 10.0)) 3.0)
(check-= (percent (make-center-percent 2.0 10)) 10 0.001)
(check-equal? (percent (make-center-percent 4 25)) 25.0)
(check-equal? (make-center-percent 3.0 10) (make-interval 2.7 3.3))

(define (almost-equal? a b delta)
  (<= (abs (- a b)) delta))

(define center-works-as-expected
  (property ([c arbitrary-natural]
             [p arbitrary-natural])
            (equal? (center (make-center-percent (exact->inexact c) (exact->inexact p))) (exact->inexact c))))
(check-property center-works-as-expected)

(define percentage-works-as-expected
  (property ([c arbitrary-natural]
             [p arbitrary-natural])
            (almost-equal? (percent (make-center-percent (exact->inexact (+ c 1)) (exact->inexact p))) (exact->inexact p) 0.0001)))
(check-property percentage-works-as-expected)

;2.13
(check-= (percent (mul-interval (make-center-percent 1 3) (make-center-percent 1 7))) 10 0.1)
(check-= (percent (mul-interval (make-center-percent 1 1) (make-center-percent 1 2))) 3 0.1)
(check-= (percent (mul-interval (make-center-percent 1 0.01) (make-center-percent 1 0.01))) 0.02 0.001)

(define (guess-percent-mul a b)
  (+ (percent a) (percent b)))

(check-property
 (property ([p1 (choose-integer 0 5)]
            [p2 (choose-integer 0 5)]
            [c1 (choose-integer 1 100)]
            [c2 (choose-integer 1 100)])
           (let ([i1 (make-center-percent c1 p1)]
                 [i2 (make-center-percent c2 p2)])
             (almost-equal? (guess-percent-mul i1 i2) (percent (mul-interval i1 i2)) 0.1))))