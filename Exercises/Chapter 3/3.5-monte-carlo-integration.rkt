#lang racket

;unit circle: x^2 + y^2 = 1^2
;area circle: pi*r^2 (unit : pi)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
  (iter trials 0))

(define (square n) (* n n))

(define (make-point x y) (list x y))
(define point-x car)
(define point-y cadr)

(define (in-circle? p)
  (<=
   (+ (square (point-x p)) (square (point-y p)))
   1))

(define (area-rect lower-left upper-right)
  (begin
    (define rect-width (- (point-x upper-right) (point-x lower-left)))
    (define rect-height (- (point-y upper-right) (point-y lower-left)))
    (* rect-width rect-height)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (estimate-integral predicate lower-bound upper-bound trials)
  (begin
    (define (experiment)
      (begin
        (define random-x (random-in-range (point-x lower-bound) (point-x upper-bound)))
        (define random-y (random-in-range (point-y lower-bound) (point-y upper-bound)))
        (define random-point (make-point random-x random-y))

        (predicate random-point)))
        
    (define mc-result (monte-carlo trials experiment))
    (define rectangle-area (area-rect lower-bound upper-bound))
    
    (* mc-result rectangle-area)))

(define (estimate-pi)
  (estimate-integral in-circle? (make-point -1 -1) (make-point 1 1) 300000))

(require rackunit)
(require quickcheck)
(print "Testing...")

(check-true (in-circle? (make-point 0 0)))
(check-true (in-circle? (make-point 0 1)))
(check-true (in-circle? (make-point 0 -1)))
(check-true (in-circle? (make-point 1 0)))
(check-false (in-circle? (make-point 1 1)))
(check-false (in-circle? (make-point -1 -1)))

(check-eq? (area-rect (make-point 1 1) (make-point 4 6)) 15)

(define random-always-produces-values-in-range
  (property ([lower arbitrary-natural]
             [delta arbitrary-natural])
            (begin
              (define upper (+ lower delta 0.0001))
              (define random-number (random-in-range lower upper))
              (and (< random-number upper)
                   (< lower random-number)))))
(quickcheck random-always-produces-values-in-range)

(check-= (exact->inexact (estimate-pi)) 3.14 0.01)

(println "..done")