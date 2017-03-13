#lang racket

(define (cont-frac n d k)
  (define (rec i acc)
    (if (= 0 i)
        acc
        (rec (- i 1) (/ (n i) (+ (d i) acc)))))
  (rec k 0))

(define k 7)

(define (d i)
  (cond
    ((= 0 (remainder (+ i 1) 3)) (* 2 (/ (+ i 1) 3)))
    (else 1)))

(define solution
  (+
   2
   (cont-frac
    (λ i 1.0)
    d
    k)))

(require rackunit)

(define e 2.71828)
(check-= solution e 0.0001)

(println "Done")