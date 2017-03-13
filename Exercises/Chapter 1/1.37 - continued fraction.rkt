#lang racket

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

(define (cont-frac-iter n d k)
  (define (rec i acc)
    (if (= 0 i)
        acc
        (rec (- i 1) (/ (n i) (+ (d i) acc)))))
  (rec k 0))

(define k 11)
(define solution
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

(define solution-rec
  (cont-frac-iter
   (lambda (i) 1.0)
   (lambda (i) 1.0)
   k))

(require rackunit)

(check-= solution 0.6180 0.0001)
(check-= solution-rec 0.6180 0.0001)
(println "Done")