#lang racket

(define (cont-frac n d k)
  (define (rec i)
    (if (= i k)
        0
        (/ (n i) (- (d i) (rec (+ i 1))))))
  (rec 1))

(define (tan-cf r k)
  (cont-frac
   (λ (n)
     (if (= n 1)
         r
         (* r r)))
   (λ (d) (+ d (- d 1)))
   k))

(require rackunit)
(check-= (tan-cf 0.5 10) 0.5463 0.0001)
(check-= (tan-cf 1 10) 1.5574 0.0001)
(println "Done")