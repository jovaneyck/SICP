#lang racket

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (rand-integer) (random 1 1000000))

(define (cesaro-test)
  (= (gcd (rand-integer) (rand-integer)) 1))

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

(require rackunit)
(print "Testing...")
(check-= (estimate-pi 1000000) 3.14 0.01)
(println "..done")
