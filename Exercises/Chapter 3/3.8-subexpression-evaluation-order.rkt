#lang racket

(define (make-f)
  (define first-input #t)
  (define state -1)
  (define (f x)
    (cond [first-input (begin
                         (set! first-input #f)
                         (set! state x)
                         x)]
          [else state]))
  f)
  

(require rackunit)

(print "Testing..")

;left-to-right
(let [(f (make-f))]
  (check-eq? (+ (f 0) (f 1)) 0))

;"right to left"
(let [(f (make-f))]
  (check-eq? (+ (f 1) (f 0)) 2)) ; not exactly the exercise, but I get it

(println "..done!")