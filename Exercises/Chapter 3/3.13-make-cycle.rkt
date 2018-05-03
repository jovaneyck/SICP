#lang racket

(require r5rs)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(require rackunit)
(print "Testing..")

(define z (make-cycle (list 'a 'b 'c)))

(check-equal? (car z) 'a)
(check-equal? (cadddr z) 'a)

;(last-pair z) ;ye olde infinite loop

(println "..done!")