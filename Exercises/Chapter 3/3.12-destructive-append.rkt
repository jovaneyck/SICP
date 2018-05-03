#lang racket

(require r5rs) ;yikes, this counts as a serious discouragement for mutable state :)

(define (last-pair x)
  (if (null? (mcdr x))
      x
      (last-pair (mcdr x))))


(define (append! x y)
  (set-cdr! (last-pair x) y))

(require rackunit)
(print "Testing..")

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))   
(check-equal? (list 'a 'b 'c 'd) z)
(check-equal? (list 'b) (cdr x))    


(append! x y)
(check-equal? (list 'a 'b 'c 'd) x)
(check-equal? (list 'b 'c 'd) (cdr x))

(println "..done!")