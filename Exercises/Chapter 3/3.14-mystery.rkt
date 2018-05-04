#lang racket

(require r5rs)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;looks like a tail-recursive reverse with mutation thrown in for lols
(require rackunit)

(print "Testing...")

(check-equal? (mystery (list 1 2 3)) (list 3 2 1))
(check-equal? (mystery '()) '())

(define v (list 'a 'b 'c 'd))
(define w (mystery v))

(check-equal? w (list 'd 'c 'b 'a))
(check-equal? v (list 'a)) ;YIKES. Who would want to work with destructive code like this?

(println "..done!")