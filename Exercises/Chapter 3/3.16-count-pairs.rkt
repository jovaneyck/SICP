#lang racket

(require r5rs)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(require rackunit)
(print "Testing...")

;yup, this works as expected on immutable structures
(check-equal? (count-pairs '()) 0)
(check-equal? (count-pairs (list 'a)) 1)
(check-equal? (count-pairs (list 'a 'b 'c)) 3)
(check-equal? (count-pairs (list (list 'a 'b) 'c)) 4)

;see what happens when we introduce set-car! to the picture
(define three (list (list (list 'a))))
(check-equal? (count-pairs three) 3)

(define four-one (list 'b 'c))
(define four-two (list 'a))
(set-car! four-one four-two)
(set-car! (cdr four-one) four-two)
;four-one -> (mcons (mcons 'a '()) (mcons (mcons 'a '()) '()))

(check-equal? (count-pairs four-one) 4)

(define infinite (list 1 2))
(set-cdr! infinite infinite)

;(count-pairs infinite)

(println "..done")