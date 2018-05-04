#lang racket

(require r5rs)

#|
Racket aliasing: this case asks for eq? which is basically pointer equality
"One way to detect sharing in list structures is to use the predicate eq?"

> (eq? (list 1) (list 1))
#f
> (equal? (list 1) (list 1))
#t
> 
|#

(define (count-pairs l)
  (define visited '())
  (define (counter l)
    (cond [(not (pair? l)) 0]
          [(memq l visited) 0]
          [else
           (begin
             (set! visited (cons l visited)) ;mutable staaaaate, but passing through an acc in multiple recursive calls is a bit more work
             (+ 1 (counter (car l)) (counter (cdr l))))]))
  (counter l))

(check-equal? (count-pairs (list 1 2)) 2)
(check-equal? (count-pairs (list 1 (list 2))) 3)


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
;four-one
;four-one -> (mcons (mcons 'a '()) (mcons (mcons 'a '()) '()))

(check-equal? (count-pairs four-one) 3)

(define infinite (list 1 2))
(set-cdr! infinite infinite)

(check-equal? (count-pairs infinite) 1) ;this "just works" (tm)

(println "..done")