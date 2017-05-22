#lang racket

(require rackunit)

(define (my-equal? a b)
  (cond
    [ (and (null? a) (null? b)) #t]
    [(and (list? a) (list? b)) (and (my-equal? (car a) (car b))
                                   (my-equal? (cdr a) (cdr b)))]
    [else (eq? a b)]))

(check-true
 (my-equal? '() '()))

(check-true
 (my-equal? '(1 2 3) '(1 2 3)))

(check-false
 (my-equal? '(1 2 3) '(3 2 1)))

(check-true
 (my-equal? '(this is a list) '(this is a list)))

(check-false
 (my-equal? '(this is a list) '(this (is a) list)))

(println "Done")