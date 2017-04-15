#lang racket

(define (square n) (* n n))

(define (square-list-1 items)
  (if (null? items)
      null
      (cons
       (square (car items))
       (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

(require rackunit)

(check-equal? (square-list-1 (list 1 2 3 4)) (list 1 4 9 16))
(check-equal? (square-list-2 (list 1 2 3 4)) (list 1 4 9 16))
(println "Done")
