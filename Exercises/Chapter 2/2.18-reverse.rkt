#lang racket

(define (reverse l)
  (define (rev-iter acc l)
    (if (null? l)
        acc
        (rev-iter (cons (car l) acc) (cdr l))))
  (rev-iter null l))

(require rackunit)

(check-equal? (reverse null) null)
(check-equal? (reverse (list 1 2 3)) (list 3 2 1))

(println "Done")