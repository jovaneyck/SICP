#lang racket

(require rackunit)

(define l (list 1 2 3 4))

(check-equal? (length l) 4)
(check-equal? (car l) 1)
(check-equal? (cdr l) (list 2 3 4))
(check-equal? (list-ref l 2) 3) ;nth
(check-equal? (append l l) (list 1 2 3 4 1 2 3 4))
(check-equal? (reverse l) (list 4 3 2 1))
(println "Done")