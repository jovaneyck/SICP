#lang racket

(require rackunit)

(define (square n) (* n n))

(define (square-list-wrong items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(define (square-list-wrong2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items null))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        (reverse answer) ;tadaa
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items null))

(check-equal? (square-list-wrong (list 1 2 3)) (list 9 4 1)) ;Whoops, reversed!
(check-equal? (square-list-wrong2 (list 1 2 3)) '(((() . 1) . 4) . 9)) ;Whoops, prepended with empty list!
;What's happening here:
(check-equal? (cons 1 null) '(1))
(check-equal? (cons null 1) '(() . 1))

(check-equal? (square-list (list 1 2 3)) (list 1 4 9))

(println "Done")