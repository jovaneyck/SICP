#lang racket

(require r5rs)

(define (contains-cycle l)
  (define visited '())
  (define (recur l)
    (cond [(null? l) #f]
          [(memq l visited) #t]
          [else (begin
                  (set! visited (cons l visited))
                  (recur (cdr l)))]))
  (recur l))

(require rackunit)
(print "Testing..")

(check-false (contains-cycle '()))
(check-false (contains-cycle (list 1 (list 1) 2)))

(begin
  (define cycle (list 2))
  (set-cdr! cycle cycle)
  (check-true (contains-cycle cycle)))

(println "..done!")