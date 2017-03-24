#lang racket

(define (repeated f n)
  (define (iter n acc)
    (if (= n 1)
      acc
      (iter (- n 1) (compose f acc))))
  (iter n f))

(require rackunit)

(define (inc n) (+ n 1))
(define (square n) (* n n))

(check-eq? ((repeated square 2) 5) 625)
(check-eq? ((repeated square 2) 2) 16)

(check-eq? ((repeated inc 10) 5) 15)
(println "Done")