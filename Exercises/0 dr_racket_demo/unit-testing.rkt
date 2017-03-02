#lang racket

(require rackunit)

(check-eq? 3 (+ 1 2))

(check-not-eq? 'a 'b)

(check-pred string? "I am a string")

(check-true #t)

(check-match (list 1 2 3) (list _ _ 3))

(print "All done!")