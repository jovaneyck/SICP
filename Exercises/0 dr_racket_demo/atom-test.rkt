#lang racket

(require "atom.rkt")
(require rackunit)

(check-eq? #f (atom? '()))

(check-eq? #f (atom? '(a)))

(check-eq? #f (atom? '(a . b)))

(check-eq? #t (atom? 'a))

(print "all done")