#lang racket

#|
Read more at 
https://www.wisdomandwonder.com/article/6601/drracket-for-the-truly-impatient-v02
|#

(require "atom.rkt")
(require rackunit)
 
(check-eq? #f (atom? '()))
 
(check-eq? #f (atom? '(a)))
 
(check-eq? #f (atom? '(a . b)))
 
(check-eq? #t (atom? 'a))

(println "test run complete")