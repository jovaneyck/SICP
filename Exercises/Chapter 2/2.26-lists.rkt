#lang racket

(require rackunit)

(define x (list 1 2 3))
(define y (list 4 5 6))

(define ce check-equal?)

(ce
 (append x y) ;appends two lists as you expect
 (list 1 2 3 4 5 6))
(ce
 (cons x y) ;prepend an element (in this case list x) to the list
 (list (list 1 2 3) 4 5 6))
(ce
 (list x y) ;make a list with arguments as elements
 (list (list 1 2 3) (list 4 5 6)))
(println "Done")