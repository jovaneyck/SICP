#lang racket

(require rackunit)

(define ce check-equal?)

(define (fringe tree)
  (cond
    [(null? tree) null]
    [(not (pair? tree)) (list tree)]
    [else
     (append
      (fringe (car tree))
      (fringe (cdr tree)))]))

(define x (list (list 1 2) (list 3 4)))
(ce (fringe x) (list 1 2 3 4))
(ce (fringe (list x x)) (list 1 2 3 4 1 2 3 4))
(ce (fringe (list (list 1 (list 2 3) 4 (list 5 6)))) (list 1 2 3 4 5 6))

;note: builtin flatten exists:
(ce (flatten (list x x)) (list 1 2 3 4 1 2 3 4))

(println "Done")