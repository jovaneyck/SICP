#lang racket

(define (square n) (* n n))

(define (square-tree-1 t)
  (cond
    [(null? t) null]
    [(not (pair? t)) (square t)]
    [else (cons (square-tree-1 (car t)) (square-tree-1 (cdr t)))]))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (square sub-tree)
             (square-tree-2 sub-tree)))
       tree))

(require rackunit)
(define ce? check-equal?)

(ce?
 (square-tree-1
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))
(ce?
 (square-tree-2
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))

(println "Done")