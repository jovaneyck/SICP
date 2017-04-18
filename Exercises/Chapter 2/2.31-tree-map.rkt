#lang racket

(define (square n) (expt n 2))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree))
             (f sub-tree)
             (tree-map f sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))

(require rackunit)

(check-equal?
 (square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))
 '(1 (4 (9 16) 25) (36 49)))

(println "Done")