#lang racket

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append
         ;cdr subsets without current car
         rest
         ;cdr subsets WITH current car
         (map (λ (subset) (cons (car s) subset)) rest)))))

(require rackunit)

(check-equal?
 (subsets (list 1 2 3))
 '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
(println "Done")