#lang racket

(require rackunit)

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (foldr append null (map proc seq)))

(define (triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k) (list i j k))
         (enumerate-interval 1 (- j 1))))
      (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(check-equal?
 (triples 5)
 '((3 2 1) (4 2 1) (4 3 1) (4 3 2) (5 2 1) (5 3 1) (5 3 2) (5 4 1) (5 4 2) (5 4 3)))

(define (triples-with-sum n s)
  (filter
   (λ (triple) (= s (apply + triple)))
   (triples n)))

(check-equal?
 (triples-with-sum 6 9)
 (list (list 4 3 2) (list 5 3 1) (list 6 2 1)))

(println "Done")