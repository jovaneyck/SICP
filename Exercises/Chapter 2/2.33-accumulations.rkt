#lang racket

(require rackunit)

(define accumulate foldr)
(define <??> identity)

(define (my-map p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   null
   sequence))

(check-equal?
 (my-map (λ(x) (+ x 1)) (list 1 2 3))
 (list 2 3 4))

(define (my-append seq1 seq2)
  (accumulate
   cons
   seq2
   seq1))

(check-equal? (my-append (list 1 2) (list 3 4)) (list 1 2 3 4))

(define (my-length sequence)
 (accumulate
  (λ(_ count) (+ count 1))
  0
  sequence))

(check-equal? (my-length null) 0)
(check-equal? (my-length (list 1 1 2 3)) 4)

(println "Done")