#lang racket

(define accumulate foldr)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(require rackunit)

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(define expected '(22 26 30))

(check-equal? (accumulate-n + 0 s) expected)
(println "Done")