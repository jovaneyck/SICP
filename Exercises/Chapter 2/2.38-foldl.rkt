#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;WARNING: This is NOT a standard foldl as in i.e. F# and Haskell!
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(require rackunit)

(check-equal? (fold-right / 1 (list 1 2 3)) (/ 3 2))
(check-equal? (fold-left / 1 (list 1 2 3)) (/ 1 6))
(check-equal? (fold-right list null (list 1 2 3)) (list 1 (list 2 (list 3 null))))
(check-equal? (fold-left list null (list 1 2 3)) (list (list (list null 1) 2) 3))

(println "Done")