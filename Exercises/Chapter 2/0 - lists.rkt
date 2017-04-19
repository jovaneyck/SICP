#lang racket

(require rackunit)

(define l (list 1 2 3 4))

;alternatively
(check-equal? (list 1 2 3) '(1 2 3))

;all the expected procedures are there:
(check-equal? (length l) 4)
(check-equal? (car l) 1)
(check-equal? (cdr l) (list 2 3 4))
(check-equal? (list-ref l 2) 3) ;nth
(check-equal? (append l l) (list 1 2 3 4 1 2 3 4))
(check-equal? (reverse l) (list 4 3 2 1))
(check-equal? (filter even? (list 1 2 3 4)) (list 2 4))
(check-equal? (map (λ (x) (+ x 1)) (list 1 2 3)) (list 2 3 4))
(check-equal? (foldl + 0 (list 1 2 3 4)) 10)

;recursively processing a list:
(define (inc list)
  (if (null? list)
      null
      (cons (+ 1 (car list)) (inc (cdr list)))))
(check-equal? (inc (list 2 5 1)) (list 3 6 2))

(println "Done")