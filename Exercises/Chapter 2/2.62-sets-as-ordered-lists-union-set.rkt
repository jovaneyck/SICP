#lang racket

(define (union-set one other)
  (cond [(null? one) other]
        [(null? other) one]
        [(< (car one) (car other)) (cons (car one) (union-set (cdr one) other))]
        [(< (car other) (car one)) (cons (car other) (union-set one (cdr other)))]
        [else (cons (car one) (union-set (cdr one) (cdr other)))]))

(require rackunit)

(check-equal? (union-set '() '()) '())
(check-equal? (union-set '(1) '()) '(1))
(check-equal? (union-set '() '(1)) '(1))
(check-equal? (union-set '(1) '(2)) '(1 2))
(check-equal? (union-set '(2) '(1)) '(1 2))
(check-equal? (union-set '(1) '(1)) '(1))
(check-equal? (union-set '(1 2 3) '(2 3)) '(1 2 3))

(check-equal? (union-set '(1 2 4 5)
                         '(0 2 3 5 6))
              '(0 1 2 3 4 5 6))

(println "Done")