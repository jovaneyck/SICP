#lang racket

(define (evens l)
  (if (null? l)
      null
      (if (even? (car l))
          (cons (car l) (evens (cdr l)))
          (evens (cdr l)))))
(define (odds l)
  (if (null? l)
      null
      (if (odd? (car l))
          (cons (car l) (odds (cdr l)))
          (odds (cdr l)))))


(define (same-parity . numbers)
  (if (null? numbers)
      null
      (if (even? (car numbers))
          (cons (car numbers) (evens (cdr numbers)))
          (cons (car numbers) (odds (cdr numbers))))))

(require rackunit)

(check-equal? (same-parity 2) (list 2))
(check-equal? (same-parity 1 2) (list 1))
(check-equal? (same-parity 1 2 3) (list 1 3))
(check-equal? (same-parity 2 1 3 4 7 6) (list 2 4 6))