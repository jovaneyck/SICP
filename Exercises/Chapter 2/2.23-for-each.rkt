#lang racket

(define (print-and-return d)
  (println d)
  #t)

(define (for-each f l)
  (if (null? l)
      void ;racket's aptly named void
      (begin ;multiple statements
        (void (f (car l))) ; F#'s ignore
        (for-each f (cdr l)))))

(require rackunit)
(check-true (print-and-return "nope"))

(for-each print-and-return (list 1 2 3 4))