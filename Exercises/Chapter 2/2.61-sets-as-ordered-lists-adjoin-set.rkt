#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) (adjoin-set x (cdr set)))]))

(require rackunit)

;breaking encapsulation here, YOLO. Could do this with constructors/selectors if we had a size or something.
(check-equal? (adjoin-set 1 '())
              '(1))
(check-equal? (adjoin-set 1 '(2 3))
              '(1 2 3))
(check-equal? (adjoin-set 2 '(1 3))
              '(1 2 3))
(check-equal? (adjoin-set 3 '(1 2))
              '(1 2 3))
(check-equal? (adjoin-set 1 '(1 2 3))
              '(1 2 3))
(check-equal? (adjoin-set 2 '(1 2 3))
              '(1 2 3))
(check-equal? (adjoin-set 3 '(1 2 3))
              '(1 2 3))