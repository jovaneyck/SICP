#lang racket

(define empty-set '())

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))


(define (union-set a b)
  (if (null? a)
      b
      (if (element-of-set? (car a) b)
          (union-set (cdr a) b)
          (union-set (cdr a) (cons (car a) b)))))

(require rackunit)

(check-equal?
 (union-set (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 empty-set)))
            (adjoin-set 4 (adjoin-set 3 empty-set)))
 (adjoin-set 1 (adjoin-set 2 (adjoin-set 4 (adjoin-set 3 empty-set)))))
(check-equal? (union-set empty-set empty-set) empty-set)
(check-equal? (union-set empty-set (adjoin-set 1 empty-set)) (adjoin-set 1 empty-set))
(check-equal? (union-set (adjoin-set 1 empty-set) empty-set ) (adjoin-set 1 empty-set))

(println "Done")