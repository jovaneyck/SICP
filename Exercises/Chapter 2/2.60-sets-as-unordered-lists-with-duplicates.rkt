#lang racket

(define empty-set '())

;O(N)
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;O(1)
(define (adjoin-set x set)
      (cons x set))

;O(n)
(define (union-set a b)
  (append a b))

;O(N2) but N is larger than number of distinct elements
(define (intersection-set a b)
  (cond [(null? a) '()]
        [(element-of-set? (car a) b) (adjoin-set (car a) (intersection-set (cdr a) b))]
        [else (intersection-set (cdr a) b)]))

;when to use this implementation? when cost(writes) is more important than cost(reads)
;+ "memory concerns" in the era of limited memory :)
(require rackunit)

(let ([union (union-set (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 empty-set)))
                        (adjoin-set 4 (adjoin-set 3 empty-set)))])
  (check-true (element-of-set? 4 union))
  (check-true (element-of-set? 3 union))
  (check-true (element-of-set? 2 union))
  (check-false (element-of-set? 5 union)))

(check-equal? (union-set empty-set empty-set) empty-set)
(check-equal? (union-set empty-set (adjoin-set 1 empty-set)) (adjoin-set 1 empty-set))
(check-equal? (union-set (adjoin-set 1 empty-set) empty-set ) (adjoin-set 1 empty-set))

(let ([intersection (intersection-set (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 empty-set)))
                                      (adjoin-set 4 (adjoin-set 3 empty-set)))])
  (check-true (element-of-set? 3 intersection))
  (check-false (element-of-set? 4 intersection))
  (check-false (element-of-set? 2 intersection)))

(println "Done")