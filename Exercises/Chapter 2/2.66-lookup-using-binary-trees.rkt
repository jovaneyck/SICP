#lang racket

(define (make-customer id name) (cons id name))
(define (key customer) (car customer))

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define empty-set empty-tree)
(define (empty-set? s) (equal? empty-set s))
(define (lookup the-key set)
  (cond [(empty-set? set) #f]
        [(= the-key (key (entry set))) (entry set)]
        [(< the-key (key (entry set))) (lookup the-key (left-branch set))]
        [else                          (lookup the-key (right-branch set))]))

(require rackunit)

(define database
  (make-tree
   (make-customer 2 "Bob")
   (make-leaf (make-customer 1 "Jo"))
   (make-leaf (make-customer 3 "Alice"))))

;mixing types :(((
(check-false (lookup 1 empty-set))
(check-equal? (lookup 2 database) (make-customer 2 "Bob"))
(check-equal? (lookup 1 database) (make-customer 1 "Jo"))
(check-equal? (lookup 3 database) (make-customer 3 "Alice"))

(println "Done")