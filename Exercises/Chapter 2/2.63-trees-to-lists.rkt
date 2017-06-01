#lang racket

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define (tree->list-1 tree)
  (if (empty-tree? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

;hello iterative vs incremental / tailrecursion!
;+ cons vs. append
(define (tree->list-2 tree)
  (define (copy-to-list tree acc)
    (if (empty-tree? tree)
        acc
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          acc)))))
  (copy-to-list tree '()))

(require rackunit)

(check-equal? (tree->list-1 (make-tree 2 (make-leaf 1) (make-leaf 3)))
              '(1 2 3))
(check-equal? (tree->list-2 (make-tree 2
                                       (make-tree 1 (make-leaf 0) empty-tree)
                                       (make-tree 3 empty-tree (make-leaf 4))))
              '(0 1 2 3 4))

(let ([expected '(1 3 5 7 9 11)]
      [t1 (make-tree 7
                     (make-tree 3 (make-leaf 1) (make-leaf 5))
                     (make-tree 9 empty-tree    (make-leaf 11)))]
      [t2 (make-tree 3
                     (make-leaf 1)
                     (make-tree 7
                                (make-leaf 5)
                                (make-tree 9
                                           empty-tree
                                           (make-leaf 11))))]
      [t3 (make-tree 5
                     (make-tree 3
                                (make-leaf 1)
                                empty-tree)
                     (make-tree 9
                                (make-leaf 7)
                                (make-leaf 11)))])

  (define (same-result-for tree)
    (check-equal? (tree->list-1 tree) (tree->list-2 tree))
    (check-equal? (tree->list-1 tree) expected))
  
  [same-result-for t1]
  [same-result-for t2]
  [same-result-for t3])

(println "Done")