#lang racket

(require rackunit)

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons empty-tree elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


(check-equal? (quotient 10 2) 5)
(check-equal? (quotient 11 2) 5)
(check-equal? (quotient 12 2) 6)

(check-equal? (list->tree '(1 3 5 7 9 11))
              (make-tree 5
                         (make-tree 1
                                    empty-tree
                                    (make-leaf 3))
                         (make-tree 9
                                    (make-leaf 7)
                                    (make-leaf 11))))

(println "Done")