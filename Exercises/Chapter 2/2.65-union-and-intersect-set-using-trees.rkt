#lang racket

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define (tree->list tree)
  (define (copy-to-list tree acc)
    (if (empty-tree? tree)
        acc
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          acc)))))
  (copy-to-list tree '()))

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

(define (element-of-set-list? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-list? x (cdr set)))))

(define (adjoin-set-list x set)
  (cond [(null? set) (list x)]
        [(= x (car set)) set]
        [(< x (car set)) (cons x set)]
        [else (cons (car set) (adjoin-set-list x (cdr set)))]))

(define (union-set-list one other)
  (cond [(null? one) other]
        [(null? other) one]
        [(< (car one) (car other)) (cons (car one) (union-set-list (cdr one) other))]
        [(< (car other) (car one)) (cons (car other) (union-set-list one (cdr other)))]
        [else (cons (car one) (union-set-list (cdr one) (cdr other)))]))

(define (intersection-set-list set1 set2)
  (if (or (empty-set? set1) (empty-set? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

(define empty-set empty-tree)
(define (empty-set? s) (equal? empty-set s))

(define (union-set one other)
  (list->tree (union-set-list (tree->list one) (tree->list other))))

(define (adjoin-set x set)
  (list->tree (adjoin-set-list x (tree->list set))))

(define (intersection-set s1 s2)
  (list->tree (intersection-set-list (tree->list s1)
                                     (tree->list s2))))

(require rackunit)

(check-equal? (union-set empty-set empty-set) empty-set)
(check-equal? (union-set (adjoin-set 1 empty-set) empty-set) (adjoin-set 1 empty-set))
(check-not-equal? (union-set (adjoin-set 1 empty-set) empty-set) empty-set)
(check-equal? (union-set empty-set (adjoin-set 1 empty-set)) (adjoin-set 1 empty-set))
(check-equal? (union-set (adjoin-set 1 empty-set) (adjoin-set 2 empty-set)) (adjoin-set 1 (adjoin-set 2 empty-set)))
(check-equal? (union-set (adjoin-set 2 empty-set) (adjoin-set 1 empty-set)) (adjoin-set 1 (adjoin-set 2 empty-set)))
(check-equal? (union-set (adjoin-set 1 empty-set) (adjoin-set 1 empty-set)) (adjoin-set 1 empty-set))
(check-equal? (union-set (adjoin-set 1 (adjoin-set 2 (adjoin-set 3 empty-set)))
                         (adjoin-set 2 (adjoin-set 3 empty-set)))
              (adjoin-set 1 (adjoin-set 2 (adjoin-set 3 empty-set))))
(check-equal? (union-set (adjoin-set 1 (adjoin-set 2 (adjoin-set 4 (adjoin-set 5 empty-set))))
                         (adjoin-set 0 (adjoin-set 2 (adjoin-set 3 (adjoin-set 5 (adjoin-set 6 empty-set))))))
              (adjoin-set 0 (adjoin-set 1 (adjoin-set 2 (adjoin-set 3 (adjoin-set 4 (adjoin-set 5 (adjoin-set 6 empty-set))))))))

(check-equal? (intersection-set empty-set empty-set) empty-set)
(check-equal? (intersection-set empty-set (adjoin-set 1 empty-set)) empty-set)
(check-equal? (intersection-set (adjoin-set 1 empty-set) empty-set) empty-set)
(check-equal? (intersection-set (adjoin-set 1 empty-set) (adjoin-set 1 empty-set)) (adjoin-set 1 empty-set))
(check-equal? (intersection-set (adjoin-set 1 (adjoin-set 2 empty-set)) (adjoin-set 2 (adjoin-set 3 empty-set)))
              (adjoin-set 2 empty-set))

(println "Done")