#lang racket
(define table '())
(define (put op type item)
  (set! table (cons (list op type item) table)))
(define (get op type)
  (let [(operation (filter (λ (operation) (and (equal? op (car operation))
                                               (equal? type (cadr operation))))
                           table))]
    (if (null? operation)
        (error "Did not find operation" op type)
        (caddr (car operation)))))

(define (make-file division contents) (cons division contents))
(define (get-division file) (car file))
(define (get-contents file) (cdr file))

;division 1
;just a list stored in a list
(define (make-customer1 key salary) (cons key salary))
(define file1 (make-file 'division1
                         (list (make-customer1 "Alice" 300)
                               (make-customer1 "David" 250))))

(define (install-division1-operations)
  (define (get-key record)
    (car record))
  (define (get-record key file)
    (car (filter (λ (e) (eq? key (get-key e))) file)))
  
  (put 'get-record 'division1 get-record))
(install-division1-operations)

;division 2
;a key-value map stored in a tree
(define (make-customer2 key salary) (cons key
                                          (list (cons 'salary salary))))

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define file2 (make-file 'division2
                         (make-tree (make-customer2 "Alice" 310)
                                    (make-leaf (make-customer2 "Bob" 290))
                                    empty-tree)))

(define (install-division2-operations)
  (define (get-key record) (car record))
  
  (define empty-set empty-tree)
  (define (empty-set? s) (equal? empty-set s))
  (define (lookup the-key set)
    (cond [(empty-set? set) #f]
          [(eq? the-key (get-key (entry set))) (entry set)]
          [(< (string-length the-key) (string-length (get-key (entry set)))) (lookup the-key (left-branch set))]
          [else                          (lookup the-key (right-branch set))]))
  
  (define (get-record key file)
    (lookup key file))
  
  (put 'get-record 'division2 get-record))
(install-division2-operations)

;generic procedures
;a)
(define (get-record key file)
  (let ([operator (get 'get-record (get-division file))]
        [contents (get-contents file)])
    (operator key contents)))

(require rackunit)

(check-equal? (get-record "David" file1) (make-customer1 "David" 250))
(check-equal? (get-record "Alice" file2) (make-customer2 "Alice" 310))
(println "Done")