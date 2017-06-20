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

(define (make-generic-record division record) (cons division record))
(define (get-record-division record) (car record))
(define (get-record-contents record) (cdr record))

;division 1
;just a list stored in a list
(define (make-customer1 key salary) (cons key salary))
(define file1 (make-file 'division1
                         (list (make-customer1 "Alice" 3000)
                               (make-customer1 "David" 2500))))

(define (install-division1-operations)
  (define (get-key record)
    (car record))
  (define (get-record key file)
    (let ([raw-record (assf (λ (k) (eq? key k)) file)]) ;assf similar to LINQ [(kvp);(kvp)].Single(key => predicate(key))
      (make-generic-record 'division1 raw-record)))
  (define (get-salary record)
    (cdr record))
  
  (put 'get-record 'division1 get-record)
  (put 'get-salary 'division1 get-salary))
(install-division1-operations)

;division 2
;a key-value map stored in a tree
(define (make-customer2 key salary)
  (list
   (cons 'key key)
   (cons 'salary salary)))

(define empty-tree '())
(define (empty-tree? t) (equal? empty-tree t))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (make-leaf entry) (make-tree entry empty-tree empty-tree))

(define file2 (make-file 'division2
                         (make-tree (make-customer2 "Alice" 3100)
                                    (make-leaf (make-customer2 "Bob" 2900))
                                    empty-tree)))

(define (install-division2-operations)
  (define (get-key record) (cdr (assoc 'key record)))
  
  (define empty-set empty-tree)
  (define (empty-set? s) (equal? empty-set s))
  (define (lookup the-key set)
    (cond [(empty-set? set) #f]
          [(eq? the-key (get-key (entry set))) (entry set)]
          [(< (string-length the-key) (string-length (get-key (entry set)))) (lookup the-key (left-branch set))]
          [else                          (lookup the-key (right-branch set))]))
  
  (define (get-record key file)
    (make-generic-record 'division2 (lookup key file)))
  
  (define (get-salary record)
    (cdr (assoc 'salary (cdr record))))
  
  (put 'get-record 'division2 get-record)
  (put 'get-salary 'division2 get-salary))
(install-division2-operations)

;generic procedures
;a)
(define (get-record key file)
  (let ([operator (get 'get-record (get-division file))]
        [contents (get-contents file)])
    (operator key contents)))
;b)
(define (get-salary record)
  (let ([operator (get 'get-salary (get-record-division record))]
        [contents (get-record-contents record)])
    (operator contents)))
;c)
(define (find-employee-record name files)
  (let ([hits (filter (λ (hit) (not (false? (cdr hit))))
                      (map (λ (file) (get-record name file))
                           files))])
    (if (null? hits)
        #f
        (car hits))))
;d)
;* Install get-record for their division which returns a generic record (which contains their division type tag)
;* Install get-salary for their division which just returns a salary from a record in their format

(require rackunit)

(check-equal? (get-record "David" file1) (make-generic-record 'division1 (make-customer1 "David" 2500)))
(check-equal? (get-record "Alice" file2) (make-generic-record 'division2 (make-customer2 "Alice" 3100)))

(check-equal? (get-salary (get-record "David" file1)) 2500)
(check-equal? (get-salary (get-record "Alice" file2)) 3100)

(check-equal? (find-employee-record "Alice" (list file1 file2)) (make-generic-record 'division1 (make-customer1 "Alice" 3000)))
(check-equal? (find-employee-record "David" (list file1 file2)) (make-generic-record 'division1 (make-customer1 "David" 2500)))
(check-equal? (find-employee-record "Bob" (list file1 file2)) (make-generic-record 'division2 (make-customer2 "Bob" 2900)))
(check-false  (find-employee-record "I don't exist" (list file1 file2)))

(println "Done")