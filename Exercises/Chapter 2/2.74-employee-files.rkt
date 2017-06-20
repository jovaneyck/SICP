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
;just a list
(define (make-customer1 key salary) (cons key salary))
(define file1 (make-file 'division1
                         (list (make-customer1 "Alice" 300)
                               (make-customer1 "Bob" 250))))

(define (install-division1-operations)
  (define (get-key record)
    (car record))
  (define (get-record key file)
    (car (filter (λ (e) (eq? key (get-key e))) file)))

  (put 'get-record 'division1 get-record))
(install-division1-operations)

;division 2
;a key-value map
(define (make-customer2 key salary) (cons key
                                          (list (cons 'salary salary))))
(define file2 (make-file 'division2
                         (list (make-customer2 "Alice" 310)
                               (make-customer2 "David" 290))))

(define (install-division2-operations)
  (define (get-key record)
    (car record))
  (define (get-record key file)
    (car (filter (λ (e) (eq? key (get-key e))) file)))

  (put 'get-record 'division2 get-record))
(install-division2-operations)

;generic procedures
;a)
(define (get-record key file)
  (let ([operator (get 'get-record (get-division file))]
        [contents (get-contents file)])
    (operator key contents)))
  

(require rackunit)

(check-equal? (get-record "Bob" file1) (make-customer1 "Bob" 250))
(check-equal? (get-record "Alice" file2) (make-customer2 "Alice" 310))
(println "Done")