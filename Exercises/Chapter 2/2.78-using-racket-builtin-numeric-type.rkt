#lang racket

;infrastructure
(define (square n) (expt n 2))

(define operations-table '())
(define (put op type item)
  (set! operations-table (cons (list op type item) operations-table)))
(define (get op type)
  (let [(operation (filter (λ (operation) (and (equal? op (car operation))
                                               (equal? type (cadr operation))))
                           operations-table))]
    (if (null? operation)
        (error "Did not find operation" op type)
        (caddar operation))))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) ;don't wrap
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond
    [(number? datum) 'scheme-number] ;isn't part of the data
    [(pair? datum) (car datum)]
    [else (error "Bad tagged datum -- TYPE-TAG" datum)]))
(define (contents datum)
  (cond
    [(number? datum) datum] ;isn't wrapped
    [(pair? datum) (cdr datum)]
    [else (error "Bad tagged datum -- CONTENTS" datum)]))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;generic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;implementations
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x))))
(install-scheme-number-package)

(require rackunit)

;consumers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(check-equal? (add (make-scheme-number 1) (make-scheme-number 2)) (make-scheme-number 3))

(println "Done")