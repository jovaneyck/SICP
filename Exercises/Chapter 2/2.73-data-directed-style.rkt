#lang racket

;Mutable state hack to try this data-directed stuff out
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


;exercise
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;let's define the necessary procedures
(define (install-deriv-package)
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  
  
  (define (make-sum a1 a2)
    (cond
      ((=number? a1 0) a2)
      ((=number? a2 0) a1)
      ((and (number? a1) (number? a2)) (+ a1 a2))
      (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  
  
  (define (deriv-sum exp var)
    (make-sum (deriv (car exp) var)
              (deriv (cadr exp) var)))
  
  (define (deriv-product exp var)
    (make-sum
     (make-product (car exp)
                   (deriv (cadr exp) var))
     (make-product (deriv (car exp) var)
                   (cadr exp))))
  
  
  ;install them in the table
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product))
(install-deriv-package)

;test it all
(require rackunit)

(put 'inc 'number (λ (x) (+ x 1)))
(check-equal? ((get 'inc 'number) 5) 6)

(check-equal? (deriv 3 'x) 0)
(check-equal? (deriv 'x 'x) 1)
(check-equal? (deriv '(+ x 5) 'x) 1)
(check-equal? (deriv '(* 3 x) 'x) 3)

(println "Done")