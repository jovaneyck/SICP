#lang racket
;simplification: only + and * are supported operators so I don't have too much trouble deciding precedence

(require rackunit)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond
    ((=number? a1 0) a2)
    ((=number? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list a1 '+ a2))))

(define (contains x xs)
  (not (eq? #f (member x xs))))

(define (sum? x)
  (and (pair? x)
       (contains '+ x)))
(define (addend s) (car s))
(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (not (sum? x)))

(define (multiplier p) (car p))

(define (multiplicand p)
 (caddr p))

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [(sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))]
        [(product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))]
        [else
         (error "unknown expression type -- DERIV" exp)]))

(check-true (sum? (make-sum 'x 'y)))
(check-equal? (addend (make-sum 'y 'z)) 'y)
(check-equal? (augend (make-sum 'y 'z)) 'z)
(check-equal? (deriv (make-sum 'x 3) 'x) 1)

(check-true (product? (make-product 'x 'y)))
(check-equal? (multiplier (make-product 'y 'z)) 'y)
(check-equal? (multiplicand (make-product 'y 'z)) 'z)
(check-equal? (deriv (make-product 'x 'y) 'x) 'y)

(check-equal?
 (deriv '((x * y) * (x + 3)) 'x)
 '((x * y) + (y * (x + 3))))

;2.58 b: parentheses are optional + operator precedence

(check-true (sum? '(x + y + z)))
(check-true (sum? '(x + y * z)))
(check-true (sum? '(x * y + z)))

(check-equal? (addend '(x * y + z)) '(x * y))
(check-equal? (augend '(x + y * z))'(y * z))

(check-true (product? '(x * y * z)))
(check-false (product? '(x * y + z)))
(check-false (product? '(x + y * z)))

;(check-equal? (deriv '(x + 3 * (x + y + 2)) 'x)
;              4)

(println "Done")