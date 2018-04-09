#lang racket

;infrastructure
(define (square n) (expt n 2))

(define coercion-table '())
(define (put-coercion from-type to-type proc)
  (set! coercion-table (cons (list from-type to-type proc) coercion-table)))
(define (get-coercion from-type to-type)
  (let [(entry (filter (λ (entry) (and (equal? from-type (car entry))
                                       (equal? to-type (cadr entry))))
                       coercion-table))]
    (if (null? entry)
        #f
        (caddr (car entry)))))

(define operations-table '())
(define (put op type item)
  (set! operations-table (cons (list op type item) operations-table)))
(define (get op type)
  (let [(operation (filter (λ (operation) (and (equal? op (car operation))
                                               (equal? type (cadr operation))))
                           operations-table))]
    (if (null? operation)
        #f
        (caddr (car operation)))))

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;revised apply-generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No coercion between these types"
                                (list type-tags))))))
              (error "Coercion only works on 2-arg procedures"
                     (list op type-tags)))))))

;generic procedures
(define (real-part-custom c) (apply-generic 'real-part-custom c))
(define (imag-part-custom c) (apply-generic 'imag-part-custom c))
(define (magnitude-custom c) (apply-generic 'magnitude-custom c))
(define (angle-custom c) (apply-generic 'angle-custom c))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
;2.81
(define (exp x y) (apply-generic 'exp x y))

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
  (put 'exp '(scheme-number scheme-number)
       (λ (x y) (tag (expt x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (λ (x y) (= x y)));just aliasing = also works
  (put '=zero? '(scheme-number)
       (λ (n) (= 0 n)))) 
(install-scheme-number-package)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? n)
    (= 0 (numer n)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (equ-rat? x y)))
  (put '=zero? '(rational)
       (λ (n) (=zero-rat? n))))
(install-rational-package)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part-custom z) (car z))
  (define (imag-part-custom z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude-custom z)
    (sqrt (+ (square (real-part-custom z))
             (square (imag-part-custom z)))))
  (define (angle-custom z)
    (atan (imag-part-custom z) (real-part-custom z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part-custom '(rectangular) real-part-custom)
  (put 'imag-part-custom '(rectangular) imag-part-custom)
  (put 'magnitude-custom '(rectangular) magnitude-custom)
  (put 'angle-custom '(rectangular) angle-custom)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))
(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude-custom z) (car z))
  (define (angle-custom z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part-custom z)
    (* (magnitude-custom z) (cos (angle-custom z))))
  (define (imag-part-custom z)
    (* (magnitude-custom z) (sin (angle-custom z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part-custom '(polar) real-part-custom)
  (put 'imag-part-custom '(polar) imag-part-custom)
  (put 'magnitude-custom '(polar) magnitude-custom)
  (put 'angle-custom '(polar) angle-custom)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))))
(install-polar-package)


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part-custom z1) (real-part-custom z2))
                         (+ (imag-part-custom z1) (imag-part-custom z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part-custom z1) (real-part-custom z2))
                         (- (imag-part-custom z1) (imag-part-custom z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude-custom z1) (magnitude-custom z2))
                       (+ (angle-custom z1) (angle-custom z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude-custom z1) (magnitude-custom z2))
                       (- (angle-custom z1) (angle-custom z2))))
  (define (equ-complex? x y)
    (and (= (real-part-custom x) (real-part-custom y))
         (= (imag-part-custom x) (imag-part-custom y))))
  
  (define (=zero-complex? n)
    (and (= 0 (real-part-custom n))
         (= 0 (imag-part-custom n))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  
  (put 'real-part-custom '(complex) real-part-custom)
  (put 'imag-part-custom '(complex) imag-part-custom)
  (put 'magnitude-custom '(complex) magnitude-custom)
  (put 'angle-custom '(complex) angle-custom)
  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (λ (x y) (equ-complex? x y)))
  (put '=zero? '(complex)
       (λ (n) (=zero-complex? n))))
(install-complex-package)

(require rackunit)

;consumers
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;2.81: type coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)
(check-eq? scheme-number->complex (get-coercion 'scheme-number 'complex))

(check-true (equ? (make-scheme-number 3) (make-complex-from-real-imag 3 0)))
(check-false (equ? (make-scheme-number 3) (make-complex-from-real-imag 3 1)))

;2.81 a)
;we installed exp only in scheme-number package:
(check-equal? (make-scheme-number 32) (exp (make-scheme-number 2) (make-scheme-number 5)))

; if we try exp on complex numbers, we get coercion errors:
;(exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 5 0))
; ERROR: No coercion between these types ((complex complex))
;let's install the identity coercions:
;(put-coercion 'scheme-number 'scheme-number identity)
;(put-coercion 'complex 'complex identity)
;(exp (make-complex-from-real-imag 2 0) (make-complex-from-real-imag 5 0))
; ERROR: infinite coercion loop: complex get coerced to complex & apply-generic gets called with exactly the same arguments.

;b)
(check-true (equ? (make-scheme-number 33) (make-scheme-number 33)))

(println "Done")