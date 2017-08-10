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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

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
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number)
       (λ (x y) (= x y)))) ;just aliasing = also works
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
       (lambda (x y) (equ-rat? x y))))
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
       (lambda (r a) (tag (make-from-mag-ang r a)))))
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

;2.79
(check-false (equ? (make-scheme-number 1) (make-scheme-number 2)))
(check-true (equ? (make-scheme-number 1337) (make-scheme-number 1337)))

(check-false (equ? (make-rational 1 2) (make-rational 1 3)))
(check-true (equ? (make-rational 1 2) (make-rational 1 2)))
(check-true (equ? (make-rational 1 2) (make-rational 3 6)))

(println "Done")