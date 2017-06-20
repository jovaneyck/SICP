#lang racket

;given
(define (square n) (* n n))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;exercise
(define (make-from-mag-ang magnitude angle)
  (λ (op)
    (cond [(eq? op 'magnitude) magnitude]
          [(eq? op 'angle) angle]
          [(eq? op 'real-part) (* magnitude (cos angle))]
          [(eq? op 'imag-part) (* magnitude (sin angle))]
          [else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)])))

(require rackunit)

(check-eq? (apply-generic 'real-part (make-from-real-imag 2 3)) 2)
(check-eq? (apply-generic 'imag-part (make-from-real-imag 2 3)) 3)
(check-eq? (apply-generic 'magnitude (make-from-real-imag 3 4)) 5)

(check-eq? (apply-generic 'magnitude (make-from-mag-ang 2 30)) 2)
(check-eq? (apply-generic 'angle (make-from-mag-ang 2 30)) 30)
(check-eqv? (apply-generic 'real-part (make-from-mag-ang 2 pi)) -2.0)
(check-eqv? (apply-generic 'imag-part (make-from-mag-ang 4 (/ pi 2))) 4.0)
(println "Done")