#lang racket

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;let's simplify: only perpendicular to x-/y-axes.

;A representation: store 2 opposite points
;A constructor
(define (make-rect lowerleft upperright)
  (cons lowerleft upperright))

;A selectors
(define (bottom-left rect)
  (car rect))
(define (top-right rect)
  (cdr rect))

(define (height-rect rect)
  (-
   (y-point (top-right rect))
   (y-point (bottom-left rect))))

(define (width-rect rect)
  (-
   (x-point (top-right rect))
   (x-point (bottom-left rect))))

;B representation: store a point and the dimensions
; B constructor
(define (make-rect2 lowerleft upperright)
  (let ((width (- (x-point upperright) (x-point lowerleft)))
        (height (- (y-point upperright) (y-point lowerleft))))
    (cons lowerleft (cons width height))))

; B selectors
(define (height-rect2 rect)
  (cdr (cdr rect)))
(define (width-rect2 rect)
  (cdr (car rect)))

;operations
(define (perimeter-rect rect)
  (let ((h (height-rect rect))
        (w (width-rect rect)))
    (+ h h w w)))

(define (area-rect rect)
  (let ((h (height-rect rect))
        (w (width-rect rect)))
    (* h w)))

(require rackunit)

(check-equal? (height-rect (make-rect (make-point 1 2) (make-point 3 5))) 3)
(check-equal? (width-rect (make-rect (make-point 1 2) (make-point 3 5))) 2)

(check-equal? (perimeter-rect (make-rect (make-point 1 2) (make-point 3 5))) 10)
(check-equal? (area-rect (make-rect (make-point 1 2) (make-point 3 5))) 6)

(displayln "Done")