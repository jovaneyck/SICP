#lang racket

(require racket/gui)
(require rackunit)

(define beside (λ (one other) one))
(define below (λ (one other) one))
(define flip-vert identity)
(define flip-horiz identity)
(define rotate180 identity)

;2.45
(define (split first-positioning second-positioning)
  (define (split-iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter painter (- n 1))))
          (first-positioning painter (second-positioning smaller smaller)))))
  split-iter)

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)])
    (combine4 (corner-split painter n))))

;2.46 2-dimensional vectors
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

;tests
(check-equal? (xcor-vect (make-vect 1 2)) 1)
(check-equal? (ycor-vect (make-vect 1 2)) 2)
(check-equal? (add-vect (make-vect 1 2) (make-vect 3 4)) (make-vect 4 6))
(check-equal? (sub-vect (make-vect 4 1) (make-vect 3 2)) (make-vect 1 -1))
(check-equal? (scale-vect 3 (make-vect 1 4)) (make-vect 3 12))

#|
;let's actually draw stuff
(define wave identity)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))

(define target (make-bitmap 400 400))
(define canvas (new bitmap-dc% [bitmap target]))
(send canvas draw-line
      0 0    ; Start at (0, 0), the top-left corner
      400 400) ; and draw to the bottom-right corner

(make-object image-snip% target)|#