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

(check-equal? (xcor-vect (make-vect 1 2)) 1)
(check-equal? (ycor-vect (make-vect 1 2)) 2)
(check-equal? (add-vect (make-vect 1 2) (make-vect 3 4)) (make-vect 4 6))
(check-equal? (sub-vect (make-vect 4 1) (make-vect 3 2)) (make-vect 1 -1))
(check-equal? (scale-vect 3 (make-vect 1 4)) (make-vect 3 12))

;2.47 cons vs. list implementation of lists
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (make-frame-alt origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (caddr f))
(define (origin-frame-alt f)
  (car f))
(define (edge1-frame-alt f)
  (cadr f))
(define (edge2-frame-alt f)
  (cddr f)) ;only difference with the list implementation: last cdr is element instead of list with 1 element

(check-equal? (origin-frame (make-frame 1 2 3)) 1)
(check-equal? (edge1-frame (make-frame 1 2 3)) 2)
(check-equal? (edge2-frame (make-frame 1 2 3)) 3)
(check-equal? (origin-frame-alt (make-frame-alt 1 2 3)) 1)
(check-equal? (edge1-frame-alt (make-frame-alt 1 2 3)) 2)
(check-equal? (edge2-frame-alt (make-frame-alt 1 2 3)) 3)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;2.48
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

;let's actually draw some stuff!
(define dimension 100)
(define target (make-bitmap dimension dimension))
(define canvas (new bitmap-dc% [bitmap target]))
(define (draw-line vect-a vect-b)
  (send canvas draw-line
        (xcor-vect vect-a)
        (ycor-vect vect-a)
        (xcor-vect vect-b)
        (ycor-vect vect-b)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(println "Done")

(define fullscreen-frame
  (make-frame (make-vect 0 0) (make-vect 0 dimension) (make-vect dimension 0)))

(define wave 
  (segments->painter
   (list
    (make-segment (make-vect 0 .5) (make-vect .999 .999))
    (make-segment (make-vect 0 .5) (make-vect .999 0))
    (make-segment (make-vect .999 0) (make-vect .999 .999))
    (make-segment (make-vect .999 0.5) (make-vect .5 .25))
    (make-segment (make-vect .5 .25) (make-vect .5 .75))
    (make-segment (make-vect .5 .75) (make-vect .999 .5)))))
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))

;2.49
(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 .999))
    (make-segment (make-vect 0 0) (make-vect .999 0))
    (make-segment (make-vect .999 0) (make-vect .999 .999))
    (make-segment (make-vect 0 .999) (make-vect .999 .999)))))

(define x
  (segments->painter
   (list
    (make-segment (make-vect 1 0) (make-vect 0 1))
    (make-segment (make-vect 0 0) (make-vect 1 1)))))

(define diamond
  (segments->painter
   (list
    (make-segment (make-vect 0 .5) (make-vect .5 0))
    (make-segment (make-vect .5 0) (make-vect 1 .5))
    (make-segment (make-vect 1 .5) (make-vect .5 1))
    (make-segment (make-vect .5 1) (make-vect 0 .5)))))


(wave fullscreen-frame)
;(outline fullscreen-frame)
;(x fullscreen-frame)
;(diamond fullscreen-frame)

(make-object image-snip% target)