#lang racket

(define beside (λ (one other) one))
(define below (λ (one other) one))
(define flip-vert identity)
(define flip-horiz identity)
(define rotate180 identity)

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

;let's actually draw stuff
(define wave identity)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (flipped-pairs wave))

(require racket/gui)

(define target (make-bitmap 400 400))
(define canvas (new bitmap-dc% [bitmap target]))
(send canvas draw-line
      0 0    ; Start at (0, 0), the top-left corner
      400 400) ; and draw to the bottom-right corner

(make-object image-snip% target)