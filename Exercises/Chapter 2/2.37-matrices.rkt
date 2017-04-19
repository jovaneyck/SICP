#lang racket

(define <??> void)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (foldr op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (foldr + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(require rackunit)
(check-equal? (dot-product (list 1 2 3) (list 4 5 6))
              32)

#|
1 2   5   1 x 5 + 2 x 6 = 17
3 4 x 6 = 3 x 5 + 4 x 6   39
|#
(check-equal? (matrix-*-vector (list (list 1 2) (list 3 4)) (list 5 6))
              (list 17 39))

(check-equal? (transpose (list (list 1 2 3) (list 4 5 6)))
              (list (list 1 4) (list 2 5) (list 3 6)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ(row) (matrix-*-vector cols row)) m)))

#|
1 2 3   7  8    1x7 + 2x9 + 3x11   1x8 + 2x10 + 3x12   58  64
4 5 6 x 9  10 = 4x7 + 5x9 + 6x11   4x8 + 5x10 + 6x12 = 139 154
        11 12       
|#
(check-equal? (matrix-*-matrix
               (list (list 1 2 3) (list 4 5 6))
               (list (list 7 8) (list 9 10) (list 11 12)))
              (list (list 58 64) (list 139 154)))
(println "Done")