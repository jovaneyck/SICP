#lang racket

(require rackunit)

(check-eq? ((lambda (x) (+ 1 x)) 2) 3)
(check-eq? ((λ (x) (+ x 1)) 2) 3)

(check-eq?
 (let ((x 1)
       (y 2))
   (+ x y))
 3)

; That's identical to
(check-eq?
 ((λ (x y) (+ x y))
  1
  2)
 3)

(println "Done")