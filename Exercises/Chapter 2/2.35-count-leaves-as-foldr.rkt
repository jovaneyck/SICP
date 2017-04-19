#lang racket

(define (count-leaves t)
 (foldr
  +
  0
  (map (λ(t)(if (not (pair? t))
                1
                (length t)))
         t)))

(define (count-leaves-2 t)
  (length (flatten t)))


(require rackunit)
(check-equal? (count-leaves null) 0)
(check-equal? (count-leaves (list 1)) 1)
(check-equal? (count-leaves (list 1 (list 2 3) 4 (list 5 6))) 6)

(check-equal? (count-leaves-2 null) 0)
(check-equal? (count-leaves-2 (list 1)) 1)
(check-equal? (count-leaves-2 (list 1 (list 2 3) 4 (list 5 6))) 6)
(println "Done")