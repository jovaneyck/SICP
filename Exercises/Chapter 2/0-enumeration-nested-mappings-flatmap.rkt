#lang racket

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

(require rackunit)
(check-equal? (enumerate-interval 1 5) (list 1 2 3 4 5))

;there's built-in stuff for this:
 (require data/enumerate/lib)
(check-equal? (enum->list natural/e 3) (list 0 1 2))

(define (flatmap proc list)
  (flatten (map proc list)))

(check-equal? (flatmap (λ (x) (list x (+ x 1))) (list 1 2 3)) (list 1 2 2 3 3 4))

(println "Done")