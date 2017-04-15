#lang racket

(define (last-pair l)
  (define (last-elem e l)
    (if (null? l)
        e
        (last-elem (car l) (cdr l))))
    
  (if (null? l)
      (error "List must be non-empty")
      (list (last-elem (car l) (cdr l)))))

(require rackunit)

(check-exn
 exn:fail?
 (λ () (last-pair null)))
(check-equal? (last-pair (list 1 2 3)) (list 3))
(check-equal? (last-pair (list 23 72 149 34)) (list 34))

(println "Done")