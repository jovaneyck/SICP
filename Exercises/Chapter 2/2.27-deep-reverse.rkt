#lang racket

(define (reverse l)
  (define (rev-iter acc l)
    (if (null? l)
        acc
        (rev-iter (cons (car l) acc) (cdr l))))
  (rev-iter null l))

(define (deep-reverse l)
    (if (not (pair? l))
        l
        (append
         (deep-reverse (cdr l))
         (list (deep-reverse (car l))))))

(require rackunit)
(define ce check-equal?)

(define l (list (list 1 2) (list 3 4)))

(ce (reverse l) (list (list 3 4) (list 1 2)))
(ce (deep-reverse l) (list (list 4 3) (list 2 1)))

(ce (deep-reverse null) null)
(ce (deep-reverse (list 1 2)) (list 2 1))
(ce (deep-reverse (list (list 1 2) 3 (list 4 5))) (list (list 5 4) 3 (list 2 1)))

(println "Done")