#lang racket

(require rackunit)

(define (inc n) (+ n 1))

(define (make-monitored f)
  (define number-calls 0)
  (λ (x)
    (cond [(eq? x 'how-many-calls?) number-calls]
          [(eq? x 'reset-count) (set! number-calls 0)]
          [else (begin
                  (set! number-calls (inc number-calls))
                  (f x))])))

(print "Running tests...")

(let
  [(s (make-monitored sqrt))]
  (begin
    (check-eq? (s 121) 11)
    (void (s 144))
    (check-eq? (s 'how-many-calls?) 2)))


(let
  [(s (make-monitored sqrt))]
  (begin
    (void (s 144))
    (s 'reset-count)
    (check-eq? (s 'how-many-calls?) 0)))

(println "...done!")