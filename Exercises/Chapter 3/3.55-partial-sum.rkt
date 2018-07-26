#lang racket

(require racket/stream)

;first tried using stream-fold, but that does not terminate on infinite lists I kid you not
(define (partial-sums s)
  (cond ([stream-empty? s] s)
        (else (let [(h (stream-first s))
                    (t (stream-rest s))]
                (stream-cons
                 h
                 (stream-map
                  (λ (x) (+ x h))
                  (partial-sums t)))))))

(require rackunit)

(define (stream-take n s)
  (if (= 0 n)
      empty-stream
      (stream-cons (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(define (inc n) (+ n 1))
(define integers (stream-cons 1 (stream-map inc integers)))

(print "testing...")
(check-equal? (stream->list (stream-take 5 (partial-sums integers))) '(1 3 6 10 15))
(check-equal? (partial-sums empty-stream) empty-stream)
(println "..done!")