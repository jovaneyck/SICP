#lang racket

;Rather than building them ourselves, let's explore racket streams!
(require racket/stream)
(require rackunit)

;Infinite stream using recursive definition
(define ones (stream-cons 1 ones))
(check-equal? 1 (stream-ref ones 0))
(check-equal? 1 (stream-ref ones 1337))

;These guys are memoized by default:
(define twos (stream-cons (begin (println "calculating a two") 2) twos))
(check-equal? 2 (stream-first twos))
(check-equal? 2 (stream-first twos))
;"calculating a two"
;>

;You can map/filter as expected
(define (inc n) (+ n 1))
(define integers (stream-cons 0 (stream-map inc integers)))

;no builtin take, really?
(define (stream-take n s)
  (if (= 0 n)
      empty-stream
      (stream-cons (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(check-equal? (stream-ref integers 1337) 1337)
(check-equal? (stream->list (stream-take 10 integers)) '(0 1 2 3 4 5 6 7 8 9))

(define evens (stream-filter (λ (n) (= 0 (modulo n 2) )) integers))
(check-equal? (stream->list (stream-take 5 evens)) '(0 2 4 6 8))