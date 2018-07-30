#lang racket

(require racket/stream)

(define (rand-update x)
  (begin
    (define G 75)
    (define n 65537)

    (modulo (* G x) n)))

#|
init: seed
requests: (operator * argument) list
|#
(define (random-stream init requests)
  (let ([updated (rand-update init)])
    (cond [(stream-empty? requests) empty-stream]
          [else
           (let [(operation (car (stream-first requests)))]
             (cond [(eq? operation 'generate)
                    (stream-cons updated (random-stream updated (stream-rest requests)))]
                   [(eq? operation 'reset)
                    (random-stream (cadr (stream-first requests)) (stream-rest requests))]
                   [else
                    (error "unknown operator")]))])))

(require rackunit)

(let
    [(requests '((generate) (generate) (generate) (generate) (reset 1337) (generate) (generate)))]
  (check-equal?
   (stream->list (random-stream 1337 requests))
   '(34738 49407 35453 37495 34738 49407)))