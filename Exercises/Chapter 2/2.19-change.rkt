#lang racket

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coins)
  (null? coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (first-denomination coins)
  (car coins))

(require rackunit)
(check-equal? (cc 100 us-coins) 292)
(check-equal? (cc 100 (reverse us-coins)) 292) ;order does not matter.
(check-equal? (cc 10 uk-coins) 50)
(check-equal? (cc 10 (reverse uk-coins)) 50)
(println "Done")