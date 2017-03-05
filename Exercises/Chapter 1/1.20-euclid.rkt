#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(require rackunit)

(check-eq? (gcd 20 15) 5)

#|
How many remainder evaluations in (gcd 206 40)?

normal-order evaluation:
"fully expand then reduce"
(if (= 40 0)
 206
 (gcd 40 (remainder 206 40)))
=>
(if (= 40 0)
 206
 (if (= (remainder 206 40) 0)
  40
  (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
applicative-order evaluation:
"evaluate args then apply"

|#