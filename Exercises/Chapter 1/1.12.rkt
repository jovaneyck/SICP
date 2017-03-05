#lang racket

(define (ae f r c expected)
  (define actual (f r c))
  (cond ((not (= expected actual))
         (displayln
          (cons "FAIL. expected: ("
          (cons f (cons r (cons c
          (cons ")" (cons "="
          (cons expected (cons "but was:" (cons actual null)))))))))))))

(define (pascal r c)
  (cond
    ((or (= c 1) (= c r)) 1)
    (else
      (define (pascal-prev-row col) (pascal (- r 1) col))
      (+ (pascal-prev-row (- c 1)) (pascal-prev-row c)))))
#|
actual triangle

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

column indices
    1
   1 2
  1 2 3
 1 2 3 4
1 2 3 4 5

|#
(displayln "Starting test run...")
(ae pascal 1 1 1)
(ae pascal 2 1 1)
(ae pascal 2 2 1)
(ae pascal 3 1 1)
(ae pascal 3 2 2)
(ae pascal 5 3 6)
(displayln "Test run completed")