#lang racket

;1.11
;f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>= 3

(define (f-rec n)
  (cond
    ((< n 3) n)
    (else (+
           (f-rec (- n 1))
           (* 2 (f-rec (- n 2)))
           (* 3 (f-rec (- n 3)))))))

(define (ae f input expected)
  (define actual (f input))
  (cond ((not (= expected actual))
         (displayln
          (cons "FAIL. expected: (" (cons f (cons input (cons ")" (cons "=" (cons expected (cons "but was:" (cons actual null))))))))))))

(ae f-rec 0 0)
(ae f-rec 1 1)
(ae f-rec 2 2)
(ae f-rec 3 4)
(ae f-rec 4 11)
(ae f-rec 5 25)
(ae f-rec 10 1892)

(define (f-iter n)
  (define (iter-acc acc b c count)
    (cond ((= 0 count) acc)
          (else (iter-acc b c (+ c (* 2 b) (* 3 acc)) (- count 1)))))
  (iter-acc 0 1 2 n))

(ae f-iter 0 0)
(ae f-iter 1 1)
(ae f-iter 2 2)
(ae f-iter 3 4)
(ae f-iter 4 11)
(ae f-iter 5 25)
(ae f-iter 10 1892)
