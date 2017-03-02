#lang racket

;Recreating 1.2.4 exponentiation procedures

(define (ae f b n expected)
  (define actual (f b n))
  (cond ((not (= expected actual))
         (displayln
          (cons "FAIL. expected: (" (cons f (cons b (cons n (cons ")" (cons "=" (cons expected (cons "but was:" (cons actual null)))))))))))))
; recursive
(define (exp b n)
  (cond ((= 0 n) 1)
        (else ( * b (exp b (- n 1))))))

(ae exp 1 1 1)
(ae exp 2 2 4)
(ae exp 3 2 9)

;iterative
;tail-recursive procedure: O(1) space, O(n) operations
(define (expi b n)
  (define (exp-acc acc counter)
    (cond ((= 0 counter) acc)
          (else (exp-acc (* b acc) (- counter 1)))))
  (exp-acc 1 n))

(ae expi 1 1 1)
(ae expi 2 2 4)
(ae expi 2 10 1024)

;recursive
; O(log n) calculations + space
#|
 b^n
=> b^n/2 * b^n/2 if n is even
=> b * b^n-1 if n is odd
|#

(define (square n) (* n n))

(define (fast-exp b n)
  (cond ((= 0 n) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(ae fast-exp 1 0 1)
(ae fast-exp 2 1 2)
(ae fast-exp 2 10 1024)

; 1.16 iterative exp O(log n)
;that uses (b^(n/2))^2 = (b^2)^(n/2)
; => b^n = (b^2)^(n/2)
(define (exp-alt b n)
  (define (exp-iter a b n)
    (cond
      ((= 0 n) a)
      ((= 1 n) (* a b))
      (else (exp-iter (* a (* b b)) b (- n 2))))
  )
  (exp-iter 1 b n))

(ae exp-alt 1 0 1)
(ae exp-alt 1 1 1)
(ae exp-alt 2 1 2)
(ae exp-alt 3 2 9)
(ae exp-alt 3 3 27)
(ae exp-alt 2 10 1024)