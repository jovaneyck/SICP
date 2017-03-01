#lang racket

#|
sqrt(x) = y : y >= 0 and y^2 = x

sqrt guess x =
 good enough guess? guess
                    sqrt improved guess x
|#

(define (sqrt-approx x)
  (root-iter improve-square good-enough-square? 1.0 x))

(define (root-iter improver checker guess x)
  (cond ((checker guess x) guess)
        (else (root-iter improver checker (improver guess x) x))))

(define (good-enough-square? guess x)
  (< (abs (- x (* guess guess))) 0.001))

(define (improve-square guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(sqrt-approx 9)

;1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
  (else else-clause)))

(define (sqrt-iter-new guess x)
  (new-if (good-enough-square? guess x)
          guess
          (sqrt-iter-new (improve-square guess x) x)))

; this will infinitely loop as arguments are eagerly evaluated: recursive term will infinitely expand

;1.7
;(define very-small-number 0.000000000000005)
;(define actual-result (sqrt very-small-number)) ;7.071067811865476e-008
;(define iter-result (sqrt-approx very-small-number))

;1.8 cube root
;(x/y^2 + 2y) / 3
(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-root-approx x)
  (root-iter improve-cube good-enough-cube? 1.0 x))

(define (good-enough-cube? guess x)
  (< (abs (- x (* guess guess guess))) 0.001))

;(cube-root-approx 27)

;1.9
#|
(define (+ a b)
 (if (= a 0)
     b
     (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc( 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

=> Recursive procedure, recursive (stack-based) process

(define (+ a b)
  (if (= a 0)
    b
    (+ (dec a) (inc b)))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

=> Recursive procedure, ITERATIVE (tail-recursive) process!

|#

;1.10
(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))))

; (A 1 10) => (A 0 (A 1 9)) => (A 0 (A 0 (A 1 8) => (A 0 (A 0 (A 0 ... (A 1 1)
;  => (A 0 (A 0 (A 0 ... 2) => ( A 0 (A 0 (A 0 .. 4) => 2 ^10 = 2014

; (A 2 4) => (A 1 (A 2 3)) => (A 1 (A 1 (A 2 2))) => (A 1 (A 1 (A 1 (A 2 1))))
; => (A 1 (A 1 (A 1 2) => (A 1 (A 1 (A 0 (A 1 1)))) => (A 1 (A 1 (A 0 2)))
; => (A 1 (A 1 4)) => (A 1 (A 0 (A 1 3))) => (A 1 (A 0 (A 0 (A 1 2))))
; => (A 1 (A 0 (A 0 (A 0 (A 1 1)))) => (A 1 (A 0 (A 0 (A 0 2))))
; => (A 1 (A 0 4)) => (A 1 8) => (A 0 (A 0 7)) ...

(define (f n) (A 0 n)) ;2n
(define (g n) (A 1 n)) ;2^n
(define (h n) (A 2 n)) ;2;4;16;65536 -> 2^1;2^2;2^4;2^16 -> 1;2;4;16 -> n^2 => 2^(n^2)

; tree-recursion
(define (fib-rec n)
  (cond ((= 0 n) 1)
        ((= 1 n) 1)
        (else (+
               (fib-rec (- n 1))
               (fib-rec (- n 2))))))

(define (fib-iter n)
  (define (fib-iter-acc a b n)
    (cond ((= n 0) b)

          (else (fib-iter-acc (+ a b) a (- n 1)))))
  (fib-iter-acc 1 1 n))

; coin-change
(define (count-change amount)
(cc amount 5))
(define (cc amount kinds-of-coins)
(cond ((= amount 0) 1)
((or (< amount 0) (= kinds-of-coins 0)) 0)
(else (+ (cc amount
(- kinds-of-coins 1))
(cc (- amount
(first-denomination kinds-of-coins))
kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
(cond ((= kinds-of-coins 1) 1)
((= kinds-of-coins 2) 5)
((= kinds-of-coins 3) 10)
((= kinds-of-coins 4) 25)
((= kinds-of-coins 5) 50)))






