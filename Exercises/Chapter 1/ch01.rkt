#lang racket

;1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b) ;;#f

(if (and (> b a) (< b (* a b)))
    b
    a)

(cond
  ((= a 4) 6)
  ((= b 4) (+ 6 7 a))
  (else 25))

(+ 2 (if (> b a) b a))

(*
 (cond
   ((> a b) a)
   ((< a b) b)
   (else -1))
 (+ a 1))

;1.2
#|
(5 + 4 + (2 - (3 - (6 + 4/5))))
/
(3 * (6 - 2) * (2 - 7))
|#
(/
 (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
 (* 3 (- 6 2) (- 2 7)))

;1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
;this next function is SO NOT FUNCTIONAL. AARGH I WANT LISt Processing!!
(define (sum-of-squares-of-largest x y z)
  (cond ((> x y)
        (cond
          ((> z y) (sum-of-squares x z))
          (else  (sum-of-squares x y))))
        (else (cond
                 ((> z x) (sum-of-squares y z))
                 (else  (sum-of-squares y x))))))

(eq? 13 (sum-of-squares-of-largest 1 2 3))
(eq? 13 (sum-of-squares-of-largest 1 3 2))
(eq? 13 (sum-of-squares-of-largest 2 1 3))
(eq? 13 (sum-of-squares-of-largest 2 3 1))
(eq? 13 (sum-of-squares-of-largest 3 2 1))
(eq? 13 (sum-of-squares-of-largest 3 1 2))

;1.4

; if b is positive, apply the + function to operands a and b.
; otherwise apply the - function
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 0 1)
(a-plus-abs-b 0 -1)

;1.5

(define (p) (p)) ;calling this function will infinitely loop
(define (test x y)
  (if (= x 0)
      0
      y))

;LISP uses applicative-order (arguments are evaluated first)
;so this will loop infinitely as it evaluates the p call before expanding test:
;(test 0 (p))

;normal-order evaluation first evaluates the operator expression before evaluating the operands.
#|
(test 0 (p))
-> (if (= 0 0)
       0
       (p))
;special form if evaluates: expression first if #t then first expression
-> 0
|#













