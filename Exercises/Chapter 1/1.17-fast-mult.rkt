#lang racket

(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))

;1.17
(define (fast-mul a b)
  (cond
    ((= 0 b) 0)
    ((= 1 b) a)
    ((even? b) (fast-mul (double a) (halve b)))
    (else (+ a (fast-mul a (- b 1))))))
    
;1.18
(define (tailrec-mul a b)
  (define (rmul a b acc)
    (cond
      ((= 0 b) acc)
      ((even? b) (rmul (double a) (halve b) acc))
      (else (rmul a (- b 1) (+ acc a)))))
  (rmul a b 0))

(require rackunit)

(define eq? check-eq?)

(eq? (mul 3 20) 60)
(eq? (mul 2 4) 8)
(eq? (mul 0 1) 0)
(eq? (mul 1 0) 0)

(eq? (double 3) 6)

(eq? (halve 6) 3)
(eq? (halve 8) 4)

(eq? (fast-mul 3 18) 54)
(eq? (fast-mul 2 4) 8)
(eq? (fast-mul 3 5) 15)
(eq? (fast-mul 0 1) 0)
(eq? (fast-mul 1 0) 0)

(eq? (tailrec-mul 2 4) 8)
(eq? (tailrec-mul 0 1) 0)
(eq? (tailrec-mul 1 0) 0)
(eq? (tailrec-mul 3 5) 15)
(eq? (tailrec-mul 4 7) 28)
(eq? (tailrec-mul 3 18) 54)

(print "all done")