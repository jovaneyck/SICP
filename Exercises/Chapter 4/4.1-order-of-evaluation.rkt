#lang racket

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (eval expression env)
  (begin
    (display (list "evaluating" expression))
    expression))

;evaluation order left-to-right, dependent on how 'cons' works
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values-lr (rest-operands exps) env))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let
          ;force evaluation right-to-left. again assumption let evaluates in order :)
          [(tail (list-of-values-rl (rest-operands exps) env))
           (head (eval (first-operand exps) env))]
        (cons head
              tail))))

(require rackunit)
(println "left-to-right")
(check-equal? '(1 2 3) (list-of-values-lr '(1 2 3) null))
(println "")

(println "right-to-left")
;same behaviour if referentially transparent/side-effect free
(check-equal? '(1 2 3) (list-of-values-rl '(1 2 3) null))