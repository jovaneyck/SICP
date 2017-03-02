#lang racket

(provide (all-defined-out))

(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))