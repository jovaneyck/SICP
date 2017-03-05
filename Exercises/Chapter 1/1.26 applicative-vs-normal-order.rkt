#lang racket

; applitive-order: Racket evaluates arguments first
(define (x)
  (sleep 1)
  10)

(define now current-inexact-milliseconds)

;2 sec
(define (time start)
  (* (x) (x))
  (println (- (now) start)))

(time (now))

;1 sec
(define (square n) (* n n))
(define (time-square start)
  (square (x))
  (println (- (now) start)))

(time-square (now))