#lang racket

(define (congruent a n)
  (= (remainder (expt a n) n) (remainder a n)))

(define (all-congruent n)
  (define (congr a n)
    (if (= 1 a)
        #t
        (and (congruent a n) (congr (- a 1) n))))
  (congr n n))


(require rackunit)

(println "Doing..")
(define start (current-seconds))
(check-true (all-congruent 561))
(check-true (all-congruent 1105))
(check-true (all-congruent 1729))
(check-true (all-congruent 2465))
(check-true (all-congruent 2821))
(check-true (all-congruent 6601))
(define duration (- (current-seconds) start))
(println "Done. It took me sec:")
(println duration)