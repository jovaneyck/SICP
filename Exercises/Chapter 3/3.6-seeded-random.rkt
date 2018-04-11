#lang racket

;going to handroll some RNG just for the sake of this exercise
;https://en.wikipedia.org/wiki/Lehmer_random_number_generator

(define random-init 1337)

(define (rand-update x)
  (begin
    (define G 75)
    (define n 65537)

    (modulo (* G x) n)))

(define rand
  (let [(x random-init)]
    (let [(generate
           (λ ()
             (begin
               (set! x (rand-update x))
               x)))
          (set-x
           (λ (new-x) (set! x new-x)))]
      (λ (operation)
        (cond [(eq? operation 'generate) (generate)]
              [(eq? operation 'reset) set-x]
              [(eq? operation 'get) x]
              [else (error "nope")])))))

(require rackunit)
(print "Testing...")

(check-eq? (rand 'generate) 34738)
(check-eq? (rand 'generate) 49407)
(check-eq? (rand 'generate) 35453)

((rand 'reset) random-init)
(check-eq? (rand 'generate) 34738)
(check-eq? (rand 'generate) 49407)

(println "..done!")