#lang racket

(require rackunit)

;Requires a special form, because arguments are evaluated:
(define (my-delay f) (λ () f))
(define (my-force f) (f))

(check-eq? 2 (my-force (my-delay 2)))

(println "defining f...")
(define f (my-delay (begin (println "delayed is executed") (+ 3 2))))
(println "defined!")

#|
    "defining f..."
    "delayed is executed" <= call-by-values
    "defined!"
    > 
|#

(check-eq? (my-force f) 5)

;Could work around it by manually wrapping all the delays ourselves:
(println "defining delayed-f...")
(define delayed-f (λ () (begin (println "delayed-f is executed") (+ 4 2))))
(println "defined!")

(check-eq? (my-force delayed-f) 6)

#|
    "defining delayed-f..."
    "defined!"
    "delayed-f is executed"
    >
|#

;Or just use the Racket special forms in promise!
(require racket/promise)

(println "defining promise-f")
(define promise-f (delay (begin (println "promise-f is executed") (+ 4 2))))
(println "defined!")

(check-eq? (force promise-f) 6)

#|
    "defining promise-f"
    "defined!"
    "promise-f is executed"
|#