#lang racket

(require racket/stream)

(define (stream-scale stream factor)
  (stream-map (λ (x) (* x factor))
              stream))

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons
                   s1car
                   (merge (stream-rest s1)
                          (stream-rest s2)))))))))

(define (stream-take n s)
  (if (= 0 n)
      empty-stream
      (stream-cons (stream-first s) (stream-take (- n 1) (stream-rest s)))))

(define S (stream-cons 1 (merge (stream-scale S 2) (merge (stream-scale S 3) (stream-scale S 5)))))

(require rackunit)
(print "testing...")

(check-equal? (stream->list (merge '(1 2 3) '(2 3 4))) '(1 2 3 4))
(check-equal? (stream->list (stream-take 20 S)) '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36))
(check-equal? (stream-ref S 10000) 288555831593533440)
(println "done!")