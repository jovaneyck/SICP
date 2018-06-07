#lang racket

(require r5rs)

(define (make-queue)
  (let [(front-ptr '())
        (rear-ptr '())]

    (define (empty?)
      (null? front-ptr))
    
    (define (insert! el)
      (let [(new-pair (cons el '()))]
        (cond [(empty?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)]
              [else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)])))

    (define (front)
      (if (empty?)
          (error "FRONT called on an empty queue")
          (car front-ptr)))

    (define (delete!)
      (cond [(empty?)
             (error "DELETE called on an empty queue")]
            [else
             (set! front-ptr (cdr front-ptr))]))

    (define (to-string)
      (format "~a" front-ptr))
    
    (define (dispatch m)
      (cond [(eq? m 'insert!) insert!]
            [(eq? m 'empty?) empty?]
            [(eq? m 'front)  front]
            [(eq? m 'delete!) delete!]
            [(eq? m 'to-string) to-string]
            [else (error "ERROR: did not recognize message:" m)]))
    dispatch))

(require rackunit)

(print "Testing...")
(define q (make-queue))

(check-true ((q 'empty?)))
(check-exn exn:fail? (λ () ((q 'front))))


((q 'insert!) 'a)

(check-false ((q 'empty?)))
(check-equal? ((q 'front)) 'a)

((q 'delete!))
(check-true ((q 'empty?)))

((q 'insert!) 'a)
((q 'insert!) 'b)

(check-equal? ((q 'to-string)) "{a b}")

((q 'delete!))
(check-equal? ((q 'to-string)) "{b}")

(println "..done")