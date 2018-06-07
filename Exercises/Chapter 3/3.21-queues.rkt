#lang racket

(require r5rs)

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue))))))

(define (to-string-queue q)
  (define (to-string-list l)
    (format "~a" l))
  (cond [(empty-queue? q) ""]
        [else
         (let [(l (front-ptr q))]
           (to-string-list l))]))
(define (print-queue q)
  (println (to-string-queue q)))

(require rackunit)
(print "Testing...")
(define q (make-queue))
(define (check-prints-to? q expected)
  (check-equal? (to-string-queue q) expected))


(insert-queue! q 'a)
(check-prints-to? q "{a}")

(insert-queue! q 'b)
(check-prints-to? q "{a b}")

(delete-queue! q)
(check-prints-to? q "{b}")

(insert-queue! q 'c)
(check-prints-to? q "{b c}")

(insert-queue! q 'd)
(check-prints-to? q "{b c d}")

(delete-queue! q)
(check-prints-to? q "{c d}")

(define q1 (make-queue))
(insert-queue! q1 'a)
(check-prints-to? q1 "{a}")
(insert-queue! q1 'b)
(check-prints-to? q1 "{a b}")
(delete-queue! q1)
(check-prints-to? q1 "{b}")
(delete-queue! q1)
(check-prints-to? q1 "")

(println "..done!")