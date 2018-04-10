#lang racket

(define (make-account balance password)
  (define (get-balance) balance)
  (define (withdraw amount)
    (set! balance (- balance amount))
    balance)
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pass operation)
    (cond [(not (eq? password pass)) (error "Incorrect password")]
          [else
           (cond [(eq? operation 'balance) get-balance]
                 [(eq? operation 'withdraw) withdraw]
                 [(eq? operation 'deposit) deposit]
                 [else (error "implement")])]))
  dispatch)

(require rackunit)

(let [(acc (make-account 100 'secret-password))]
  (check-eq? ((acc 'secret-password 'balance)) 100)
  (check-exn exn:fail? (λ () (acc 'another-password 'balance))))

(let [(acc (make-account 100 'secret-password))]
  (check-eq? ((acc 'secret-password 'deposit) 20) 120)
  (check-eq? ((acc 'secret-password 'withdraw) 80) 40))

(let [(acc1 (make-account 100 'secret-password))
      (acc2 (make-account 100 'another-secret-password))]
  (check-eq? ((acc1 'secret-password 'deposit) 20) 120)
  (check-eq? ((acc2 'another-secret-password 'withdraw) 80) 20))

(print "Running tests...")
(println "..done!")