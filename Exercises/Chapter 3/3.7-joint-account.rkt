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
                 [else (error "unknown operation" operation)])]))
  dispatch)

(define (make-joint original-account original-pass new-pass)
  (λ (pass operation)
    (cond [(not (eq? new-pass pass)) (error "Incorrect password")]
          [else (original-account original-pass operation)])))

(require rackunit)

(print "Running tests...")

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

(void ((paul-acc 'rosebud 'withdraw) 30))

(check-eq? ((paul-acc 'rosebud 'balance)) 70)
(check-eq? ((peter-acc 'open-sesame 'balance)) 70)

(void ((peter-acc 'open-sesame 'withdraw) 30))

(check-eq? ((paul-acc 'rosebud 'balance)) 40)
(check-eq? ((peter-acc 'open-sesame 'balance)) 40)

(check-exn exn:fail? (λ () (peter-acc 'another-password 'balance)))
(check-exn exn:fail? (λ () (paul-acc 'another-password 'balance)))

(println "..done!")