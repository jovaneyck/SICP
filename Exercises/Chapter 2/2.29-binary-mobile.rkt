#lang racket

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))
(define (is-weight? structure) (number? structure))

(define (total-weight-branch branch)
  (let ([s (branch-structure branch)])
    (if (is-weight? s)
        s
        (total-weight s))))

(define (total-weight mobile)
  (+ (total-weight-branch (left-branch mobile))
     (total-weight-branch (right-branch mobile))))

(define (balanced? mobile)
  (define (torque branch)
    (let ([l (branch-length branch)]
          [w (total-weight-branch branch)])
      (* l w)))
  (if (is-weight? mobile)
      #t
      (and
       [= (torque (left-branch mobile)) (torque (right-branch mobile))]
       [balanced? (branch-structure (left-branch mobile))]
       [balanced? (branch-structure (right-branch mobile))])))

(require rackunit)
(define ce check-equal?)

(ce (left-branch (make-mobile 1 2)) 1)
(ce (right-branch (make-mobile 1 2)) 2)

(ce (branch-length (make-branch 1 null)) 1)
(ce (branch-structure (make-branch 1 3)) 3)

(define
  nontrivial-mobile
  (make-mobile
   (make-branch
    1
    2)
   (make-branch
    3
    (make-mobile
     (make-branch 4 5)
     (make-branch 6 7)))))

(ce (left-branch nontrivial-mobile) (make-branch 1 2))
(ce (left-branch (branch-structure (right-branch nontrivial-mobile)))
    (make-branch 4 5))

(ce (total-weight nontrivial-mobile) 14)

(check-true (balanced?
             (make-mobile
              (make-branch 2 3)
              (make-branch 6 1))))

(check-true (balanced?
              (make-mobile
               (make-branch 3 6)
               (make-branch
                3
                (make-mobile
                 (make-branch 1 4)
                 (make-branch 2 2))))))

;top-level is balanced but submodule isn't
(check-false (balanced?
              (make-mobile
               (make-branch 3 5)
               (make-branch
                3
                (make-mobile ;this sub-mobile isn't balanced
                 (make-branch 1 3)
                 (make-branch 1 2))))))

(println "Done")