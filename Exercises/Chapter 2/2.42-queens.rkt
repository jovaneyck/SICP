#lang racket

(define (flatmap proc seq)
  (foldr append null (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;first take, represent positions by an int list, index is column
(define empty-board  null)

(define (empty-board? b)
  (equal? b empty-board))

(define (adjoin-position new-row k rest)
  (cons new-row rest))

(define (last positions) (car positions))

(define (older positions) (cdr positions))

(define (column-safe? new-pos positions)
  #t) ;by data-structure-design

(define (row-safe? new-pos positions)
  (andmap ;F#'s List.forall
   (λ(pos) (not (= pos new-pos)))
   positions))

(define (diagonal-safe? new-pos positions)
  (define (ds delta pos)
    (cond
      [(null? pos) #t]
      [(= (+ new-pos delta) (last pos)) #f]
      [(= (- new-pos delta) (last pos)) #f]
      [ else (ds (+ delta 1) (older pos))]))
  (ds 1 positions))

(define (safe? column positions)
  (if (empty-board? positions)
      #t
      (let(
           [new-pos (last positions)]
           [others (older positions)])
        (and
         (row-safe? new-pos others)
         (column-safe? new-pos others)
         (diagonal-safe? new-pos others)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(require rackunit)

(check-true (empty-board? empty-board))
(check-false (empty-board? (adjoin-position 3 8 empty-board)))

(check-equal? (last (adjoin-position 3 8 empty-board)) 3)
(check-equal? (older (adjoin-position 3 8 empty-board)) empty-board)

(check-true (row-safe?
             3
             (adjoin-position 2 8
              (adjoin-position 1 8 empty-board))))
(check-false (row-safe?
             1
             (adjoin-position 2 8
              (adjoin-position 1 8 empty-board))))

(check-true (column-safe?
             1
             (adjoin-position 2 8
              (adjoin-position 1 8 empty-board))))

(check-true (diagonal-safe? 1 empty-board))

(check-false (diagonal-safe?
             1
             (adjoin-position 2 8 empty-board)))
(check-false (diagonal-safe?
             2
             (adjoin-position 1 8 empty-board)))
(check-true (diagonal-safe?
             1
             (adjoin-position 3 8 empty-board)))
(check-false (diagonal-safe?
             1
             (adjoin-position 5 8 (adjoin-position 3 8 empty-board))))
(check-true (diagonal-safe?
             1
             (adjoin-position 5 8 (adjoin-position 4 8 empty-board))))

(check-equal? (queens 4) (list (list 3 1 4 2) (list 2 4 1 3)))
(check-equal? (length (queens 8)) 92)

(println "Done")