#lang racket

(require rackunit)

(check-equal?
 (let (
       [l (list 1 3 (list 5 7) 9)])
   (car (cdr (car (cdr (cdr l)))))) 7)

(check-equal?
 (let (
       [l (list (list 7))])
   (car (car l))) 7)

(check-equal?
 (let (
       [l (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))])
   (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l)))))))))))))
 7)

(println "Done")