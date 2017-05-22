#lang racket

(require rackunit)

#|
Footnote 34:

Strictly, our use of the quotation mark violates the general rule that all compound expressions in
our language should be delimited by parentheses and look like lists. We can recover this consistency
by introducing a special form quote, which serves the same purpose as the quotation mark. Thus, we
would type (quote a) instead of ’a, and we would type (quote (a b c)) instead of ’(a b
c). This is precisely how the interpreter works. 
|#

(check-eq? 'quote (car ''abracadabra))
(check-eq? 'quote (car (quote (quote abracadabra))))

(println "Done")