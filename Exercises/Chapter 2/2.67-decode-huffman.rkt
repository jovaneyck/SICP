#lang racket

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond [(null? tree) (error "symbol not found in tree (empty)" symbol tree)]
        [(leaf? tree) (error "symbol not found in tree (leaf)" symbol tree)]
        [else (let ([l (left-branch tree)]
                    [r (right-branch tree)])
                (cond [(and (leaf? l)
                            (eq? (symbol-leaf l)
                                 symbol))
                       (list 0)]
                      [(and (leaf? r)
                            (eq? (symbol-leaf r)
                                 symbol))
                       (list 1)]
                      [(memq symbol (symbols l))
                       (cons 0 (encode-symbol symbol l))]
                      [(memq symbol (symbols r))
                       (cons 1 (encode-symbol symbol r))]
                      [else (error "symbol not found in tree" symbol tree)]))]))



(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= 1 (length leaf-set))
      (car leaf-set)
      (let ([fst (car leaf-set)]
            [snd (cadr leaf-set)]
            [tail (cddr leaf-set)])
        (successive-merge (adjoin-set (make-code-tree fst snd) tail)))))

(require rackunit)
;2.67
(let
    ([sample-tree
      (make-code-tree (make-leaf 'A 4)
                      (make-code-tree
                       (make-leaf 'B 2)
                       (make-code-tree (make-leaf 'D 1)
                                       (make-leaf 'C 1))))]
     [sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)]
     [raw-message '(A D A B B C A)])
  (check-equal? (decode sample-message sample-tree) raw-message)
  (check-equal? (encode raw-message sample-tree) sample-message))

(let ([symbol-frequency-pairs '((A 4) (B 2) (D 1) (C 1))]
      [expected-tree '((leaf A 4) ((leaf B 2) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 4) (A B C D) 8)])
  (check-equal? (generate-huffman-tree symbol-frequency-pairs) expected-tree))

(let ([symbol-frequency-pairs '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))]
      [expected-tree '((leaf A 8)
                       ((((leaf H 1)
                          (leaf G 1)
                          (H G)
                          2)
                         ((leaf F 1)
                          (leaf E 1)
                          (F E)
                          2)
                         (H G F E)
                         4)
                        (((leaf D 1)
                          (leaf C 1)
                          (D C)
                          2)
                         (leaf B 3)
                         (D C B)
                         5)
                        (H G F E D C B)
                        9)
                       (A H G F E D C B)
                       17)])
  (check-equal? (generate-huffman-tree symbol-frequency-pairs) expected-tree))

;2.70
(let ([tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))]
      [message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)]
      [expected-code '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)])
  (check-equal? (encode message tree) expected-code)
  (check-equal? (decode (encode message tree) tree) message))
;encoded string: 84 bits

;fixed length:
;eight-symbol: 3 bits per symbol
;message length: 36 symbols
;encoded length: 3 x 36 = 108 bits

(println "Done")