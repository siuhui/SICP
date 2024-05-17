#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;test
(define a (list 1 2 3 4))
(define b (list 5 6 7 8))

(map inc a); => (2 3 4 5)
(append a b); => (1 2 3 4 5 6 7 8)
(length a); => 4