#lang sicp

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car x)
  (counter x 2))

(define (cdr x)
  (counter x 3))

(define (counter x m)
  (if (> (remainder x m) 0)
      0
      (+ 1
         (counter (/ x m) m))))
