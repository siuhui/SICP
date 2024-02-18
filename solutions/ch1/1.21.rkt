#lang sicp

(define (smallest-divisor x)
  (define (iter x n)
    (cond ((> (sqare n) x) x)
          ((divisor? x n) n)
          (else (iter x (+ n 1)))))
  (iter x 2))

(define (divisor? x n)
  (= 0 (remainder x n)))

(define (sqare x)
  (* x x))

(smallest-divisor 199)
;199
(smallest-divisor 1999)
;1999
(smallest-divisor 19999)
;7
