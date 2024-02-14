#lang sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (* a b)
  (cond ((= a 0) 0)
        ((even? a) (double (* (halve a) b)))
        (else (+ b (* (- a 1) b)))))
