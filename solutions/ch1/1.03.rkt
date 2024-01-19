#lang sicp

(define (add-two-bigger x y z)
  (- (+ x y z)
     (min x y z)))

(define (min x y z)
  (if (< x y)
      (if (< x z) x z)
      (if (< y z) y z)))