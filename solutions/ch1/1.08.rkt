#lang sicp

(define (cube-root x)
  (cube-root-iter 1 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve guess x)
  (/ (+ (* 2 guess)
        (/ x (* guess guess)))
     3))

(define (cube x)
  (* x x x))