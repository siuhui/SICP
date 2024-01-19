#lang sicp

(define (sqrt x)
  (sqrt-iter 1 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs(- (square guess) x)) 0.001))

(define (sqrt-improved x)
  (sqrt-iter-improved 1 0 x))

(define (sqrt-iter-improved guess lastguess x)
  (if (good-enough?-improved guess lastguess)
      guess
      (sqrt-iter-improved (improve guess x) guess x)))

(define (good-enough?-improved guess lastguess)
  (< (abs(- guess lastguess)) 0.001))

(define (square x)
  (* x x))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))