#lang sicp

(define dx 0.00001)

(define (average v1 v2 v3)
  (/ (+ v1 v2 v3) 3))

(define (smooth f)
  (lambda (x)
    (average
     (f (- x dx))
     (f x)
     (f (+ x dx)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond ((= n 1)
        (lambda (x) (f x)))
        ((= n 2)
        (compose f f))
        (else
         (compose
          f
          (repeated f (- n 1))))))

(define (repeated-smooth f n)
  ((repeated smooth n) f))
