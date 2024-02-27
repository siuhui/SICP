#lang sicp

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average v1 v2)
  (/ (+ v1 v2) 2))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root n x)
  (fixed-point
   ((repeated average-damp (floor (log2 n)))
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))
