#lang sicp

(define (average x y) (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (print v)
    (display v)
    (newline))
  (define (try guess)
    (let ((next (f guess)))
      (print next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)

;average damping
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
