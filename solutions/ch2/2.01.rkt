#lang sicp

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (< (* n d) 0)
        (cons (- (/ (abs n) g))
              (/ (abs d) g))
        (cons (/ (abs n) g)
              (/ (abs d) g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
