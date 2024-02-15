#lang sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (* a b)
  (*-iter a b 0))

(define (*-iter a b c)
  (cond ((= a 0) c)
        ((even? a) (*-iter (halve a) (double b) c))
        (else (*-iter (- a 1) b (+ b c)))))

;(f 10 1 0) = (f 5 2 0) = (f 4 2 2) = (f 2 4 2)
;           = (f 1 8 2) = (f 0 8 10) = 10
