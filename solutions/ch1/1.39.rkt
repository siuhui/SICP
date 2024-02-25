#lang sicp

(define (cont-frac n d k)
  (define (term i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (term (inc i))))))
  (term 1))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (* x x)))
  (cont-frac
   n
   (lambda (i) (- (* i 2) 1))
   k))
