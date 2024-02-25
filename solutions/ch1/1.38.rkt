#lang sicp

(define (cont-frac n d k)
  (define (term i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (term (inc i))))))
  (term 1))

(define (e k)
  (define (d i)
    (if (= 2 (remainder i 3))
        (* (/ 2 3)
           (+ i 1))
        1))
  (+ 2
     (cont-frac
      (lambda (i) 1)
      d
      k)))
