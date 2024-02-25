#lang sicp

(define (cont-frac n d k)
  (define (term i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (term (inc i))))))
  (term 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter
         (dec i)
         (/ (n i)
            (+ (d i)
               result)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
