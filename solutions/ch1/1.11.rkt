#lang sicp

(define (f-recursion n)
  (cond ((< n 3) n)
        ((>= n 3) (+ (f-recursion (- n 1))
                     (* 2 (f-recursion (- n 2)))
                     (* 3 (f-recursion (- n 3)))))))

(define (f-iter n p0 p1 p2)
  (cond ((< n 2) n)
        ((= n 2) p2)
        ((> n 2) (f-iter (- n 1)
                         p1
                         p2
                         (+ p2
                            (* 2 p1)
                            (* 3 p0))))))
(define (f-iteration n)
  (f-iter n 0 1 2))