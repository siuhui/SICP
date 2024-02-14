#lang sicp

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (sqare b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(define (sqare x)
  (* x x))

;(f 2 3 1) = (f 2 2 2) = (f 4 1 2) = (f 4 0 8) = 8
;(f 2 4 1) = (f 4 2 1) = (f 16 1 1) = (f 16 0 16) = 16
;(f 2 5 1) = (f 2 4 2) = (f 16 1 2) = (f 16 0 32) = 32
