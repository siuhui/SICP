#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product
          term
          (next a)
          next
          b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi/4 n)
  (define (add-two x) (+ x 2))
  (/ (* (product identity 2 add-two n)
        (product identity 4 add-two (- n 1)))
     (* (product identity 3 add-two n)
        (product identity 3 add-two (- n 1)))))

(pi/4 100)
(pi/4 1000)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter
         (next a)
         (* result (term a)))))
  (iter a 1))
