#lang sicp

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b)
      null-value
      (combiner (if (filter a)
                    (term a)
                    null-value)
                (filtered-accumulate
                 combiner
                 null-value
                 term
                 (next a)
                 next
                 b
                 filter))))

(define (prime-sum a b)
  (filtered-accumulate
   +
   0
   identity
   a
   inc
   b
   prime?))

(define (coprime-product n)
  (define (coprime? x)
    (= 1 (gcd x n)))
  (filtered-accumulate
   *
   1
   identity
   1
   inc
   n
   coprime?))

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor x)
  (define (iter x n)
    (cond ((> (sqare n) x) x)
          ((divisor? n x) n)
          (else (iter x (+ n 1)))))
  (iter x 2))

(define (sqare x)
  (* x x))

(define (divisor? a b)
  (= 0 (remainder b a)))

;test
(prime-sum 2 10)
(coprime-product 10)
