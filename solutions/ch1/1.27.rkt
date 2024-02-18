#lang sicp

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (sqare (expmod a (/ n 2) m)) m))
        (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (sqare x)
  (* x x))

(define (fermat-test n a)
  (= a (expmod a n n)))

(define (prime? n)
  (define (iter n a)
    (cond ((= a n) true)
          ((and (< a n) (fermat-test n a)) (iter n (+ a 1)))
          (else false)))
  (iter n 1))

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
