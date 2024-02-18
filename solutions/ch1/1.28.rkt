#lang sicp

(define (fermat-test n)
  (define (try n a)
    (= 1 (expmod a (- n 1) n)))
  (try n (random-num (+ 1 (random (- n 1))) n)))

(define (random-num a n)
  (if (= 1 (gcd a n))
      a
      (random-num (+ 1 (random (- n 1))) n)))

(define (gcd a b)
  (if (= 0 b)
      a
      (gcd b (remainder a b))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((nontrivial? base m) 0)
        ((even? exp) (remainder (sqare (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (sqare x)
  (* x x))

(define (nontrivial? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (sqare a) n))))

(define (prime? n)
  (define (iter n times)
    (cond ((= times 0) true)
          ((fermat-test n) (iter n (- times 1)))
          (else false)))
  (iter n (ceiling (/ n 2))))

(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)

(prime? 7)
(prime? 17)
(prime? 29)
