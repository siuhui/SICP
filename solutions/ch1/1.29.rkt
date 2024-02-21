#lang sicp

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (y-term k)
    (cond ((= k 0) (y k))
          ((= k n) (y k))
          ((odd? k) (* 4 (y k)))
          (else (* 2 (y k)))))
  (* (sum y-term 0 inc n)
     (/ h 3)))

(simpson cube 0 1 100)
(simpson cube 0 1 1000)
