#lang sicp

(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (make-center-percent c p)
  (make-interval (- c (* c 0.01 p))
                 (+ c (* c 0.01 p))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i))
     (* 2 (center i) 0.01)))

(define (product-tolerance i j)
  (+ (percent i) (percent j)))
