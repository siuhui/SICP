#lang sicp

(define (make-interval a b)
  (cons a b))

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;test
(define i1 (make-interval 2 4))
(define i2 (make-interval 3 7))

(width i1)
;1
(width i2)
;2

(width (add-interval i1 i2))
;3
(width (sub-interval i1 i2))
;3

(width (mul-interval i1 i2))
;11
(width (div-interval i1 i2))
;0.5238095238095237
