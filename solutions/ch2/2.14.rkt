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


;;demonstrate that Lem is right

(define R1 (make-interval 1.4 1.6))
(define R2 (make-interval 3.4 3.6))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(par1 R1 R2)
;(0.9153846153846152 . 1.2000000000000002)
(par2 R1 R2)
;(0.9916666666666667 . 1.1076923076923078)

;;computing the expressions A/A and A/B

(define A (make-interval 1 2))
(define B (make-interval 3 4))

(div-interval A A)
;(0.5 . 2.0)
(div-interval A B)
;(0.25 . 0.6666666666666666)


;;using intervals whose width is a small percentage of the center value

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

(define (mul-center-percent i j)
  (make-center-percent (* (center i) (center j))
                       (product-tolerance i j)))

(define (div-center-percent i j)
  (let ((k (make-center-percent (/ 1 (center j))
                                (* (- (/ 1
                                         (- 1 (* 0.01 (percent j))))
                                      1)
                                   100))))
    (mul-center-percent i k)))

(define (add-center-percent i j)
  (make-center-percent (+ (center i) (center j))
                       (/ (+ (* (center i) (percent i))
                             (* (center j) (percent j)))
                          (+ (center i) (center j)))))

(define R3 (make-center-percent 1.5 0.01))
(define R4 (make-center-percent 3.5 0.01))

(define (par3 r1 r2)
  (div-center-percent (mul-center-percent r1 r2)
                      (add-center-percent r1 r2)))

(define (par4 r1 r2)
  (let ((one (make-center-percent 1 0)))
    (div-center-percent
     one (add-center-percent (div-center-percent one r1)
                             (div-center-percent one r2)))))

(par3 R3 R4)
;(0.7497749924992498 . 0.7502250075007502)
(par3 R3 R4)
;(0.7497749924992498 . 0.7502250075007502)
