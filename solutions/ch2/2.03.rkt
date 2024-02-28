#lang sicp

(define (make-segment s e)
  (cons s e))

(define (start-segment x)
  (car x))

(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-rectangle s1 s2)
  (cons s1 s2))

(define (s1-rectangle r)
  (car r))

(define (s2-rectangle r)
  (cdr r))

(define (length s)
  (sqrt (+ (sqare (- (x-point (start-segment s))
                     (x-point (end-segment s))))
           (sqare (- (y-point (start-segment s))
                     (y-point (end-segment s)))))))

(define (sqare x) (* x x))

(define (perimeter r)
  (* 2
     (+ (length (s1-rectangle r))
        (length (s2-rectangle r)))))

(define (area r)
  (* (length (s1-rectangle r))
        (length (s2-rectangle r))))

;test
(define zero-zero (make-point 0 0))
(define two-zero (make-point 2 0))
(define zero-three (make-point 0 3))
(define s1 (make-segment zero-zero two-zero))
(define s2 (make-segment zero-zero zero-three))
(define rectangle (make-rectangle s1 s2))

(perimeter rectangle)
;10

(area rectangle)
;6
