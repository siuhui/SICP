#lang sicp

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor x)
  (define (iter x n)
    (cond ((> (sqare n) x) x)
          ((divisor? x n) n)
          (else (iter x (+ n 1)))))
  (iter x 2))

(define (sqare x)
  (* x x))

(define (divisor? x n)
  (= 0 (remainder x n)))

(define (timed-search-test n)
  (start (+ n 1) (runtime) 0))

(define (start n start-time cnt)
  (cond ((and (< cnt 3) (prime? n))
         (begin
          (report-n&time n (- (runtime) start-time))
          (start (+ n 2) (runtime) (+ cnt 1))))
        ((< cnt 3) (start (+ n 2) (runtime) cnt))))

(define (report-n&time n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(timed-search-test 1000)
(timed-search-test 10000)
(timed-search-test 100000)
(timed-search-test 1000000)
