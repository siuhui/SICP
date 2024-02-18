#lang sicp

(define (prime? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n) (remainder (sqare (expmod a (/ n 2) m)) m))
        (else (remainder (* a (expmod a (- n 1) m)) m))))

(define (sqare x)
  (* x x))

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
