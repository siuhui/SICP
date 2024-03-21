#lang sicp

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter items nil))

(define (same-parity . x)
  (if (odd? (car x))
      (select odd? x)
      (select even? x)))

(define (select condition items)
  (define (iter items result)
    (if (null? items)
        result
        (if (condition (car items))
            (iter (cdr items)
                  (cons (car items) result))
            (iter (cdr items)
                  result))))
  (reverse (iter items nil)))

(same-parity 1 2 3 4 5 6 7); => (1 3 5 7)
(same-parity 2 3 4 5 6 7); => (2 4 6)