#lang sicp

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter items nil))

(reverse (list 1 4 9 16 25))
;(25 16 9 4 1)