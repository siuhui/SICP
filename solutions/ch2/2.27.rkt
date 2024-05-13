#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter items nil))

(define (deep-reverse items)
    (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (if (pair? (car items))
                        (deep-reverse (car items))
                        (car items))
                    result))))
  (iter items nil))


x; => ((1 2) (3 4))
(reverse x); => ((3 4) (1 2))
(deep-reverse x); => ((4 3) (2 1))