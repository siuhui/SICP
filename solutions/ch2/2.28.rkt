#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result
                      (if (pair? (car items))
                          (fringe (car items))
                          (list (car items)))))))
  (iter items nil))
                      
(fringe x)
(fringe (list x x))