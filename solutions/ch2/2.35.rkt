#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; it works, but ...
(define (count-leaves t)
  (accumulate (lambda (x y)
                (+ (if (pair? x)
                       (count-leaves x)
                       1)
                   y))
              0
              (map (lambda (x) x) t)))
              
(count-leaves (list (list 1 2) (list 3 4)))