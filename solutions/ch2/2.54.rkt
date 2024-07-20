#lang sicp

(define (is-list? s)
  (pair? s))

(define (equal? s1 s2)
  (cond ((and (is-list? s1)
              (is-list? s2))
         (and (equal? (car s1) (car s2))
              (equal? (cdr s1) (cdr s2))))
        ((eq? s1 s2) true)
        (else false)))
