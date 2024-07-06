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

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence)
         nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else
         (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (j)
             (map
              (lambda (i) (list i j))
              (enumerate-interval (+ j 1) n)))                 
           (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
