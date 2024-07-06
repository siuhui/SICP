#lang sicp

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

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (make-triple n)
  (let ((interval (enumerate-interval 1 n)))
    (flatmap (lambda (i)
               (flatmap (lambda(j)
                          (map (lambda (k)
                                 (list i j k))
                               (remove j (remove i interval))))
                        (remove i interval)))
             interval)))

(define (equal-sum? triple s)
  (= s (+ (car triple) (cadr triple) (caddr triple))))

(define (equal-sum-triple n s)
  (filter (lambda (x) (equal-sum? x s))
          (make-triple n)))
  