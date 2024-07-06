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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (make-queen row col)
  (list row col))

(define (queen-row queen)
  (car queen))

(define (queen-col queen)
  (cadr queen))
 
(define (adjoin-position new-row k rest-of-queens)
  (append (list (make-queen new-row k)) rest-of-queens))

(define (safe? k positions)
  (let ((new-queen (car positions))
        (rest-of-queens (cdr positions)))
    (define (helper rest-of-queens)
      (cond ((null? rest-of-queens) true)
            ((not (pass? new-queen (car rest-of-queens))) false)
            (else (helper (cdr rest-of-queens)))))
    (helper rest-of-queens)))

(define (pass? new-queen last-queen)
  (and (not (= (queen-row new-queen)
               (queen-row last-queen)))
       (not (= (abs (- (queen-row new-queen)
                       (queen-row last-queen)))
               (abs (- (queen-col new-queen)
                       (queen-col last-queen)))))))
