#lang sicp

(define (pascal row col)
  (cond ((and (= row 1) (= col 1)) 1)
        ((or (< row 1) (< col 1)) 0)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))