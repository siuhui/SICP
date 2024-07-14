#lang sicp
(#%require sicp-pict)

(define (split proc1 proc2)
  (lambda (painter)
    (proc1 painter (proc2 painter painter))))

(define right-split (split beside below))
(define up-split (split below beside))
