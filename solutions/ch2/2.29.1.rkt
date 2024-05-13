#lang sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))


(define (total-weight structure)
  (if (mobile? structure)
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))
      structure))

(define (mobile? structure)
  (pair? structure))

(define (balance? structure)
  (define (check mobile)
    (= (* (branch-length (left-branch mobile))
          (total-weight (branch-structure (left-branch mobile))))
       (* (branch-length (right-branch mobile))
          (total-weight (branch-structure (right-branch mobile))))))
  (and (check structure)
       (if (mobile? (branch-structure (left-branch structure)))
           (balance? (branch-structure (left-branch structure)))
           true)
       (if (mobile? (branch-structure (right-branch structure)))
           (balance? (branch-structure (right-branch structure)))
           true)))

;;test
(define a (make-mobile (make-branch 1
                                    (make-mobile (make-branch 1 1)
                                                 (make-branch 1
                                                              (make-mobile (make-branch 1 4)
                                                                           (make-branch 1 5)))))
                       (make-branch 1
                                    (make-mobile (make-branch 1 2)
                                                 (make-branch 1 3)))))

(define b (make-mobile (make-branch 2 6)
                       (make-branch 3 4)))

(define c (make-mobile (make-branch 1
                                    (make-mobile (make-branch 18 1)
                                                 (make-branch 2
                                                              (make-mobile (make-branch 5 4)
                                                                           (make-branch 4 5)))))
                       (make-branch 2
                                    (make-mobile (make-branch 3 2)
                                                 (make-branch 2 3)))))

(total-weight a); => 15
(total-weight b); => 10
(balance? a); => #f
(balance? b); => #t
(balance? c); => #t