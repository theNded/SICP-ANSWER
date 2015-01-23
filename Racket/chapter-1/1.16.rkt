#lang racket
(define (fast-expr-iter b n)
  (define (iter b n p)
    (cond ((= n 0) p)
          ((even? n) (iter (* b b) (/ n 2) p))
          (else (iter b (- n 1) (* p b)))))
  (iter b n 1))

(define (make-interval l h)
  (if (> l h) '()
      (cons l (make-interval (+ l 1) h))))

(map (lambda (x) (fast-expr-iter 2 x)) (make-interval 0 64))