#lang racket
(define (square x) (* x x))
(define (fast-expr b n)
  (define (iter b n p)
    (cond ((= n 0) p)
          ((even? n) (iter (square b) (/ n 2) p))
          (else (iter b (- n 1) (* p b)))))
  (iter b n 1))

(define (cons a b)
  (* (fast-expr 2 a) (fast-expr 3 b)))

(define (car n)
  (if (even? n) (+ 1 (car (/ n 2)))
      0))

(define (cdr n)
  (define (mod3? n) (= (remainder n 3) 0))
  (if (mod3? n) (+ 1 (cdr (/ n 3)))
      0))

(car (cons 74 32))
(cdr (cons 74 32))