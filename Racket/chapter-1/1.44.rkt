#lang racket
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (lambda (x)
    (if (= n 0) x
        ((compose f (repeated f (- n 1))) x))))

(define (smooth f)
  (let ((dx 0.001))
    (lambda (x)
      (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3))))

(define (repeated-smooth f n)
  (lambda (x)
    (((repeated smooth n) f) x)))

(define (square x) (* x x))

((repeated-smooth square 10) 2.002)
