#lang racket
; cons requires a parameter
; which is a function
; which requires two parameters
(define (cons x y)
  (lambda (m) (m x y)))
  
(define (car z)
  (z (lambda (x y) x)))

(define (cdr z)
  (z (lambda (x y) y)))

(car (cons 1 2))
(cdr (cons 1 2))