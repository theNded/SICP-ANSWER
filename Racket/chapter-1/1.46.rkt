#lang racket
(define (square x) (* x x))
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (if (good-enough? x) x
        ((iterative-improve good-enough? improve) (improve x)))))

(define (sqrt n)
  (define dx 0.001)
  ((iterative-improve
   (lambda (x)
     (< (abs (- (square x) n)) dx))
   (lambda (x)
     (/ (+ x (/ n x)) 2)))
   1.0))

(define (fixed-point f)
  (define dx 0.001)
  ((iterative-improve
    (lambda (x)
      (< (abs (- x (f x))) dx))
    (lambda (x)
      (/ (+ x (f x)) 2)))
   1.0))

(display (sqrt 100)) 
(newline)
(display (fixed-point (lambda (x) (+ (* x x) (- x) 1))))
(newline)