#lang racket
(define (fib n)
  (fib-iter 1 0 0 1 n))
; a and b act as answer, p and q act as base
(define (fib-iter a b p q n)
  (cond ((= n 0) b)
        ((even? n)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* 2 p q) (* q q))
                   (/ n 2)))        
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q)) 
                   p
                   q
                   (- n 1)))))

(define (make-interval l h)
  (if (> l h) '()
      (cons l (make-interval (+ l 1) h))))

(map fib (make-interval 0 20))