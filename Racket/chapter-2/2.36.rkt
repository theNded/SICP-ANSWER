#lang racket
(define (accumulate op init lat)
  (if (null? lat) init      
      (accumulate op (op (car lat) init) (cdr lat))))

(define (accumulate-n op init lats)
  (if (null? (car lats)) '()
      (cons (accumulate op init (map car lats))
            (accumulate-n op init (map cdr lats)))))

(accumulate-n + 0 (list 
                   (list 1 2 3) 
                   (list 4 5 6) 
                   (list 7 8 9) 
                   (list 10 11 12)))