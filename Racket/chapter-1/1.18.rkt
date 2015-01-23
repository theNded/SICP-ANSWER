#lang racket
(define (fast-product a b)
  (define (halve x) (/ x 2))
  (define (double x ) (* x 2))
  (define (iter a b s)
    (cond ((= b 0) s)
          ((even? b) (iter (double a) (halve b) s))
          (else (iter a (- b 1) (+ s a)))))
  (iter a b 0))

(define (make-interval l h)
  (if (> l h) '()
      (cons l (make-interval (+ l 1) h))))

(define (accumulate op init lat)
  (if (null? lat) init
      (op (car lat)
          (accumulate op init (cdr lat)))))
                  
(for-each 
 (lambda (x)
   (display x) (display " ") 
   (display (fast-product (car x) (cadr x)))
   (newline))
 (accumulate append '() (map (lambda (x)
                              (map (lambda (y) (list x y)) (make-interval 1 9)))
                            (make-interval 1 9))))
       