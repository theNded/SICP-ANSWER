#lang racket
(define (pascal i j)
  (if (or (= j 1) (= j i)) 1
      (+ (pascal (- i 1) (- j 1))
         (pascal (- i 1) j))))

(define (make-interval l h)
  (if (> l h) '()
      (cons l (make-interval (+ l 1) h))))

(define (print-pascal n)
  (define (iter i)
    (if (> i n) (void)
        (begin
          (for-each (lambda (x)
                      (display (pascal i x)) (display " "))
                    (make-interval 1 i))
          (newline)
          (iter (+ i 1)))))
  (iter 1))

(print-pascal 10)
      
  