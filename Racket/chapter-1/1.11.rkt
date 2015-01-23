#lang racket
(define (f-recur n)
  (if (< n 3) n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (iter a b c i)
    (let ((d (+ c (* 2 b) (* 3 a))))
      (if (= i n) d
          (iter b c d (+ i 1)))))
  (if (< n 3) n
      (iter 0 1 2 3)))

(define (make-interval l h)
  (if (> l h) '()
      (cons l (make-interval (+ l 1) h))))

(for-each (lambda (x)
            (display "f(") (display x) (display ")")
            (display " iter: ") (display (f-iter x))
            (display " recur: ") (display (f-recur x))
            (newline))
          (make-interval 0 20))