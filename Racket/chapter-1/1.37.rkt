#lang racket
(define (cont-frac-recur n d k)
  (define (recur i)
    (if (= i k) (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1.0))

(define (cont-frac-iter n d k)
  (define (iter i res)
    (if (= i 0) res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

(define one (lambda (x) 1.0))
(define phi (/ (- (sqrt 5) 1) 2))

(define (approx-phi)
  (define (iter i)
    (let ((r (cont-frac-recur one one i)))
      (if (< (abs (- r phi)) 0.00001) 
          (printf "~a times iterated. result: ~a~n" i r)
          (iter (+ i 1)))))
  (iter 1))
(approx-phi)