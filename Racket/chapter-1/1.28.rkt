#lang racket
(define (square x) (* x x))

(define (expmod-mr b n m)
  (cond ((= n 0) 1)
        ((even? n) 
         (let ((r (expmod-mr b (/ n 2) m)))
           (let ((s (remainder (square r) m)))             
             (if (and (= s 1)
                      (not (= r 1)) (not (= r (- m 1))))
                 0 s))))
         (else 
          (remainder (* b (expmod-mr b (- n 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-mr a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((even? n) false)
        ((= 0 times) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(map (lambda (x) (fast-prime? x 15)) (list 561 1105 1729 2465 2821 6601))
  
  