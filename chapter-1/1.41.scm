(define (inc x) (+ x 1))

(define (double f)
  (lambda (x)
    (f (f x))))

; val = 21
(((double (double double)) inc) 5)
