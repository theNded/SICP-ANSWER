(load "1.43.scm")

(define (smooth f)
  (define dx 0.001)
  (lambda (x)
    (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(define (repeated-smooth f n)
  (lambda (x)
    ((repeated (smooth f) n) x)))
