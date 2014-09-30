; fix-point-function
(define (fix-point f x paint?)
  (define tolerance 0.0001)
  (define (accurate? x y)
    (< (abs (- x y)) tolerance))
  (define g
    (lambda (x)
      (/ (+ x (f x)) 2)))

  (let ((fx (g x)))
    (cond ((accurate? x fx) fx)
          (paint?
           (display x)
           (newline)
           (fix-point f fx paint?))
          (else
           (fix-point f fx paint?)))))


; 1.35
(fix-point (lambda (x) (+ 1 (/ 1 x))) 1.0 #f)

