(define (mul a b)
  (define (double x) (* x 2))
  (define (halve  x) (/ x 2))
  (define (mul-iter a b result operator)
    (cond ((= b 0) result)
          ((even? b) (mul-iter (double a) (halve b) result operator))
          (else (mul-iter a (- b 1) (operator result a) operator))))
  (mul-iter (abs a) (abs b) 0
            ; handle the negative situation
            ((lambda (a b)
              (if (> (* a b) 0)
                  +
                  -)) a b)))
