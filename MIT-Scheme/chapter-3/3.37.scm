(load "constraint.scm")

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv val)
  (let ((z (make-connector)))
    (constant val z)
    z))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "Celsius" C)
(probe "Fahrenheit" F)
(set-value! C 12 'user)
(forget-value! C 'user)
(set-value! F 100 'user)
