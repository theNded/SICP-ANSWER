(define (sqrt x)
  (define good-enough?
    (lambda (guess x)
      (< (abs (- (square guess) x)) 0.0001)))
  (define improve
    (lambda (guess x)
      (/ (+ guess (/ x guess)) 2)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

(define (safe-sqrt x)
  ; |guess^2 - guess^2| / x < 0.1%
  (define (good-enough? old-guess new-guess x)
    (and
     (< (/ (abs (- (square old-guess) (square new-guess))) x) 0.001)
     (< (abs (- (square new-guess) x)))))

  (define (improve guess x)
    (/ (+ guess (/ x guess)) 2))

  (define (sqrt-iter old-guess new-guess x)
    (if (good-enough? old-guess new-guess x)
        new-guess
        (sqrt-iter new-guess (improve new-guess x) x)))
  (sqrt-iter 0.0 1.0 x))
