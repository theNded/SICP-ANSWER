; fast exp calculation
(define (fast-expt b n)
  (define (f b n result)
    (cond ((= 0 n) result)
          ((even? n) (f (square b) (/ n 2) result))
          (else (f b (- n 1) (* b result)))))
  (f b n 1))
