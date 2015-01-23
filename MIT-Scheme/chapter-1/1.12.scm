; pascal function, with low efficiency due to the recursion
(define (pascal i j)
  (cond ((= i 1) 1)
        ((= j 1) 1)
        ((= j i) 1)
        (else (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j)))))

; print-pascal as the global function
(define (print-pascal n)
  ; print by line
  (define (iter-line i j n)
    (cond ((> i n)
           ())
          ((> j i)
           (newline)
           (iter-line (+ 1 i) 1 n))
          (else
           (display (pascal i j))
           (display " ")
           (iter-line i (+ 1 j) n))))
  (iter-line 1 1 n))
