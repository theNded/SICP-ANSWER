; a more apparent way to calculate:
; use the MATRIX (1 1) to do the iteration
;                (1 0)
; b contains the base matrix.
; r contains the result matrix.
; just looks like the fast-expr, just in the form of matrix

(define (fast-fib n)
  (define (fast-fib-iter b1 b2 b3 b4 n r1 r2 r3 r4)
    (cond ((= n 0) r4)
          ((even? n)
           (fast-fib-iter
            (+ (* b1 b1) (* b2 b3))
            (+ (* b1 b2) (* b2 b4))
            (+ (* b3 b1) (* b4 b3))
            (+ (* b3 b2) (* b4 b4))
            (/ n 2) r1 r2 r3 r4))
          (else
           (fast-fib-iter
            b1 b2 b3 b4 (- n 1)
            (+ (* r1 b1) (* r2 b3))
            (+ (* r1 b2) (* r2 b4))
            (+ (* r3 b1) (* r4 b3))
            (+ (* r3 b2) (* r4 b4))))))
  (fast-fib-iter 1 1 1 0 n 1 0 1 0))

; print method for testing
(define (print-fib n)
  (define (print-fib-iter i n)
    (if (> i n)
        ()
        (begin
         (display (fast-fib i))
         (display " ")
         (print-fib-iter (+ i 1) n))))
  (print-fib-iter 0 n))
