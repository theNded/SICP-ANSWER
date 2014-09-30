; load prime?
(load "1.28.scm")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define self (lambda (x) x))
(define inc  (lambda (x) (+ x 1)))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (filtered-accumulate-iter a result)
    (cond ((> a b) result)
          ((filter a)
           (filtered-accumulate-iter (next a) (combiner (term a) result)))
          (else
           (filtered-accumulate-iter (next a) result))))
  (filtered-accumulate-iter a null-value))

; (a)
(define (prime-sum a b)
  (filtered-accumulate prime? + 0 self a inc b))

; (b)
(define (gcd-sum n)
  (filtered-accumulate
   (lambda (x)
     (= 1 (gcd x n)))
   + 0 self 1 inc n))
