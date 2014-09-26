; generic form (iteration)
(define (accumulate-iter combiner null-value term a next b)
  (define (accumulate a result)
    (if (> a b)
        result
        (accumulate
         (next a)
         (combiner (term a) result))))
  (accumulate a null-value))

; generic form (recursion)
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner
       (term a)
       (accumulate-recur combiner null-value term (next a) next b))))

; sum form
(define (sum term a next b)
  (accumulate-recur + 0 term a next b))

; product form
(define (product term a next b)
  (accumulate-recur * 1 term a next b))
