; recursion version
(define f-recur
  (lambda (n)
    (if (< n 3)
        n
        (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3)))))))

; iteration version
(define (f-iter n)
  (define (f a b c cnt)
    (if (= cnt 0)
        c
        (f (+ a (* 2 b) (* 3 c)) a b (- cnt 1))))
  (f 2 1 0 n))
