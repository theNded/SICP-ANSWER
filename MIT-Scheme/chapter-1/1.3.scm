(define (f a b c)
  (define (min a b)
    (if (< a b)
        a
        b))
  (- (+ a b c) (min (min a b) c)))
