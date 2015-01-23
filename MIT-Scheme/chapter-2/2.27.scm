; construct nil in the front is dull
(define (reverse x)
  (define (rev-iter x result)
    (if (null? x)
        result
        (rev-iter (cdr x)
                  (cons (car x) result))))
  (rev-iter x '()))

(define (deep-reverse x)
  (define (deep-rev-iter x result)
    (if (null? x)
        result
        (deep-rev-iter (cdr x)
                       (cons (if (pair? (car x))
                                 (deep-reverse (car x))
                                 (car x))
                             result))))
  (deep-rev-iter x '()))
