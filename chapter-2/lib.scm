(define (reverse x)
  (define (rev-iter x result)
    (if (null? x)
        result
        (rev-iter (cdr x)
                  (cons (car x) result))))
  (rev-iter x '()))

(define (enumerate-interval l h)
  (if (l > h)
      '()
      (cons l (enumerate-interval (+ l 1) h))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
