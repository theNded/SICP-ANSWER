(define (enumerate-interval l h)
  (if (> l h)
      '()
      (cons l
            (enumerate-interval (+ l 1) h))))

(define (make-unique-pair n)
  (map (lambda (i)
         (map (lambda (j) (list j i))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))
