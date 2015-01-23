#lang racket
(define (reverse lat)
  (if (null? lat) '()
      (append (reverse (cdr lat)) (list (car lat)))))

(define (deep-reverse lat)
  (let ((rev-lat (reverse lat)))
    (define (loop lat)
      (if (null? lat) '()
          (cons (if (pair? (car lat))
                    (deep-reverse (car lat))
                    (car lat))
                (loop (cdr lat)))))
    (loop rev-lat)))

(deep-reverse '(1 2 3 4))
(deep-reverse (list 1 (list 2 3) (list 4 5)))

