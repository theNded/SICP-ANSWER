#lang racket
(define (same-parity x . lat)
  (let ((even-x? (even? x)))
    (define (loop lat)
      (cond ((null? lat) '())
            ((eq? even-x? (even? (car lat)))
             (cons (car lat) (loop (cdr lat))))
            (else (loop (cdr lat)))))
    (cons x (loop lat))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)