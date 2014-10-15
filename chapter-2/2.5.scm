(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (define (div-2-iter z cnt)
    (if (not (zero? (remainder z 2)))
        cnt
        (div-2-iter (/ z 2) (+ cnt 1))))
  (div-2-iter z 0))

(define (cdr z)
  (define (div-3-iter z cnt)
    (if (not (zero? (remainder z 3)))
        cnt
        (div-3-iter (/ z 3) (+ cnt 1))))
  (div-3-iter z 0))
