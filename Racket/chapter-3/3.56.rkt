#lang racket
(define the-empty-stream '())
(define (stream-null? stream) (null? stream))
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (apply proc (map stream-car s))
                   (apply stream-map 
                          (cons proc (map stream-cdr s))))))
(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons (stream-car s) (stream-filter pred (cdr s))))
         (else (stream-filter pred (cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-head s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-head (stream-cdr s) (- n 1)))))

(define (scale-stream stream factor)
    (stream-map (lambda (x)
                    (* x factor))
                stream))

(define (merge s1 s2)
    (cond
        ((stream-null? s1)
            s2)
        ((stream-null? s2)
            s1)
        (else
            (let ((s1car (stream-car s1))
                  (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                        (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            s2)))
                      ((> s1car s2car)
                        (cons-stream s2car
                                     (merge s1
                                            (stream-cdr s2))))
                      (else
                        (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(stream-head S 100)
