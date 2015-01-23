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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

; (a)
(define (integrate-series stream-a)
  (mul-streams stream-a
               (div-streams ones integers)))

; (b)
(define cosine-series
  (cons-stream 1
               (integrate-series
                (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0
               (integrate-series cosine-series)))

(newline)
(display (stream-head sine-series 10))
(newline)
(display (stream-head cosine-series 10))
