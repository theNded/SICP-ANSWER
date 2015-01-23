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
  (cond ((stream-null? s) 
         the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s) 
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-head s n)
  (if (= n 0)
      '()
      (cons (stream-car s) (stream-head (stream-cdr s) (- n 1)))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((<= (weight s1car) (weight s2car))
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1) s2
                                               weight)))
                 ; interleave
                 (else
                  (cons-stream s2car
                               (merge-weighted s1 (stream-cdr s2)
                                               weight))))))))

(define (weighted-pairs s1 s2 weight)
  (cons-stream (list (stream-car s1) (stream-car s2))
               (merge-weighted
                (stream-map (lambda (x) (list (stream-car s1) x))
                            (stream-cdr s2))
                (weighted-pairs (stream-cdr s1)
                                (stream-cdr s2)
                                weight)
                weight)))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (stream-map + ones integers)))

; (a)
(define weight-a (lambda (x) (+ (car x) (cadr x))))
(define pairs-a (weighted-pairs integers integers weight-a))

; (b)
(define weight-b
  (lambda (x) (+ (* 2 (car x))
                 (* 3 (cadr x))
                 (* 5 (car x) (cadr x)))))

(define (divide? x y) (= (remainder x y) 0))

(define stream-b
  (stream-filter
   (lambda (x)
     (not (or (divide? x 2)
              (divide? x 3)
              (divide? x 5))))
   integers))

(define pairs-b (weighted-pairs stream-b stream-b weight-b))

(newline)
(display (stream-head pairs-a 20))
(newline)
(display (stream-head pairs-b 20))