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
