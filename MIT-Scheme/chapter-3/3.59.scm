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
