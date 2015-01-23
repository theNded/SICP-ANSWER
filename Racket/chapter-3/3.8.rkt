#lang racket
(define (constructor)
  (let ((static-val 0) (return-val 0))
    (lambda (x)
      (set! return-val static-val)
      (set! static-val x)
      return-val)))

; When first call (f 0) and second call (f 1),
; the result would be 0 + 0
(define f (constructor))
(newline)
(display (f 0))
(newline)
(display (f 1))

; Otherwise it would be 0 + 1
(define g (constructor))
(newline)
(display (g 1))
(newline)
(display (g 0))
