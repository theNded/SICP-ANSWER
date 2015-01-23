;;; scheme-number package ;;;
(load "gen-arith.scm")
(define (install-scheme-number-package)
    ;; interfaces
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))

    ;; 2.79
    (put 'equ? '(scheme-number scheme-number)
         (lambda (x y) (= x y)))

    ;; 2.80
    (put '=zero? '(scheme-number)
         (lambda (x) (zero? x)))

    ;; 2.88
    (put 'negative '(scheme-number)
         (lambda (x) (tag (- x))))

    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))
