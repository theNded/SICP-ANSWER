;; real-number-package
(load "gen-arith.scm")
(define (install-real-package)
    ;; interfaces
    (define (tag x) (attach-tag 'real x))
    (put 'add '(real real)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(real real)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(real real)
         (lambda (x y) (tag (* x y))))
    (put 'div '(real real)
         (lambda (x y) (tag (/ (+ 0.0 x) (+ 0.0 y)))))

    ;; 2.79
    (put 'equ? '(real real)
         (lambda (x y) (= x y)))

    ;; 2.80
    (put 'zero? '(real)
         (lambda (x) (zero? x)))

    ;; 2.83
    (put 'raise '(real)
         (lambda (x) (make-complex-from-real-imag x 0)))

    ;; 2.88
    (put 'negative '(real)
         (lambda (x) (tag (- x))))
    (put 'make 'real
         (lambda (x) (tag x)))
    'done)

(define (make-real n)
    ((get 'make 'real) n))
