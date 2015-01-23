;; integer-number-package
(load "gen-arith.scm")
(define (install-integer-package)
    ;; interfaces
    (define (tag x) (attach-tag 'integer x))
    (put 'add '(integer integer)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(integer integer)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(integer integer)
         (lambda (x y) (tag (* x y))))
    (put 'div '(integer integer)
         (lambda (x y) (tag (integer-divide x y))))

    ;; 2.79
    (put 'equ? '(integer integer)
         (lambda (x y) (= x y)))

    ;; 2.80
    (put '=zero? '(integer)
         (lambda (x) (zero? x)))

    ;; 2.83
    (put 'raise '(integer)
         (lambda (x) (make-rational x 1)))

    ;; 2.88
    (put 'negative '(integer)
         (lambda (x) (tag (- x))))

    (put 'make 'integer
         (lambda (x) (tag (floor x))))
    'done)

(define (make-integer n)
    ((get 'make 'integer) n))
