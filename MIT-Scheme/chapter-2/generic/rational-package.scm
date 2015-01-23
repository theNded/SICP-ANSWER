;;; The rational number package
(load "gen-arith.scm")

(define (install-rational-package)
  ;; internal procedures
  (define (numer a) (car a))
  (define (denom a) (cdr a))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interfaces
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  ;; 2.79
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))

  ;; 2.80
  (put '=zero? '(rational)
       (lambda (x) (zero? (numer x))))

  ;; 2.83
  (put 'raise '(rational)
       (lambda (x) (make-real
                    (/ (+ 0.0 (car x))
                       (cdr x)))))

  ;; 2.88
  (put 'negative '(rational)
       (lambda (x) (tag (make-rat (- n) d))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)


(define (make-rational n d)
  ((get 'make 'rational) n d))
