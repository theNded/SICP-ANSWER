(load "gen-arith.scm")
(load "scheme-number-package.scm")
(load "rational-package.scm")
(load "complex-package.scm")

(install-scheme-number-package)
(install-rational-package)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define (equ? x y)
  (apply-generic 'equ? x y))

(display (equ? (make-scheme-number 2) (make-scheme-number 2)))
(newline)
(display (equ? (make-complex-from-real-imag 1 2)
               (make-complex-from-real-imag 1 2)))
(newline)
(display (equ? (make-rational 1 2) (make-rational 1 2)))
