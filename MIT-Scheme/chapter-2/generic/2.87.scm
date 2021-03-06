(load "gen-arith.scm")
(load "integer-package.scm")
(load "rational-package.scm")
(load "real-package.scm")
(load "complex-package.scm")
(load "polynomial-package.scm")

(install-integer-package)
(install-rational-package)
(install-real-package)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)
(install-polynomial-package)

(define (equ? x y)
  (apply-generic 'equ? x y))
(define (=zero? x)
  (apply-generic '=zero? x))

; for modularity and expansion, every coefficient is generic.
(define a (make-polynomial 'x
                           (list
                            (list 2 (make-integer 1))
                            (list 0 (make-integer 1)))))

(define b (make-polynomial 'x
                           (list
                            (list 3 (make-polynomial
                                     'y (list 1 (make-integer 2))))
                            (list 0 (make-integer 2)))))
(display (add a b))
