(load "gen-arith.scm")
(load "integer-package.scm")
(load "rational-package.scm")
(load "real-package.scm")
(load "complex-package.scm")
(load "polynomial-multi-package.scm")

(install-integer-package)
(install-rational-package)
(install-real-package)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(install-polynomial-multi-package)

(define a
  (make-multi-polynomial '(x y) (list
                                 (list '(1 1) (make-integer 1)))))
(define b
  (make-multi-polynomial '(x y) (list
                                 (list '(2 0) (make-integer 1))
                                 (list '(1 1) (make-integer 3)))))
(add a b)
