(load "gen-arith.scm")
(load "integer-package.scm")
(load "rational-package.scm")
(load "real-package.scm")
(load "complex-package.scm")

(install-integer-package)
(install-rational-package)
(install-real-package)

(install-polar-package)
(install-rectangular-package)
(install-complex-package)

(define (raise x) (apply-generic 'raise x))

(display (raise (make-integer 2)))
(newline)
(display (raise (make-rational 2 3)))
(newline)
(display (raise (make-real 2.4)))
