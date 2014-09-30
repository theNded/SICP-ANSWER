; load fix-point function
(load "1.35.scm")
(fix-point (lambda (x) (/ (log 1000) (log x))) 2.0 #t)
