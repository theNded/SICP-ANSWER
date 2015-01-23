(load "1.37.scm")

(cont-frac (lambda (i) 1)
           (lambda (i)
             (let ((mod (remainder i 3)))
               (if (or (= mod 0) (= mod 1))
                   1.0
                   (* (+ (car (integer-divide i 3)) 1) 2))))
           100)
