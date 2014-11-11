(define (deriv exp var)
  ; basic parse
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (=number? x num)
    (and (number? x) (= x num)))
  (define (=length? x len)
    (= (length x) len))

  ; sum parse
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend s) (cadr s))
  (define (augend s)
    (if (=length? (cddr s) 1)
        (caddr s)
        (cons '+ (cddr s))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  ; product parse
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier p) (cadr p))
  (define (multiplicand p)
    (if (=length? (cddr p) 1)
        (caddr p)
        (cons '* (cddr p))))
  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list '* a1 a2))))

  ; exponent parse
  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))
  (define (base e) (cadr e))
  (define (exponent e) (cddr e))
  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((=number? b 0) 0)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

  ; sin parse, with chain law
  (define (sin? x)
    (and (pair? x) (eq? (car x) 'sin)))
  (define (sin-rad x) (cadr x))
  (define (make-sin x)
    (if (number? x)
        (sin x)
        (list 'sin x)))

  ; cos parse, with chain law
  (define (cos? x)
    (and (pair? x) (eq? (car x) 'cos)))
  (define (cos-rad x) (cadr x))
  (define (make-cos x)
    (if (number? x)
        (cos x)
        (list 'cos x)))

  (define (make-negative x)
    (list '- x))

  ; main function
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))

        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))

        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))

        ((exponentiation? exp)
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product (make-product n (make-exponentiation u (- n 1)))
                         (deriv u var))))

        ((cos? exp)
         (make-negative
          (make-product (deriv (cos-rad exp) var)
                        (make-sin (cos-rad exp)))))

        ((sin? exp)
         (make-product (deriv (sin-rad exp) var)
                       (make-cos (sin-rad exp))))

        (else
         (error "Cannot Parse the Expression"))))
