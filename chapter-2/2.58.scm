; (a)
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
    (and (pair? x) (eq? (cadr x) '+)))
  (define (addend s) (car s))
  (define (augend s) (caddr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  ; product parse
  (define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))
  (define (multiplier p) (car p))
  (define (multiplicand p) (caddr p))
  (define (make-product a1 a2)
    (cond ((or (=number? a1 0) (=number? a2 0)) 0)
          ((=number? a1 1) a2)
          ((=number? a2 1) a1)
          ((and (number? a1) (number? a2)) (* a1 a2))
          (else (list '* a1 a2))))

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

        (else
         (error "Cannot Parse the Expression"))))

; (b)
; Impossible, since the priority cannot be confirmed without changing the function 'deriv'
