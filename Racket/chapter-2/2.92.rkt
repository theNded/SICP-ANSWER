#lang racket
(require racket/mpair)

(define (square x) (* x x))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record (mcdr record) false))
            false)))
    
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mlist key-1
                                     (mcons key-2 value))
                              (mcdr local-table)))))
      'ok)
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;; The bottom level typing system
(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for the given types -- APPLY-GENERIC"
                 (mlist op type-tags))))))

;;; Some generic arithmetic procedures
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x ) (apply-generic '=zero? x))
(define (negative x) (apply-generic 'negative x))
(define (raise x) (apply-generic 'raise x))

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
       (lambda (x y) (tag (quotient x y))))
  (put 'equ? '(integer integer)
       (lambda (x y) (tag (= x y))))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'negative '(integer)
       (lambda (x) (tag (- x))))
  (put 'make 'integer
       (lambda (x) (tag (floor x))))
  'done)
(define (make-integer n)
  ((get 'make 'integer) n))

;;; The rational number package
(define (install-rational-package)
  ;; internal procedures
  (define numer car)
  (define denom cdr)
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
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (= 0 (numer x)))
  (define (neg-rat x)
    (make-rat (- (numer x)) (denom x)))
  (define (rat->real x)
    (/ (+ 0.0 (numer x)) (denom x)))  
  
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
  (put 'equ? '(rational rational) equ-rat?)     
  (put 'raise '(rational) rat->real)      
  (put '=zero? '(rational) =zero-rat?)
  (put 'negative '(rational) neg-rat)   
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  ;; interfaces
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ (+ 0.0 x) (+ 0.0 y)))))
  (put 'equ? '(real real)
       (lambda (x y) (tag (= x y))))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))  
  (put '=zero? '(real) 
       (lambda (x) (= x 0)))
  (put 'negative '(real)
       (lambda (x) (tag (- x))))
  (put 'make 'real
       (lambda (x) (tag x)))
  'done)
(define (make-real n)
  ((get 'make 'real) n))

;;; The rectangular package
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

;;; The polar package
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;;; The complex package
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (equ-complex? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero-complex? z)
    (and (= 0 (real-part z))
         (= 0 (imag-part z))))
  (define (neg-complex z)
    (make-from-real-imag (- (real-part z))
                         (- (imag-part z))))
  
  ;; interfaces
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  (put 'negative '(complex) neg-complex)
       
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (x y)
         (tag (make-from-mag-ang x y))))    
  'done)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;; The polynomial package
(define (install-polynomial-multi-package)
  ;; representation of poly
  (define (make-poly variable-list term-list)
    (cons variable-list term-list))
  (define (variable-list p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable-list? v1 v2)
    (and (all-variable? v1)
         (all-variable? v2)
         (equal? v1 v2)))

  (define (all-variable? var-list)
    (if (empty-termlist? var-list)
        #t
        (if (variable? (car var-list))
            (all-variable? (cdr var-list))
            #f)))

  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order-list coeff)
    (list order-list coeff))
  (define (order-list term) (car term))
  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
    (if (same-variable-list? (variable-list p1) (variable-list p2))
        (make-poly (variable-list p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (let ((comparison (compare-order (order-list t1)
                                              (order-list t2))))
               (cond ((= 1 comparison)
                      (adjoin-term t1
                                   (add-terms (rest-terms L1)
                                              L2)))
                     ((= -1 comparison)
                      (adjoin-term t2
                                   (add-terms (rest-terms L2)
                                              L1)))
                     (else
                      (adjoin-term (make-term (order-list t1)
                                              (add (coeff t1) (coeff t2)))
                                   (add-terms (rest-terms L1)
                                              (rest-terms L2))))))))))

  (define (compare-order L1 L2)
    (cond ((and (null? L1) (null? L2)) 0)
          ((null? L1) -1)
          ((null? L2) 1)
          (else
            (let ((O1 (car L1)) (O2 (car L2)))
              (cond ((> O1 O2) 1)
                    ((< O1 O2) -1)
                    (else
                      (compare-order (cdr L1) (cdr L2))))))))

  (define (mul-poly p1 p2)
    (if (same-variable-list? (variable-list p1) (variable-list p2))
        (make-poly (variable-list p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (add-order-list (order-list t1) (order-list t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-order-list O1 O2)
    (cond ((null? O1) O2)
          ((null? O2) O1)
          (else
            (cons (+ (car O1) (car O2))
                  (add-order-list (cdr O1) (cdr O2))))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'multi-polynomial p))
  (put 'add '(multi-polynomial multi-polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(multi-polynomial multi-polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'multi-polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-multi-polynomial var term)
  ((get 'make 'multi-polynomial) var term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your code goes here ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some basic testing

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-polynomial-multi-package)

; a representation with more variables:
; (x y z) ((order-x order-y order-z) coeff) ...
; in which order-x >= order-y >= order-z by default.
(define a
  (make-multi-polynomial '(x y) (list
                                 (list '(1 1) (make-integer 1)))))
(define b
  (make-multi-polynomial '(x y) (list
                                 (list '(2 0) (make-integer 1))
                                 (list '(1 1) (make-integer 3)))))
(add a b)
