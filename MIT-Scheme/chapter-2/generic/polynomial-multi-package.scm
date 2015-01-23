; a representation with more variables:
; (x y z) ((order-x order-y order-z) coeff) ...
; in which order-x >= order-y >= order-z by default.

(load "gen-arith.scm")

(define (install-polynomial-multi-package)
  ;; internal procedures

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
