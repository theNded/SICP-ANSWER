(define (floyd-cycle? x)
  (define (safe-cdr x)
    (if (pair? x) (cdr x) #f))

  (define (tortoise x) (safe-cdr x))
  (define (hare x) (safe-cdr (safe-cdr x)))

  (define (iter a b)
    (cond ((or (not a) (not b)) #f)
          ((eq? a b) #t)
          (else
           (iter (tortoise a) (hare b)))))

  (iter (tortoise x) (hare x)))

(define normal (list 'a 'b 'c 'd))
(newline)
(display (floyd-cycle? normal))

(define cycle (list 'a 'b 'c 'd))
(set-cdr! cycle cycle)
(newline)
(display (floyd-cycle? cycle))

(define cycle-1 (list 'a 'a))
(set-cdr! cycle-1 cycle-1)
(newline)
(display (floyd-cycle? cycle))
