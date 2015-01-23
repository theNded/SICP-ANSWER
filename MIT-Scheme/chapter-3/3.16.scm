(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; three
(define three (list 'a 'b 'c))

; four
(define one (cons 'a 'b))
(define temp (cons 'c one))
(define four (cons temp one))

; seven
(define two (cons one one))
(define seven (cons two two))

; cycle
(define cycle (list 'a 'b))
(set-cdr! cycle cycle)
