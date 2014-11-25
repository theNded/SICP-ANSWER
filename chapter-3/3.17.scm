(define (count-pairs x)
  (define visited '())
  (define (iter x)
    (if (or (not (pair? x))
            (memq x visited))
        0
        (begin
          (set! visited (cons x visited))
          (+ (iter (car x))
             (iter (cdr x))
             1))))
  (iter x))

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

(define (display-newline x)
  (newline)
  (display x))

(display-newline (count-pairs three))
(display-newline (count-pairs four))
(display-newline (count-pairs seven))
(display-newline (count-pairs cycle))
