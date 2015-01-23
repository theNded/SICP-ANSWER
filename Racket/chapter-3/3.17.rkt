#lang racket
(require scheme/mpair)

(define (count-pairs x)
  (define visited (mlist))
  (define (iter x)
    (if (or (not (mpair? x))
            (mmemq x visited))
        0
        (begin
          (set! visited (mcons x visited))
          (+ (iter (mcar x))
             (iter (mcdr x))
             1))))
  (iter x))

; three (a b c)
(define three (mlist 'a 'b 'c))

; four (c (a . b)) --
;             |      |
;              ------ 
(define one (mcons 'a 'b))
(define temp (mcons 'c one))
(define four (mcons temp one))

; seven
(define two (mcons one one))
(define seven (mcons two two))

; cycle
(define cycle (mlist 'a 'b))
(set-mcdr! cycle cycle)

(define (display-newline x)
  (newline)
  (display x))

(display-newline three)
(display-newline (count-pairs three))
(display-newline four)
(display-newline (count-pairs four))
(display-newline seven)
(display-newline (count-pairs seven))
(display-newline cycle)
(display-newline (count-pairs cycle))
