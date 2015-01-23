#lang racket
(require scheme/mpair)

(define (cycle? x)
  (define (iter x visited)
    (cond ((not (mpair? x)) #f)
          ((null? (mcdr x)) #f)
          ((mmemq (mcdr x) visited) #t)
          (else (iter (mcdr x) (mcons x visited)))))
  (iter x (mlist)))
           
(define cycle (mlist 'a 'b))
(set-mcdr! cycle cycle)
(display cycle)
(newline)
(display (cycle? cycle))
(newline)

(define t0 (mcons 'a 'b))
(define t1 (mcons t0 t0))
(display t1)
(newline)
(display (cycle? t1))
