#lang racket
(require scheme/mpair)

(define (floyd-cycle? x)
  (define (safe-cdr x)
    (if (mpair? x) (mcdr x) #f))

  (define (tortoise x) (safe-cdr x))
  (define (hare x) (safe-cdr (safe-cdr x)))

  (define (iter a b)
    (cond ((or (not a) (not b)) #f)
          ((eq? a b) #t)
          (else
           (iter (tortoise a) (hare b)))))

  (iter (tortoise x) (hare x)))

(define normal (mlist 'a 'b 'c 'd))
(display normal)
(newline)
(display (floyd-cycle? normal))
(newline)

(define cycle (mlist 'a 'b 'c 'd))
(set-mcdr! (mcdr (mcdr (mcdr cycle))) cycle)
(display cycle)
(newline)
(display (floyd-cycle? cycle))
(newline)

(define cycle-1 (mlist 'a 'a))
(set-mcdr! cycle-1 cycle-1)
(display cycle-1)
(newline)
(display (floyd-cycle? cycle))
