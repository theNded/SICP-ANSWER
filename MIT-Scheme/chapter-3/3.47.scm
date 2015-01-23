#lang racket
(require scheme/mpair)

(define (make-mutex)
  (let ((cell (mlist false)))

    (define (clear! cell)
      (set-mcar! cell false))

    (define (test-and-set! cell)
      (if (mcar cell)
          true
          (begin (set-mcar! cell false)
                 false)))

    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (dispatch 'acquire) (void)))
            ((eq? m 'release)
             (clear! cell))))

    dispatch))


(define (make-semaphore-by-mutex n)
  (let ((mutex (make-mutex))
        (counter n))

    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> counter 0)
                 (begin (set! counter (- counter 1))
                        (mutex 'release)
                        'done)
                 (begin (mutex 'release)
                        (dispatch 'acquire))))

            ((eq? m 'release)
             (mutex 'acquire)
             (set! counter (+ counter 1))
             (mutex 'release)
             'done)))
    dispatch))

(define (make-semaphore-by-test-and-set! n)
  (let ((counter n))

    (define (test-and-set! n)
      (without-interrupts
       (lambda ()
         (if (= n 0) true
             (begin (set! n (- n 1)) false)))))

    (define (dispatch m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! counter)
                 (dispatch acquire)
                 'done))
            ((eq? m 'release)
             (set! counter (+ counter 1))
             'done)))
    dispatch))
