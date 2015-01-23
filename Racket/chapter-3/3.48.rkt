#lang racket
(define (make-counter)
  (let ((i 0))
    (lambda ()
      (set! i (+ i 1))
      i)))

(define counter (make-counter))

(define (make-account balance)
  (let ((id (counter)))

    (define (withdraw amount)
      (if (>= amount balance)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient Funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (let ((balance-serializer (make-serializer)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'balance) balance)
              ((eq? m 'serializer) balance-serializer)
              ((eq? m 'id) id)
              (else (error "Unknown request -- MAKE-ACCOUNT"))))
      dispatch)))

(define (serialized-exchange account-1 account-2)
  (define (serialized-exchange-small-big acc-1 acc-2)
    (let ((serializer-1 (acc-1 'serializer))
          (serializer-2 (acc-2 'serializer)))
      (serializer1 (serializer-2 exchange)
                   acc-1
                   acc-2)))

  (if (< (account-1 'id) (account-2 'id))
      (serialized-exchange-small-big account-1 account-2)
      (serialized-exchange-small-big account-2 account-1)))
