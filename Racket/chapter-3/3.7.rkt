#lang racket
; 3.3 is required.
(define (make-account balance passwd)
  (lambda (certify-passwd operation)
    (if (eq? passwd certify-passwd)
        (cond ((eq? operation 'withdraw)
               (lambda (amount)
                 (if (>= balance amount)
                     (begin (set! balance (- balance amount))
                            balance)
                     "Insufficient funds!")))
              ((eq? operation 'deposit)
               (lambda (amount)
                 (set! balance (+ balance amount))
                 balance))
              ((eq? operation 'certify) #t)
              (else "Invalid operation!"))
        (lambda args
          "Invalid password."))))

(define (make-joint main-account certify-passwd new-passwd)
  ; Clumsy method to handle error
  (if (main-account certify-passwd 'certify)
      ; Joint established
      (lambda (certify-new-passwd operation)
        (if (eq? new-passwd certify-new-passwd)
            (cond ((eq? operation 'withdraw)
                   (lambda (amount)
                     ((main-account certify-passwd 'withdraw) amount)))
                  ((eq? operation 'deposit)
                   (lambda (amount)
                     ((main-account certify-passwd 'deposit) amount)))
                  (else "Invalid operation!"))
            (lambda args
              "Invalid password.")))
      "Joint creation failed."))

; Test for 3.3
(define pku-acc (make-account 100 'PKU))

(newline)
(display ((pku-acc 'PKU 'withdraw) 80))
(newline)
(display ((pku-acc 'PKU 'withdraw) 10))
(newline)
(display ((pku-acc 'PKU 'withdraw) 30))
(newline)
(display ((pku-acc 'PKU 'deposit)  1000))
(newline)
(display ((pku-acc 'THU 'withdraw) 1))

; Test for 3.7
(define thu-acc (make-joint pku-acc 'PKU 'THU))
(newline)
(display ((thu-acc 'THU 'withdraw) 0.1))
(newline)
(display ((thu-acc 'THU 'deposit) 100))
