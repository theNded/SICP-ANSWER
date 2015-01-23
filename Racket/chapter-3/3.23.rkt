#lang racket
(require racket/mpair)
; Basic Structure:
;  mcar                mcdr
;  value      mcar              mcdr
;         ptr-to-prev      ptr-to-next
; Deque itself has only front and rear two ptrs
(define (make-deque) (mcons (mlist) (mlist)))
(define (empty-deque? deque) (null? (get-front deque)))

(define (get-front  deque) (mcar deque))
(define (set-front! deque ptr) (set-mcar! deque ptr))

(define (get-rear  deque) (mcdr deque))
(define (set-rear! deque ptr) (set-mcdr! deque ptr))

(define (get-prev-ptr ptr) (mcar (mcdr ptr)))
(define (set-prev-ptr! ptr prev)
  (set-mcar! (mcdr ptr) prev))

(define (get-next-ptr ptr) (mcdr (mcdr ptr)))
(define (set-next-ptr! ptr next)
  (set-mcdr! (mcdr ptr) next))

(define (get-value deque ptr)
  (if (empty-deque? deque)
      "Error: empty deque -- GET-VALUE"
      (mcar ptr)))

(define (front-deque deque)
  (get-value deque (get-front deque)))

(define (rear-deque deque)
  (get-value deque (get-rear deque)))

(define (insert-deque! deque value pos)
  (let ((new-item (mcons value (mcons (mlist) (mlist)))))
    (cond ((empty-deque? deque)
           (set-front! deque new-item)
           (set-rear!  deque new-item))
          ((eq? pos 'front)
           (set-prev-ptr! (get-front deque) new-item)
           (set-next-ptr! new-item (get-front deque))
           (set-front! deque new-item))
          ((eq? pos 'rear)
           (set-prev-ptr! new-item (get-rear deque))
           (set-next-ptr! (get-rear deque) new-item)
           (set-rear! deque new-item))
          (else
           "Error: invalid option -- insert-deque!"))))

(define (delete-deque! deque pos)
  (cond ((empty-deque? deque)
         "Error: empty deque -- delete-deque!")
        ((eq? pos 'front)
         (set-front! deque (get-next-ptr (get-front deque)))
         (if (not (empty-deque? deque))
             (set-prev-ptr! (get-front deque) (mlist)) (mlist)))
        ((eq? pos 'rear)
         (set-rear! deque (get-prev-ptr (get-rear deque)))
         (if (not (null? (get-rear deque)))
             (set-next-ptr! (get-rear deque) (mlist))
             (void)))))


(define (front-insert-deque! deque value)
  (insert-deque! deque value 'front))

(define (front-delete-deque! deque)
  (delete-deque! deque 'front))

(define (rear-insert-deque! deque value)
  (insert-deque! deque value 'rear))

(define (rear-delete-deque! deque)
  (delete-deque! deque 'rear))

(define (print-deque deque)
  (define (iter x)
    (if (null? x) (mlist) 
        (mcons (mcar x) (iter (mcdr (mcdr x))))))
  (iter (get-front deque)))

(define q (make-deque))
(front-insert-deque! q 1)
(front-insert-deque! q 3)
(rear-insert-deque! q 2)
(rear-insert-deque! q 4)
(display (print-deque q))

(rear-delete-deque! q)
(front-delete-deque! q)
(newline)
(display (print-deque q))
