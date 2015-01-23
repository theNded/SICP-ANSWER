; Basic Structure:
;  car                cdr
; value      car              cdr
;        ptr-to-prev      ptr-to-next

(define (make-deque) (cons '() '()))
(define (empty-deque? deque) (null? (get-front deque)))

(define (get-front  deque) (car deque))
(define (set-front! deque ptr) (set-car! deque ptr))

(define (get-rear  deque) (cdr deque))
(define (set-rear! deque ptr) (set-cdr! deque ptr))

(define (get-prev-ptr ptr) (cadr ptr))
(define (set-prev-ptr! ptr prev)
  (set-car! (cdr ptr) prev))

(define (get-next-ptr ptr) (cddr ptr))
(define (set-next-ptr! ptr next)
  (set-cdr! (cdr ptr) next))

(define (get-value deque ptr)
  (if (empty-deque? deque)
      "Error: empty deque -- GET-VALUE"
      (car ptr)))

(define (front-deque deque)
  (get-value deque (get-front deque)))

(define (rear-deque deque)
  (get-value deque (get-rear deque)))

(define (insert-deque! deque value pos)
  (let ((new-item (cons value (cons '() '()))))
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
             (set-prev-ptr! (get-front deque) '()) '()))
        ((eq? pos 'rear)
         (set-rear! deque (get-prev-ptr (get-rear deque)))
         (if (not (null? (get-rear deque)))
             (set-next-ptr! (get-rear deque) '())))))


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
    (if (null? x) '() (cons (car x) (iter (cddr x)))))
  (iter (get-front deque)))

(define q (make-deque))
(front-insert-deque! q 1)
(front-insert-deque! q 3)
(rear-insert-deque! q 2)
(rear-insert-deque! q 4)
(newline)
(display (print-deque q))

(rear-delete-deque! q)
(front-delete-deque! q)
(newline)
(display (print-deque q))
