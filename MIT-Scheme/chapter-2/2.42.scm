(define (enumerate-interval l h)
  (if (> l h)
      '()
      (cons l (enumerate-interval (+ l 1) h))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define (iter i row-test-queens row-new-queen)
    (if (= k i)
        #t
        (let ((row-test-queen (car row-test-queens)))
          (cond ((= row-new-queen row-test-queen) #f)
                ((= (abs (- row-new-queen row-test-queen)) i) #f)
                (else (iter (+ i 1) (cdr row-test-queens) row-new-queen))))))
  (iter 1 (cdr positions) (car positions)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
