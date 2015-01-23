#lang racket
(define (triple-pairs n s)
  (define (make-interval l h)
    (if (> l h) '()
        (cons l (make-interval (+ 1 l) h))))
  (define (accumulate op init lat)
    (if (null? lat) init
        (accumulate op (op (car lat) init) (cdr lat))))
  (define (triples n)
    (map (lambda (a)
           (map (lambda (b)
                  (map (lambda (c)
                         (list a b c))
                       (make-interval b n)))
                (make-interval a n)))
         (make-interval 1 n)))
  (define (refine lat)
    (accumulate append '() lat))     
  
  (define cmp
    (lambda (x y)
      (or (< (car x) (car y))
          (and (= (car x) (car y))
               (< (cadr x) (cadr y)))
          (and (= (car x) (car y))
               (= (cadr x) (cadr y))
               (< (caddr x) (caddr y))))))
  (sort
   (filter (lambda (x)
             (= (accumulate + 0 x) s))
           (refine (refine (triples n))))
   cmp))

(triple-pairs 5 10)