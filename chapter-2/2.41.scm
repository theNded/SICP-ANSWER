; mit-scheme has no pre-compiled accumulate method ...
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (enumerate-interval l h)
  (if (> l h)
      '()
      (cons l
            (enumerate-interval (+ l 1) h))))

(define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (list j i))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (unique-triples n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (cons j i))
                          (enumerate-interval 1 (- (car i) 1))))
                   (unique-pairs n))))

(define (triple-sum-n-verbose? n)
  (filter (lambda (i)
            (= (+ (car i) (cadr i) (caddr i)) n))
          (accumulate append '()
                      (accumulate append '()
                                  (map (lambda (i)
                                         (map (lambda (j)
                                                (map (lambda (k) (list k j i))
                                                     (enumerate-interval 1 (- j 1))))
                                              (enumerate-interval 1 (- i 1))))
                                       (enumerate-interval 1 n))))))

(define (triple-sum-n? n)
  (filter (lambda (i)
            (= (+ (car i) (cadr i) (caddr i)) n))
          (unique-triples n)))
