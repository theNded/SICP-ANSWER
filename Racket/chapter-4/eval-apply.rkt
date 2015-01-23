#lang racket
(require scheme/mpair)

; Evaluate framework
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((not? exp) (eval-not exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->exec-lambda exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (my-apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" env))))

;;;;;;;
; Switch all kind of expressions
;;;;;;;
; Helper function
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (text-of-quotation exp) (cadr exp))

; Basic conditions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))

; Assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define f (lambda ...))   ;
; or                        ;
; (define (f args) (body))  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (lambda (args) (exprs))   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; Let
;;;;;;;;;;;;;;;;;;;;;;;;
; (let ((var-a val-a)  ;
;       (var-b val-b)) ;
;  body)               ;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-vars exp)
  (map car (cadr exp)))
(define (let-vals exp)
  (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))

; And
(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

; Or
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

; Not
(define (not? exp)
  (tagged-list? exp 'not))

; if
;;;;;;;;;;;;;;;;;;;;
; (if predicate    ;
;     consequent   ;
;     alternative) ;
;;;;;;;;;;;;;;;;;;;;
(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


; begin
;;;;;;;;;;;;;;;;;;;;;
; (begin            ;
;   expr-1          ;
;   expr-2 ... end) ;
;;;;;;;;;;;;;;;;;;;;;
(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

; cond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (cond ((cond-a expr-as)    ;
;        (cond-b expr-bs)    ;
;        (else expr-elses))) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                ; need modify for test4.in
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (if (null? (cond-actions first))
                         (cond-predicate first)
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

; Other situations
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Implementation of all kinds of expressions ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; Definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; If
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Lambda
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Let
(define (let->exec-lambda exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-vals exp)))

; Begin
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;;;;;;;; Modified ;;;;;;;;;;;
; And
(define (eval-and exp env)
  (define (loop-clauses clauses)
    (let ((bool (eval (car clauses) env)))
      (cond ((last-exp? clauses)
             (if bool bool false))
            (else
             (if bool
                 (loop-clauses (rest-exps clauses))
                 false)))))
  (loop-clauses (and-clauses exp)))

; Or
(define (eval-or exp env)
  (define (loop-clauses clauses)
    (let ((bool (eval (car clauses) env)))
      (cond ((last-exp? exp)
             (if bool bool false))
            (else
             (if bool
                 bool
                 (loop-clauses (rest-exps clauses)))))))
  (loop-clauses (and-clauses exp)))

(define (eval-not exp env)
  (let ((bool (eval (cadr exp) env)))
    (if bool false true)))

; Others
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Environment structure
;  list
;    |
; ((vars ...) . vals ...) . enclosing-environment)
;    |            |
;  list        mlist
;     \         /
;        mlist
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; Frame structure
(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

; Expand of environment
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Get & Set & Define values in the environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Primitive functions
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'symbol? symbol?)
        (list 'number? number?)
        (list 'variable? variable?)
        (list 'eq? eq?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '> >)
        (list 'remainder remainder)
        (list 'length length)
        (list 'append append)
        (list 'sqrt sqrt)
        ; More primitives
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

; Setup environment
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define glb-env (setup-environment))

; Apply framework
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (read-eval-print-loop)
  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (begin
          (let ((val (eval exp glb-env)))
            (if (eq? val 'ok)
                (read-eval-print-loop)
                (begin
                  (display val)
                  (newline)
                  (read-eval-print-loop))))))))

(read-eval-print-loop)

