#lang racket
(require scheme/mpair)

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((and? exp) (analyze-and exp))
        ((or? exp) (analyze-or exp))
        ((not? exp) (analyze-not exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze (let->exec-lambda exp)))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Switch all kind of expressions ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Initiate helper functions
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
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (variable? exp) (symbol? exp))

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
; Basic conditions
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

; Assignment
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

; Definition
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

; If
(define (analyze-if exp)
  (let ((pred-proc (analyze (if-predicate exp)))
        (cons-proc (analyze (if-consequent exp)))
        (altr-proc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pred-proc env))
          (cons-proc env)
          (altr-proc env)))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Lambda
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

; Begin (Sequence, important)
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
        'ok)
    (loop (car procs) (cdr procs))))

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

(define (analyze-application exp)
  (let ((oproc (analyze (operator exp)))
        (aproc (map analyze (operands exp))))
    (lambda (env)
      (execute-application (oproc env)
                           (map (lambda (aproc) (aproc env)) aproc)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply (primitive-implementation proc) args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION"
                proc))))


;;;;;;;;; Modified ;;;;;;;;;;;
; And
(define (analyze-and exp)
  (lambda (env)
    (define (loop-clauses clauses)
      (let ((fproc (analyze (first-exp clauses))))
        (let ((fval (fproc env)))
          (cond ((last-exp? clauses)
                 (if (true? fval) fval false))
                (else
                 (if (true? fval)
                     (loop-clauses (rest-exps clauses))
                     false))))))
    (loop-clauses (and-clauses exp))))

; Or
(define (analyze-or exp)
  (lambda (env)
    (define (loop-clauses clauses)
      (let ((fproc (analyze (first-exp clauses))))
        (let ((fval (fproc env)))
          (cond ((last-exp? clauses)
                 (if (true? fval) fval false))
                (else
                 (if (true? fval) fval
                     (loop-clauses (rest-exps clauses))))))))
    (loop-clauses (or-clauses exp))))

; Not
(define (analyze-not exp)
  (let ((nproc (analyze (cadr exp))))
    (lambda (env)
      (if (true? (nproc env)) false true))))

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
