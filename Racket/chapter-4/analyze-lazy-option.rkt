#lang racket
(require scheme/mpair)

(define (mcadr lat) (mcar (mcdr lat)))
(define (mcddr lat) (mcdr (mcdr lat)))
(define (mcaadr lat) (mcar (mcadr lat)))
(define (mcaddr lat) (mcar (mcddr lat)))
(define (mcdadr lat) (mcdr (mcadr lat)))
(define (mcdddr lat) (mcdr (mcddr lat)))
(define (mcadddr lat) (mcar (mcdddr lat)))
(define (mcaddddr lat) (mcadddr (mcdr lat)))

(define (list->mlist-recur lat)
  (cond ((null? lat) (mlist))
        ((pair? lat)
         (mcons (list->mlist-recur (car lat))
                (list->mlist-recur (cdr lat))))
        (else lat)))

(define (mlist->list-recur mlat)
  (cond ((null? mlat) '())
        ((mpair? mlat)
         (cons (mlist->list-recur (mcar mlat))
               (mlist->list-recur (mcdr mlat))))
        (else mlat)))

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
                         (lambda-lazy-opt exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->exec-lambda exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (my-apply (actual-value (operator exp) env)
                   (operands exp)
                   env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;;;;;;
; Switch all kind of expressions
;;;;;;;
; Helper function
(define (tagged-list? exp tag)
  (if (mpair? exp)
      (eq? (mcar exp) tag)
      false))
(define (text-of-quotation exp) (mcadr exp))

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
(define (assignment-variable exp) (mcadr exp))
(define (assignment-value exp) (mcaddr exp))

; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define f (lambda ...))   ;
;  or                       ;
; (define (f args) (body))  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (mcadr exp))
      (mcadr exp)
      (mcaadr exp)))
(define (definition-value exp)
  (if (symbol? (mcadr exp))
      (mcaddr exp)
      (make-lambda (mcdadr exp)
                   (mcddr exp))))

; Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (lambda (args) (exprs))   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lambda? exp)
  (tagged-list? exp 'lambda))

; (arg1 (arg2 lazy) (arg3 lazy-memo) ... )
(define (lambda-parameters exp)
  (mmap (lambda (arg)
          (cond ((symbol? arg) arg)
                ((mpair? arg) (mcar arg))))
        (mcadr exp)))

(define (lambda-lazy-opt exp)
  (mmap (lambda (arg)
          (cond ((symbol? arg) 'strict)
                ((mpair? arg)
                 (cond ((eq? (mcadr arg) 'lazy) 'thunk)
                       ((eq? (mcadr arg) 'lazy-memo) 'thunk-memo)))))
        (mcadr exp)))

(define (lambda-body exp) (mcddr exp))
(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

; Let
;;;;;;;;;;;;;;;;;;;;;;;;
; (let ((var-a val-a)  ;
;       (var-b val-b)) ;
;  body)               ;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-vars exp)
  (mmap mcar (mcadr exp)))
(define (let-vals exp)
  (mmap mcadr (mcadr exp)))
(define (let-body exp) (mcddr exp))

; And
(define (and? exp)
  (tagged-list? exp 'and))
(define (and-clauses exp) (mcdr exp))

; Or
(define (or? exp)
  (tagged-list? exp 'or))
(define (or-clauses exp) (mcdr exp))

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
(define (if-predicate exp) (mcadr exp))
(define (if-consequent exp) (mcaddr exp))
(define (if-alternative exp)
  (if (not (null? (mcdddr exp)))
      (mcadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

; begin
;;;;;;;;;;;;;;;;;;;;;
; (begin            ;
;   expr-1          ;
;   expr-2 ... end) ;
;;;;;;;;;;;;;;;;;;;;;
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp) (mcdr exp))
(define (last-exp? seq)
  (null? (mcdr seq)))
(define (first-exp seq) (mcar seq))
(define (rest-exps seq) (mcdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (mcons 'begin seq))

; cond
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (cond ((cond-a expr-as)    ;
;        (cond-b expr-bs)    ;
;        (else expr-elses))) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (mcdr exp))
(define (cond-predicate clause) (mcar clause))
(define (cond-actions clause) (mcdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (mcar clauses))
            (rest (mcdr clauses)))
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
(define (application? exp) (mpair? exp))
(define (operator exp) (mcar exp))
(define (operands exp) (mcdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (mcar ops))
(define (rest-operands ops) (mcdr ops))
(define (first-opt opts) (mcar opts))
(define (rest-opts opts) (mcdr opts))

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
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Lambda
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (mcadr p))
(define (procedure-opts p) (mcaddr p))
(define (procedure-body p) (mcadddr p))
(define (procedure-environment p) (mcaddddr p))
(define (make-procedure parameters opt body env)
  (mlist 'procedure parameters opt body env))

; Let
(define (let->exec-lambda exp)
  (mcons (make-lambda (let-vars exp)
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
    (let ((bool (eval (first-exp clauses) env)))
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
    (let ((bool (eval (first-exp clauses) env)))
      (cond ((last-exp? exp)
             (if bool bool false))
            (else
             (if bool
                 bool
                 (loop-clauses (rest-exps clauses)))))))
  (loop-clauses (and-clauses exp)))

(define (eval-not exp env)
  (let ((bool (eval (mcadr exp) env)))
    (if bool false true)))

; Environment structure
;  list
;    |
; ((vars ...) . vals ...) . enclosing-environment)
;    |            |
;  mlist        mlist
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
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

; Expand of environment
(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (cons (make-frame vars (list->mlist-recur vals)) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Get & Set & Define values in the environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
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
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
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
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; Primitive functions
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcadr proc))

(define primitive-procedures
  (list->mlist-recur
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
        (list 'display display)
        (list 'newline newline)
        ; More primitives
        )))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc)))
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
(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (let ((args-val (list-of-arg-values arguments env)))
           ;(display args-val)
           (apply (primitive-implementation procedure) (mlist->list-recur args-val))))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-opt-args arguments (procedure-opts procedure) env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

; THUNK
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo))
(define (thunk-exp thunk) (mcadr thunk))
(define (thunk-env thunk) (mcaddr thunk))
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (mcadr evaluated-thunk))

(define (delay-memo-it exp env)
  (mlist 'thunk-memo exp env))

(define (delay-it exp env)
  (mlist 'thunk exp env))

(define (force-it obj)
  (cond ((thunk-memo? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-mcar! obj 'evaluated-thunk)
           (set-mcar! (mcdr obj) result)
           (set-mcdr! (mcdr obj) (mlist))
           result))
        ((thunk? obj)
         (actual-value (thunk-exp obj)
                       (thunk-env obj)))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (actual-value exp env)
  (force-it (eval exp env)))

; Convert from mlist to list for primitive evaluation
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      (mlist)
      (mcons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
; Remains mlist
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      (mlist)
      (mcons (delay-it (first-operand exps) env)
             (list-of-delayed-args (rest-operands exps)
                                   env))))

; Mapped mlist
(define (list-of-opt-args exps opts env)
  (if (no-operands? exps)
      (mlist)
      (mcons (let ((opt (first-opt opts))
                   (exp (first-operand exps)))
               (cond ((eq? opt 'strict) (actual-value exp env))
                     ((eq? opt 'thunk) (delay-it exp env))
                     ((eq? opt 'thunk-memo) (delay-memo-it exp env))))
             (list-of-opt-args (rest-operands exps)
                               (rest-opts opts)
                               env))))

(define (read-eval-print-loop)
  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (begin
          (let ((val (actual-value (list->mlist-recur exp) glb-env)))
            (if (or (eq? val 'ok) (void? val))
                (read-eval-print-loop)
                (begin
                  (display val)
                  (newline)
                  (read-eval-print-loop))))))))

(read-eval-print-loop)
