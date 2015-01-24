#lang racket
(require scheme/mpair)

(define mcddr (lambda (mlat) (mcdr (mcdr mlat))))
(define (tagged-list? exp tag)
  (cond ((pair? exp) (eq? (car exp) tag))
        ((mpair? exp) (eq? (mcar exp) tag))
        (else false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal data structures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ('primitive #<procedure: XXX>) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (primitive-procedure? proc)
  (procedure? proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ('procedure (arg-1 arg-2) (proc-seq-1 proc-seq-2) env) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (list-ref p 1))
(define (procedure-body p) (list-ref p 2))
(define (procedure-environment p) (list-ref p 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; { (<class-1> <class-2>) . proc) } ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-method mcons)
(define method-specializers mcar)
(define method-procedure mcdr)
(define (method-set-procedure! method proc)
  (set-mcdr! method proc))

; given ((cat <cat>) (stuff <object>) 1)
; -> (cat stuff 1)
(define (paramlist-element-name paramlist-element)
  (if (pair? paramlist-element)
      (car paramlist-element)
      paramlist-element))

; -> (<cat> <object> <object>)
(define (paramlist-element-class-name paramlist-element)
  (if (pair? paramlist-element)
      (cadr paramlist-element)
      '<object>))

; method1: (<class-1> <class-2>)
; method2: (<class-1> <class-3>) -> false
(define (same-specializers? method1 method2)
  (define (check spec1 spec2)
    (cond ((and (null? spec1) (null? spec2)) true)
          ((or (null? spec1) (null? spec2)) false)
          ((eq? (car spec1) (car spec2))
           (check (cdr spec1) (cdr spec2)))
          (else false)))
  (check (method-specializers method1)
         (method-specializers method2)))

; supplied: <class-1> <class-2>
; required: <class-1-super> <class-2>
(define (method-applies-to-classes? method classes)
  (define (check-classes supplied required)
    (cond ((and (null? supplied) (null? required)) true)
          ((or (null? supplied) (null? required)) false)
          ((subclass? (car supplied) (car required))
           (check-classes (cdr supplied) (cdr required)))
          (else false)))
  (check-classes classes (method-specializers method)))

; Similar to method-applies-to-classes?
(define (method-more-specific? method1 method2)
  (define (check-classes c1 c2)
    (cond ((and (null? c1) (null? c2)) true)
          ((or (null? c1) (null? c2))
           (error "Bug: method lists not of same length"))
          ((subclass? (car c1) (car c2))
           (check-classes (cdr c1) (cdr c2)))
          (else false)))
  (check-classes (method-specializers method1)
                 (method-specializers method2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; {'generic-function f method-1 method-2 }            ;
; where method-i = { (<class-i1> <class-i2>) proc-i } ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-generic-function name)
  (mlist 'generic-function name))
(define (generic-function? exp) (tagged-list? exp 'generic-function))
(define (generic-function-name exp) (mlist-ref exp 1))
(define (generic-function-methods generic-function)
  (mcddr generic-function))
(define (generic-function-set-methods! generic-function methods)
  (set-mcdr! (mcdr generic-function) methods))

(define (add-method-to-generic-function! method generic-function)
  (let ((method-list (generic-function-methods generic-function)))
    (let ((current-method 
           (find-existing-method method method-list)))
      (if current-method
          (method-set-procedure! current-method (method-procedure method))
          (generic-function-set-methods!
           generic-function (mcons method method-list))))))

(define (find-existing-method method method-list)
  (cond ((null? method-list) false)
        ((same-specializers? method (mcar method-list)) (mcar method-list))
        (else (find-existing-method method (mcdr method-list)))))

(define (install-method-in-generic-function gf specializers proc)
  (let ((method (make-method specializers proc)))
    (add-method-to-generic-function! method gf)))

; (compute-applicable-methods-using-classes '(<class-1> <class-2>))
; -> 
; ((<class-1> <class-2>) (<class-1-super> <class-2>))
(define (compute-applicable-methods-using-classes generic-function classes)
  (let ((methods 
         (sort
          (filter
           (lambda (method)
             (method-applies-to-classes? method classes))
           (mlist->list (generic-function-methods generic-function)))
          method-more-specific?)))
    ;(printf "methods: ~a ~n" methods)
    methods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ('class <class-name> (<superclass-1> <superclass-2>) ;
;                      (slotname-1 slotname-2))        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-class name superclass slot-names)
  (let ((subsuming
         (if (null? superclass) '()
             (cons superclass (class-ancestors superclass)))))
    (list 'class name subsuming slot-names)))

(define (class? exp) (tagged-list? exp 'class))
(define (class-name class) (list-ref class 1))
(define (class-ancestors class) (list-ref class 2))
(define (class-slot-names class) (list-ref class 3))
(define *primitive-class* (make-class '<object> '() '()))

(define (class-of object)
  (if (standard-instance? object)
      (instance-class object)
      (built-in-class object)))

(define (subclass? class1 class2)
  (or (eq? class1 class2)
      (memq class2 (class-ancestors class1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ('instance <class-name> {slot-val-1 slot-val-2}) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-standard-instance class slot-values)
  (list 'instance class slot-values))
(define (standard-instance? exp) (tagged-list? exp 'instance))
(define (instance-class obj) (list-ref obj 1))
(define (instance-slot-values obj) (list-ref obj 2))

(define slot-name car)
(define slot-value cadr)

;;;;;;;;;;;;;;;;;;;;;;
;;; EVAL FRAMEWORK ;;;
;;;;;;;;;;;;;;;;;;;;;;
(define (tool-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        
        ((definition? exp) (eval-definition exp env))        
        ((assignment? exp) (eval-assignment exp env))
        
        ((if? exp) (eval-if exp env))
        ((cond? exp) (tool-eval (cond->if exp) env))                
        ((and? exp) (tool-eval (and->if exp) env))
        ((or? exp) (tool-eval (or->if exp) env))
        ((not? exp) (tool-eval (not->if exp) env))
        
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (tool-eval (let->combination exp) env))
        
        ; OOP
        ((generic-function-definition? exp)
         (eval-generic-function-definition exp env)) 
        ((method-definition? exp) (eval-define-method exp env))
        ((class-definition? exp) (eval-define-class exp env))
        ((instance-creation? exp) (eval-make exp env))
        
        ((application? exp)
         (tool-apply (tool-eval (operator exp) env)
                     (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexical structures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
; Basic conditions ;
;;;;;;;;;;;;;;;;;;;;
; 18               ;
; "Naive"          ;
; '(1 2 3)         ;
;;;;;;;;;;;;;;;;;;;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definitions              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define f (lambda ...))  ;
; or                       ;
; (define (f args) (body)) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp)) (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp)) (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assignment                      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (set! inc (lambda (x) (+ x 1))) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (list-ref exp 1))
(define (assignment-value exp) (list-ref exp 2))

;;;;;;;;;;;;;;;;
;; Conditions ;;
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
; (if predicate    ;
;     consequent   ;
;     alternative) ;
;;;;;;;;;;;;;;;;;;;;
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (list-ref exp 1))
(define (if-consequent exp) (list-ref exp 2))
(define (if-alternative exp)
  (if (not (null? (cdddr exp))) (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (cond ((cond-a expr-as)    ;
;        (cond-b expr-bs)    ;
;        (else expr-elses))) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses) 'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest) (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (if (null? (cond-actions first)) (cond-predicate first)
                         (sequence->exp (cond-actions first)))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (and clause-1 clause-2) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (and->if exp)
  (define (expand-and seq)
    (if (last-exp? seq) (first-exp seq)
        (make-if (first-exp seq) (expand-and (rest-exps seq))
                 'false)))
  (expand-and (and-clauses exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; (or clause-1 clause-2) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))
(define (or->if exp)
  (define (expand-or seq)
    (if (last-exp? seq) (first-exp seq)
        (make-if (first-exp seq) (first-exp seq)
                 (expand-or (rest-exps seq)))))
  (expand-or (or-clauses exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;
; (not clause-negative) ;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (not? exp) (tagged-list? exp 'not))
(define (not->if exp)
  (make-if (cadr exp) 'false 'true))

;;;;;;;;;;;;;;;;;;;;;
; Sequence          ;
;;;;;;;;;;;;;;;;;;;;;
; (begin            ;
;   expr-1          ;
;   expr-2 ... end) ;
;;;;;;;;;;;;;;;;;;;;;
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq)
  (cons 'begin seq))

;;;;;;;;;;;;;;;
;; Procedures ;
;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (lambda (args) (exprs)) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

;;;;;;;;;;;;;;;;;;;;;;;;
; (let ((var-a val-a)  ;
;       (var-b val-b)) ;
;  body)               ;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-vals exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (let-vals exp)))

;;;;;;;;;
;; OOP ;;
;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-generic-function f) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generic-function-definition? exp) 
  (tagged-list? exp 'define-generic-function))
(define (generic-function-definition-name exp) (list-ref exp 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-method f ((var-1 <class-1>) (var-2 <class-2>)) ;
;   body)                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (method-definition? exp) (tagged-list? exp 'define-method))
(define (method-definition-generic-function exp) (list-ref exp 1))
(define (method-definition-parameters exp) (list-ref exp 2))
(define (method-definition-body exp) (list-tail exp 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-class <class> <superclass> slot-1 slot-2) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (class-definition? exp) (tagged-list? exp 'define-class))
(define (class-definition-name exp) (list-ref exp 1))
(define (class-definition-superclass exp) (list-ref exp 2))
(define (class-definition-slot-names exp) (list-tail exp 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (make <class-name> (slot-1 val-1) (slot-2 val-2)) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (instance-creation? exp) (tagged-list? exp 'make))
(define (instance-creation-class exp) (list-ref exp 1))
(define (instance-creation-slots exp) (list-tail exp 2))

;;;;;;;;;;;;;;;
; Application ;
;;;;;;;;;;;;;;;
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluation of all kinds of expressions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (tool-eval (definition-value exp) env)
    env)
  (void))

; Assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (tool-eval (assignment-value exp) env)
                       env)
  (void))

; If
(define (eval-if exp env)
  (if (true? (tool-eval (if-predicate exp) env))
      (tool-eval (if-consequent exp) env)
      (tool-eval (if-alternative exp) env)))
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; Begin
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (tool-eval (first-exp exps) env))
        (else (tool-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; OOP ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-generic-function f) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval-generic-function-definition exp env)
  (let ((name (generic-function-definition-name exp)))
    (let ((val (make-generic-function name)))
      (define-variable! name val env)
      (list 'defined 'generic 'function: name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-method f ((var-1 <class-1>) (var-2 <class-2>)) ;
;   body)                                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval-define-method exp env)
  (let ((gf-name (method-definition-generic-function exp)))
    (let ((gf (tool-eval gf-name env)))
      (if (not (generic-function? gf))
          (let ((gf-create (make-generic-function gf-name)))
            (define-variable! gf-name gf-create glb-env)
            (set! gf (tool-eval gf-name env))
            (write-line (list 'defined 'generic 'function: gf-name)))
          (void))
      (let ((params (method-definition-parameters exp)))
        (install-method-in-generic-function 
         gf          
         (map (lambda (p) (paramlist-element-class p env))
              params)
         (make-procedure (map paramlist-element-name params)
                         (method-definition-body exp)
                         env))
        (list 'added 'method 'to 'generic 'function:
              (generic-function-name gf))))))

(define (paramlist-element-class p env)
  (let ((class (tool-eval (paramlist-element-class-name p) env)))
    (if (class? class) class
        (error "Unrecognized class -- DEFINE-METHOD >> " class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (define-class <class> <superclass> slot-1 slot-2) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval-define-class exp env)
  (let ((superclass (tool-eval (class-definition-superclass exp)
                               env)))
    (if (not (class? superclass))
        (error "Unrecognized superclass -- MAKE-CLASS >> "
               (class-definition-superclass exp))
        (let ((name (class-definition-name exp))
              (all-slots (collect-slots
                          (class-definition-slot-names exp)
                          superclass)))
          (let ((new-class
                 (make-class name superclass all-slots)))            
            (define-variable! name new-class env)
            (for-each
             (lambda (slot)
               (tool-eval `(define-method ,slot ((x ,name)) (get-slot x ',slot)) env))
             all-slots)
            (list 'defined 'class: name))))))

(define (collect-slots slot-names superclass)
  (let ((superclass-slots (class-slot-names superclass)))
    (if (good-slot-names slot-names superclass-slots)
        ;!!!! BUG: This should have duplicates removed
        (append slot-names superclass-slots)
        (error "Bad slot list -- MAKE-CLASS >> "
               slot-names
               superclass-slots))))

(define (good-slot-names slots superclass-slots)
  (or (null? slots)
      (and (symbol? (car slots))
           (not (memq (car slots) (cdr slots)))
           (not (memq (car slots) superclass-slots))
           (good-slot-names (cdr slots) superclass-slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; (make <class-name> (slot-1 val-1) (slot-2 val-2)) ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (eval-make exp env)
  (let ((class (tool-eval (instance-creation-class exp) env)))
    (if (not (class? class))
        (error "Unrecognized class -- MAKE >> "
               (instance-creation-class exp))
        (let ((slots (instance-creation-slots exp)))
          (let ((specified-slot-names (map slot-name slots))
                (specified-slot-values
                 (map (lambda (s) (tool-eval (slot-value s) env))
                      slots)))
            (make-standard-instance class
                                    (make-instance-slots
                                     specified-slot-names specified-slot-values
                                     (class-slot-names class))))))))

(define (make-instance-slots names values all-names)
  (list->mlist 
   (map (lambda (name)
          (get-initial-slot-value name names values))
        all-names)))

(define (get-initial-slot-value name names values)
  (cond ((null? names) '*undefined*)
        ((eq? name (car names)) (car values))
        (else (get-initial-slot-value name
                                      (cdr names)
                                      (cdr values)))))

(define (get-slot object slot-name)
  (if (not (standard-instance? object))
      (error "Unrecognized object -- GET-SLOT >> " object)
      (mcar (designated-value slot-name object))))

(define (set-slot! object slot-name value)
  (if (not (standard-instance? object))
      (error "Unrecognized object -- SET-SLOT! >> " object)
      (set-mcar! (designated-value slot-name object)
                 value))
  (void))

(define (designated-value name object)
  (let ((v (named-position name
                           (class-slot-names (instance-class object))
                           (instance-slot-values object))))
    (if v v
        (error "Bad slot name for object >> " name v))))

(define (named-position name namelist valuelist)
  (cond ((null? namelist) false)
        ((eq? name (car namelist)) valuelist)
        (else (named-position name
                              (cdr namelist)
                              (mcdr valuelist)))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; APPLY FRAMEWORK ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(define (tool-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        ((generic-function? procedure)
         (apply-generic-function procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))

; Argument values
(define (list-of-values exps env)
  (if (no-operands? exps) '()
      (cons (tool-eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; (or (apply ('procedure ...) arguments)
;     (apply ('primitive ...) arguments)
(define (apply-generic-function generic-function arguments)
  (let ((methods (compute-applicable-methods-using-classes
                  generic-function
                  (map class-of arguments)))) 
    (if (null? methods)
        (error "No method found -- APPLY-GENERIC-FUNCTION")
        (tool-apply (method-procedure (car methods)) arguments))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Environment structure ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  list, the total environment
;    |
; ((vars ...) . vals ...) enclosing-environment-1 enclosing-environment-n)
;    |            |
;  list        mlist
;     \         /
;        mlist, a frame, namely the car of the total environment
; Frame and Environment
(define (first-frame env) (car env))
(define (enclosing-environment env) (cdr env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; Define & Get & Set values in the environment
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

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (begin (printf "Unbound variable: ~a~n" var) (void))
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

;;;;;;;;;;;;;;;;;;;;;;;
;;; Initializations ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(define scheme-object-classes
  (list
   (list '<boolean> boolean?)
   (list '<number> number?)
   (list '<symbol> symbol?)
   (list '<list> list?)
   (list '<procedure> (lambda (x)
                        (or (compound-procedure? x)
                            (primitive-procedure? x)
                            (generic-function? x))))
   ))

(define (built-in-class object)
  (define (check-scheme-classes classes)
    (if (null? classes)
        *primitive-class*
        (let ((test-class (car classes)))
          (if ((cadr test-class) object)
              (lookup-variable-value (car test-class)
                                     glb-env)
              (check-scheme-classes (cdr classes))))))
  (check-scheme-classes scheme-object-classes))

(define initial-objects
  (list
   (list 'true true)
   (list 'false false)
   (list 'nil '())
   ))

(define (write-line x) 
  (display x) (newline))

(define (print object)
  (cond ((standard-instance? object)
         (write-line
          (list 'instance 'of
                (class-name (instance-class object)))))
        ((class? object)
         (write-line (list 'the 'class (class-name object))))
        ((generic-function? object)
         (write-line
          (list 'the 'generic 'function (generic-function-name object))))
        (else (write-line object))))

(define initial-procedures
  (list
   (list '+ '(<number> <number>) +)
   (list '- '(<number> <number>) -)
   (list '* '(<number> <number>) *)
   (list '/ '(<number> <number>) /)
   (list '= '(<number> <number>) =)
   (list '> '(<number> <number>) >)
   (list '< '(<number> <number>) <)
   (list 'sqrt '(<number>) sqrt)
   (list 'cons '(<object> <object>) cons)
   (list 'append '(<list> <list>) append)
   (list 'car '(<list>) car)
   (list 'cdr '(<list>) cdr)
   (list 'null? '(<object>) null?)
   (list 'print '(<object>) print)
   (list 'get-slot '(<object> <symbol>) get-slot)
   (list 'set-slot! '(<object> <symbol> <object>) set-slot!)
   ))



; Setup environment
(define (setup-environment)
  (let ((initial-object-names  (map car initial-objects))
        (initial-object-values (map cadr initial-objects)))
    (let ((initial-env
           (extend-environment initial-object-names
                               initial-object-values
                               the-empty-environment)))
      ;;define the initial class, called <object>
      (define-variable! '<object> *primitive-class* initial-env)
      ;;define the classes that come from Scheme objects
      (for-each
       (lambda (entry)
         (tool-eval
          `(define-class ,(car entry) <object>)
          initial-env))
       scheme-object-classes)
      ;;install initial generic functions and their methods
      (for-each
       (lambda (entry)
         (tool-eval `(define-generic-function ,(car entry))
                    initial-env)
         (let ((gf (tool-eval (car entry) initial-env))
               (specializers
                (map
                 (lambda (c) (lookup-variable-value c initial-env))
                 (cadr entry))))
           (install-method-in-generic-function gf
                                               specializers
                                               (caddr entry))))
       initial-procedures)
      initial-env)))

(define glb-env (setup-environment))

(define (read-eval-print-loop)
  (let ((exp (read)))
    (if (eq? exp eof)
        (void)
        (begin
          (let ((val (tool-eval exp glb-env)))
            (if (eq? val (void))
                (read-eval-print-loop)
                (begin
                  (display val)
                  (newline)
                  (read-eval-print-loop))))))))
(read-eval-print-loop)
