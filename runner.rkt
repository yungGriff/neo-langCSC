#lang racket
(require "utility.rkt")

(define resolve_scope;((a 1) (b 2) (c 5)), it gives two kinds of result. found return a value
  ; not found return #false
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
      )
    )
  )

;environment is a list of scopes
;global variable scope (global (a 1) (b 2) (c 5))
;local variable scope has no keywords as the first element
(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
       );if we resolve_scope returns a value that is what we are looking for
      ;otherwise, if resolve_scope returns #false, we should look up global variable scope
      )
    )
  )
                                 
;it will be only called in let-exp
(define extend-scope
  (lambda (list-of-varname list-of-value scope) ;((x y z) (1 2 3) env)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extend-scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
      )
    )
  )

(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    ;construct a new scope based on list of varnames and list of values
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env))) ;pop off scopes on top of global scope in environment
      (cons new_scope pop_off_env) ;concate the new scope to the global scope environment
      )
    )
  )

;remove all scopes on top of global scope
(define pop_env_to_global_scope
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env
           #false))
      (else (pop_env_to_global_scope (cdr env)))
      )
    )
  )
(define update-scope
  (lambda (varname value scope)
    (letrec (
           (check-varname-in-pair (lambda (pair) (equal? (car pair) varname)))
           (check-varname-in-scope
            (lambda (curr_scope)
              (cond
                ((null? curr_scope) #false)
                (else
                 (if (check-varname-in-pair (car curr_scope)) #true
                     (check-varname-in-scope (cdr curr_scope))
                     )
                 )
                )
              )
            )
           )
      (cond
        ((null? scope) (cons (list varname value) scope))
        ((equal? varname (caar scope)) (cons (list varname value) (cdr scope)))
        (else
         (if (check-varname-in-scope scope) 
             (cons (car scope) (update-scope varname value (cdr scope)))
             (cons (list varname value) scope)
             )
         )
        )
      )
    )
  )
;add name value pairs to the local scope
(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ;check the first scope is local scope or not
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      ;use extend_scope function to add new variables into the local scope
      (else (cons (extend-scope list-of-varname list-of-value (car env))
                  (pop_env_to_global_scope env)))
     )
    )
  )

(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve_env env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code) env))
      ;(math-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      ;(app-exp (func-exp (params (x)) (body-exp (let-exp ((a 1) (b 2) (c 3)) (math-exp + (var-exp a) (var-exp b))))) ((num-exp 5)))
      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code env))
      ((equal? (car parsed-code) 'print-exp)
       (run-print-exp (cadr parsed-code) env))
      ;(assign-exp a (num-exp 8))
      ((equal? (car parsed-code) 'assign-exp)
       (run-assign-exp (elementAt parsed-code 1)
                       (run-neo-parsed-code (elementAt parsed-code 2) env)
                       env))
      ;(block-exp (assign-exp x (num-exp 8)) (print-exp (var-exp 8))
      ((equal? (car parsed-code) 'block-exp)
       ;(displayln parsed-code))
       (run-block-exp (cdr parsed-code) env))
      ;(while-exp (bool < i (num-exp 10))(block-exp (assign i (math + i 1))))
      ((equal? (car parsed-code) 'while-exp)
       (run-while-exp (cadr parsed-code) (caddr parsed-code) env))
      (else (run-neo-parsed-code
             (cadr parsed-code) ;function expression
             (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                                env
                                )
             )
         )            
      )
    ) 
  )


;run bool parsed code
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (let ((op (elementAt parsed-code 0))
           (num1 (run-neo-parsed-code (elementAt parsed-code 1) env))
           (num2 (run-neo-parsed-code (elementAt parsed-code 2) env)))
           (cond
             ((equal? op '>) (> num1 num2))
             ((equal? op '<) (< num1 num2))
             ((equal? op '>=) (>= num1 num2))
             ((equal? op '<=) (<= num1 num2))
             ((equal? op '==) (= num1 num2))
             ((equal? op '!=) (not (= num1 num2)))
             ((equal? op '&&) (and num1 num2))
             ((equal? op '||) (or num1 num2))
             (else (not num1))
        )
      )
    )
  )

(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2)) ;+ 1 1)
      ((equal? op '-) (- num1 num2))  ;- 1 1
      ((equal? op '*) (* num1 num2)) ;* 1 1
      ((equal? op '/) (/ num1 num2))  ;/ 1 1 float division
      ((equal? op '//) (quotient num1 num2))  ;// 1 1 interger division
      ((equal? op '%) (modulo num1 num2))  ;% 1 1 modulo
      (else #false)
      )
    )
  )

(define run-let-exp
  (lambda (parsed-code env)
    ;((a (num-exp 7)) (b (var-exp a)) (x (var-exp b))) > ((a 7) (b 7) (x 7))
    ;(a (num-exp 7)) -> (a 7) < (list (car code) (run-neo-parsed-code (cadr code) env))) 
    (let* ((new_env (cascade-update-env (elementAt parsed-code 1) env))
          (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env)
    )
  )
)

;(display (string-append "**screen** " (number->string a)))
(define run-print-exp
  (lambda (parsed-code env)
    ;(display (run-neo-parsed-code parsed-code env))
    (displayln (string-append "**screen** " (number->string
                                           (run-neo-parsed-code parsed-code env))))
    )
  )

;(assign-exp x (num-exp 8))
;add (x 8) into the local variable scope
;it would return a new environment, but we do not want to run this expression alone
(define run-assign-exp
  (lambda (varname value env)
    (cond
      ((null? env) #false)
      ((equal? (caar env) 'global)
       (cons (list (list varname value)) env)); (((varname value)) (global (a 1) ...))
      (else (let*
          ;(((x 8) (y 9)) (global (a 1) (b 2) (c 5)) <- (z 10)
          ;new-local-scope: ((z 10) (x 8) (y 9))
          ((new-local-scope (cons (list varname value) (car env)))
           (under-env (cdr env)))
        (cons new-local-scope under-env))
      )
      )
    )
  )

;(block (assign x 8) (print x) (assign y 10) (assign z 12) (print (math + y z)))
;(block-exp (assign-exp x (num-exp 8)) (print-exp (var-exp x))
;

(define run-block-exp
  (lambda (parsed-list-exp env)
    (cond
      ((null? parsed-list-exp) '())
      ((equal? (caar parsed-list-exp) 'assign-exp)
     (run-block-exp
(cdr parsed-list-exp)
(run-assign-exp(cadr (car parsed-list-exp))
(run-neo-parsed-code (elementAt (car parsed-list-exp) 2) env) 
env)))
(else
(let ((return (run-neo-parsed-code (car parsed-list-exp) env)))
(if (void? return) (run-block-exp (cdr parsed-list-exp) env)
			(cons return (run-block-exp (cdr parsed-list-exp) env))
			)
		)
	)
	)
)
)

(define run-while-exp
  (lambda (bool_exp block_exp env)
    (let* ((new_block_exp (append block_exp (list (list 'while-exp bool_exp block_exp)))))
      (if (run-neo-parsed-code bool_exp env)
          (run-neo-parsed-code new_block_exp env)
          '()
          )
     )
    )
  )
    

(define cascade-update-env
  (lambda (parsed-scope env)
    (if (null? parsed-scope) env
        (let* (
               ;1. what is the local scope: (((a 7)) (global (a 1) (b 2) (c 5)))
               ;1.1 there is only one global scope there, so local scope should be '()
               ;1.2 there is a scope on top of global scope, that is the local scope
               (local-scope (if (equal? (car (car env)) 'global)
                                '()
                                (car env)))
               ;2. the global scope
               (global-scope-env (pop_env_to_global_scope env));((global (a 1)...)
               ;3. update the local scope
               (first-name-value-pair (car parsed-scope));((a 7)...)
               (new-local-scope (cons
                                 (list
                                  (car first-name-value-pair)
                                  (run-neo-parsed-code (cadr first-name-value-pair) env))
                                 local-scope))
               ;4. concate updated local scope on top of global scope to form the new environment
               (new_env (cons new-local-scope global-scope-env))
               )
          (cascade-update-env (cdr parsed-scope) new_env)
          )
      )
    )
  )

(provide (all-defined-out))