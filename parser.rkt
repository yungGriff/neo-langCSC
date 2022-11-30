#lang racket
(require "utility.rkt")

(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
      ((equal? (car neo-code) 'bool) (neo-bool-code-parser neo-code))
      ((equal? (car neo-code) 'math) (neo-math-code-parser neo-code))
      ((equal? (car neo-code) 'call) (neo-call-code-parser neo-code))
      ((equal? (car neo-code) 'local-vars) (neo-let-code-parser neo-code))
      ((equal? (car neo-code) 'print) (list 'print-exp (neo-parser (cadr neo-code))))
      ((equal? (car neo-code) 'assign)
       (list 'assign-exp (cadr neo-code) (neo-parser (caddr neo-code))))
     ((equal? (car neo-code) 'block)
       (cons 'block-exp (neo-parser (cdr neo-code))))
      ((equal? (car neo-code) 'while)
       (list 'while-exp (neo-parser (cadr neo-code)) (neo-parser (caddr neo-code))))
      (else (map neo-parser neo-code)) 
      )
    )
  )
    


(define neo-bool-code-parser
  (lambda (neo-code)
     (if (equal? (length neo-code) 3)
            (list 'bool-exp (elementAt neo-code 1) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code)))))     
    )
  )

(define neo-math-code-parser
  (lambda (neo-code)
    (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code)))
    )
  )

(define neo-function-code-parser
  (lambda (neo-code)
    (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code))))
    )
  )

(define neo-ask-code-parser
  (lambda (neo-code)
    (cons 'ask-exp
             (map neo-parser (cdr neo-code)))
    )
  )

(define neo-call-code-parser
  (lambda(neo-code)
    (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code)))
    )
  )

;(local-vars ((a 1) (b 2) (c a)) (neo-exp)) = neo-code
;(let-exp ((a 1) (b 2) (c 3)) (parsed-neo-exp))
;(let-exp ((a (num-exp 1)) (b (num-exp 2)) (c (var-exp a))) (parsed-neo-code))
;1 -> (num-exp 1) = code -> (neo-parser code)
;(a 1) -> (a (num-exp 1)) -> code == (a 1) < (list (car code) (neo-parser (cadr code)))
;((a 1) (b 2) (c a)) -> ((a (num-exp 1)) (b (num-exp 2)) (c (var-exp a)))
;((map (lambda (pair) (list (car pair) (neo-parser (cadr pair))) lst)
(define neo-let-code-parser
  (lambda (neo-code)
    (list 'let-exp
          (map (lambda (pair) (list (car pair) (neo-parser (elementAt pair 1))))
               (elementAt neo-code 1))
           (neo-parser (elementAt neo-code 2)))
    )
  )

(provide (all-defined-out))