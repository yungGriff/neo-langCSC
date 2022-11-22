#lang Racket

(require "parser.rkt")
(require "runner.rkt")
(require "utility.rkt")


(define env '((global (a 1) (b 2) (c 5))))

;(define sample-code3 '(call (function (r) (local-vars ((p c)) (math / r p)) ) (a)))
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))
;(define sample-code2 '(call (function (a) (call (function (r) a) (a))) (5)))
;1
;(define sample-code '(print a))


(define sample-code '(block (print a) (assign x 8) (assign y (math * x 2)) (print y) (assign z (math + b y)) (print z)))
;(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)