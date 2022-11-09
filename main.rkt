#lang Racket

(require "parser.rkt")
(require "runner.rkt")
(require "utility.rkt")


(define env '((global (a 1) (b 2) (c 5))))


(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))
(define sample-code2 '(call (function (a) (call (function (r) a) (a))) (5)))

(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code2))
(run-neo-parsed-code parsed-neo-code env)