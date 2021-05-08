#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "SMOG.rkt")

(define (check-SMOG code value)
  (check-equal? (with-output-to-string (λ () (run code))) value))

(define SMOG-tests
  (test-suite
   "SMOG tests"
   (check-SMOG '("print(\"hello world\")") "hello world") ;; output
   (check-SMOG '("if 0:" ;; if statement
                 " print(1)"
                 "elseif 1:"
                 " print(2)"
                 "elseif 1:"
                 " print(3)"
                 "else:"
                 " print(4)"
                 "endif")
               "2")
   (check-SMOG '("if 0:" ;; goto
                 " print(1)"
                 "else:"
                 " goto 1"
                 "endif")
               "1")
   (check-SMOG '("let x = 1 + 1" ;; variables
                 "print($x$)")
               "2")
   (check-SMOG '("let x = \"hello world\"" ;; delayed variables
                 "print(!x!)")
               "hello world")
   (check-SMOG '("print(1 + 2 == 3 % 2)") ;; operator precedence (or lack thereof)
               "1")))
 
(define level-tests
  (test-suite
   "Level tests"))

(run-tests SMOG-tests)