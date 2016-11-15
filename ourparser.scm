(load "compiler.scm")

(define test "-0008/00012")

(test-string <Number> test)


(display "\n----  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
