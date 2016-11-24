(load "parser.so")

(display "Test starting...\n")
(display "Expression tested:\n")

(define test  "##f(x)[a]")

(display test)
(display "\n")
(display "\n- ---  Meir's parser: ----\n\n")

(test-string <sexpr> test)


(load "compiler.scm")
(display "\n- ---  Our parser: ----\n\n")


(test-string <sexpr1> test)






