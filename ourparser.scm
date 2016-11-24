(load "parser.so")
(load "compiler.scm")

(display "Test starting...\n")
(display "Expression tested:\n")

(define test  "##f(g(x()))")

(display test)
(display "\n")
(test-string <sexpr> test)

(display "\n- ---  Meir's parser: ----\n\n")

(test-string <sexpr1> test)






