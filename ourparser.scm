(load "parser.so")
(load "compiler.scm")

(display "Test starting...\n")
(display "Expression tested:\n")

(define test "##4-----5")

(display test)
(display "\n")
(test-string <Sexpr> test)

(display "\n- ---  Meir's parser: ----\n\n")

(test-string <sexpr> test)






