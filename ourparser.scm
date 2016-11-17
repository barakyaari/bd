(load "compiler.scm")

(define test "abc d ee")

(test-string <sexpr> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")


(test-string <sexpr> test)
