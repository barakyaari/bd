(load "compiler.scm")
(display "Test starting:\n")

(define test "##-5+4")

(test-string <sexpr1> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)

