(load "parser.so")
(load "compiler.scm")

(display "Test starting:\n")

(define test "abc")

(test-string <sexpr1> test)


(display "\n- ---  Meirs parser: ----\n\n")

(test-string <sexpr> test)

