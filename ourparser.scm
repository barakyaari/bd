(load "compiler.scm")
(display "Test starting:\n")
(define test "##1**2**3**4")
(test-string <sexpr> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)

