(load "compiler.scm")
(display "Test starting:\n")
(define test "##(2)")
(test-string <sexpr> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)

