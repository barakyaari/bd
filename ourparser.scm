(load "compiler.scm")
(display "Test starting:\n")
(define test "##1+2")
(test-string <InfixSymbol> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)

