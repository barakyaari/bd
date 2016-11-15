(load "compiler.scm")

(define test "\"hello\"")

(test-string <String> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
