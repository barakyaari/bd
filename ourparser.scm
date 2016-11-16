(load "compiler.scm")

(define test "  Hello world")
;finished my change!
(test-string <sexpr> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
