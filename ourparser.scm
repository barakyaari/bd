(load "compiler.scm")

(define test "  my change")

(test-string <sexpr> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
