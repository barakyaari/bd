(load "compiler.scm")

(define test "#\\x00000aB")

(test-string <Char> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
