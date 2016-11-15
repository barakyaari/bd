(load "compiler.scm")

(define test "#\\tab")

(test-string <Char> test)


(display "\n----  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
