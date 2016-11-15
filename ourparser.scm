(load "compiler.scm")

(define test "\"\x0000003bb; is the good sign\"")

(test-string <String> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
