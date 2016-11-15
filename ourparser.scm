(load "compiler.scm")

(define test "##")

(test-string <InfixPrefixExtensionPrefix> test)


(display "\n- ---  Meirs parser: ----\n\n")
(load "parser.so")

(test-string <sexpr> test)
