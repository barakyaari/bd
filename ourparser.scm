(load "parser.so")
(load "compiler.scm")

(display "Test starting:\n")

;(define test "0123456789abcdeABCDE!$^*-_=+<>?/")
(define test "3!")

(test-string <sexpr1> test)


(display "\n- ---  Meirs parser: ----\n\n")

(test-string <sexpr> test)

