(load "parser.so")
(load "compiler.scm")

(display "Test starting...\n")
(display "Expression tested:\n")





;## 2 + #; 3 - 4 + 5 * 6 ^ 7 8
;## 2 + #; 3 - 4 + 5 * 6 ^ 7 8 #; 1+2^3
;## #; 2+5 -b+7
;## ( #; 1+2 1+3)

(define test  "## FUNC(a#;1+2,b)")
(display test)
(display "\n")
(test-string <sexpr> test)

(display "\n- ---  Meir's parser: ----\n\n")

(test-string <sexpr1> test)






