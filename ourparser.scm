(load "parser.so")
(load "compiler.scm")

(display "Test starting...\n")
(display "Expression tested:\n")

;(define test "0123456789abcdeABCDE!$^*-_=+<>?/")
;##-b[i][j][i+j]

;## 8 ^ (7+8)[5][6]
;## 2 + #; 3 - 4 + 5 * 6 ^ 7 8
;#%A[1]+A[2]*A[3]^B[4][5][6]
;## 2 + #; 3 - 4 + 5 * 6 ^ 7 8 #; 1+2^3
;## #; 2+5 -b+7
;## ( #; 1+2 1+3)

(define test  "##b[i][j]")
(display test)
(display "\n")
(test-string <sexpr> test)

(display "\n- ---  Meir's parser: ----\n\n")

(test-string <sexpr1> test)






