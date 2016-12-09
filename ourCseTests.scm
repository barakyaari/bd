(load "cse.so")
(load "cse.scm")

(display "Test starting...\n")
(display "Expression tested:\n")
(display "\n")

(define test '((a)))

(display test)
(newline)
(newline)
(cse2 test)

(display "\n- ---  Meir's parser: ----\n\n")

(cse test)



(load "cse.scm")






(display "\n- ---  Our cse: ----\n\n")


(cse2 '((b)))
(newline)
(cse2 '((b) (a)))
(newline)
(cse2 '((b) (b)))
(newline)
(cse2 '(+ (* (- x y) (* x x))
(* x x)
(foo (- x y))
(goo (* (- x y) (* x x)))))
(newline)
(cse2 '(f (g x)
(g (g x))
(h (g (g x)) (g x))
((g x) (g x))))
(newline)
(cse2 '(list '(a b)
(list '(a b) '(c d))
(list '(a b) '(c d))))
(newline)
(cse2 '(list (cons 'a 'b)
(cons 'a 'b)
(list (cons 'a 'b)
(cons 'a 'b))
(list (list (cons 'a 'b)
(cons 'a 'b)))))
(newline)
(cse2 '(list '(a b) (a b) (a b)))
(newline)
(newline)


(display "\n- ---  Meirs cse: ----\n\n")

(load "cse.so")


	
(cse '((b)))
(newline)
(cse2 '((b) (a)))
(newline)
(cse '((b) (b)))
(newline)
(cse '(+ (* (- x y) (* x x))
(* x x)
(foo (- x y))
(goo (* (- x y) (* x x)))))
(newline)
(cse '(f (g x)
(g (g x))
(h (g (g x)) (g x))
((g x) (g x))))
(newline)
(cse '(list '(a b)
(list '(a b) '(c d))
(list '(a b) '(c d))))
(newline)
(cse '(list (cons 'a 'b)
(cons 'a 'b)
(list (cons 'a 'b)
(cons 'a 'b))
(list (list (cons 'a 'b)
(cons 'a 'b)))))
(newline)
(cse '(list '(a b) (a b) (a b)))
(newline)



